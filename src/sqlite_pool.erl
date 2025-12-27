%% SQLite connection pool with crash-safe connection management.
%% Ported from pgo_pool.erl (https://github.com/erleans/pgo)
%% Uses ETS heir mechanism for automatic connection reclamation on process death.
-module(sqlite_pool).

-export([start_link/2,
         checkout/2,
         checkin/2,
         disconnect/2,
         stop/1]).

-export([init/1,
         handle_call/3,
         handle_info/2,
         terminate/2]).

-define(TIMEOUT, 5000).
-define(QUEUE, true).
-define(QUEUE_TARGET, 50).
-define(QUEUE_INTERVAL, 1000).
-define(IDLE_INTERVAL, 1000).
-define(TIME_UNIT, 1000).
-define(HOLDER_KEY, '__info__').

-type ref() :: {Pool :: pid(),
                Ref :: reference(),
                TimerRef :: reference() | undefined,
                Holder :: ets:tid()}.

%% Start a new SQLite connection pool
%% Path: path to SQLite database file
%% PoolConfig: map with pool_size, queue_target, queue_interval, idle_interval
start_link(Path, PoolConfig) ->
    PoolConfig1 = normalize_pool_config(PoolConfig),
    gen_server:start_link(?MODULE, {Path, PoolConfig1}, []).

-spec checkout(pid(), list()) -> {ok, ref(), any()} | {error, any()}.
checkout(Pool, Opts) ->
    MaybeQueue = proplists:get_value(queue, Opts, ?QUEUE),
    Now = erlang:monotonic_time(?TIME_UNIT),
    Timeout = abs_timeout(Now, Opts),
    case do_checkout(Pool, MaybeQueue, Now, Timeout) of
        {ok, Ref, Conn} ->
           {ok, {Ref, Conn}};
        Error={error, _} ->
            Error;
        {exit, Reason} ->
            exit({Reason, {?MODULE, checkout, [Pool, Opts]}})
    end.

checkin({Pool, Ref, Deadline, Holder}, Conn) ->
    cancel_deadline(Deadline),
    Now = erlang:monotonic_time(?TIME_UNIT),
    checkin_holder(Holder, Pool, Conn, {checkin, Ref, Now}).

disconnect({Pool, _Ref, Deadline, Holder}, Err) ->
    cancel_deadline(Deadline),
    ets:delete(Holder),
    gen_server:cast(Pool, {disconnected, Err}).

stop(Pool) ->
    gen_server:stop(Pool).

%% gen_server callbacks

init({Path, PoolConfig}) ->
    process_flag(trap_exit, true),
    PoolSize = maps:get(pool_size, PoolConfig, 10),

    %% Create initial connections
    case create_connections(Path, PoolSize, []) of
        {ok, Connections} ->
            QueueTid = ets:new(?MODULE, [protected, ordered_set]),
            Target = maps:get(queue_target, PoolConfig, ?QUEUE_TARGET),
            Interval = maps:get(queue_interval, PoolConfig, ?QUEUE_INTERVAL),
            IdleInterval = maps:get(idle_interval, PoolConfig, ?IDLE_INTERVAL),
            Now = erlang:monotonic_time(?TIME_UNIT),
            Codel = #{target => Target, interval => Interval, delay => 0, slow => false,
                      next => Now, poll => undefined, idle_interval => IdleInterval, idle => undefined},
            Codel1 = start_idle(Now, Now, start_poll(Now, Now, Codel)),

            %% Create holders for each connection and add to ready queue
            State = {ready, QueueTid, Codel1, Path},
            lists:foreach(fun(Conn) ->
                Holder = create_holder(self(), Conn, QueueTid),
                ets:insert(QueueTid, {{Now, Holder}})
            end, Connections),

            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(tid, _From, {_, Queue, _, _} = D) ->
    {reply, Queue, D}.

handle_info({db_connection, From, {checkout, Now, MaybeQueue}}, Busy={busy, Queue, _, _}) ->
    case MaybeQueue of
      true ->
        ets:insert(Queue, {{Now, erlang:unique_integer(), From}}),
        {noreply, Busy};
      false ->
        gen_server:reply(From, {error, none_available_no_queuing}),
        {noreply, Busy}
  end;
handle_info(Checkout={db_connection, From, {checkout, _Now, _MaybeQueue}}, Ready) ->
    {ready, Queue, Codel, Path} = Ready,
    case ets:first(Queue) of
        Key={_Time, Holder} ->
            checkout_holder(Holder, From, Queue) andalso ets:delete(Queue, Key),
            {noreply, Ready};
        '$end_of_table' ->
            handle_info(Checkout, {busy, Queue, Codel, Path})
    end;

%% Client died without checkin - ETS heir mechanism kicks in
%% For SQLite, we can safely reuse the connection (no transaction state issues)
handle_info({'ETS-TRANSFER', Holder, _Pid, Queue}, {State, Queue, Codel, Path} = _Data) ->
    case ets:lookup(Holder, ?HOLDER_KEY) of
        [{_, Conn, Deadline, Pool}] ->
            %% Cancel any deadline timer
            cancel_deadline(Deadline),
            %% Reset the holder and put it back in the queue
            true = ets:update_element(Holder, ?HOLDER_KEY, [{3, undefined}]),
            %% Reset heir to pool with queue data
            ets:setopts(Holder, {heir, Pool, Queue}),
            Now = erlang:monotonic_time(?TIME_UNIT),
            ets:insert(Queue, {{Now, Holder}}),
            %% Verify connection is still valid
            case sqlight_ffi:exec(<<"SELECT 1">>, Conn) of
                {ok, _} ->
                    {noreply, {State, Queue, Codel, Path}};
                {error, _} ->
                    %% Connection is bad, close and create new
                    sqlight_ffi:close(Conn),
                    ets:delete(Queue, {Now, Holder}),
                    ets:delete(Holder),
                    case sqlight_ffi:open(Path) of
                        {ok, NewConn} ->
                            NewHolder = create_holder(self(), NewConn, Queue),
                            ets:insert(Queue, {{Now, NewHolder}}),
                            {noreply, {State, Queue, Codel, Path}};
                        {error, _} ->
                            {noreply, {State, Queue, Codel, Path}}
                    end
            end;
        _ ->
            ets:delete(Holder),
            {noreply, {State, Queue, Codel, Path}}
    end;

handle_info({'ETS-TRANSFER', Holder, _, {Msg, Queue, Extra}}, {_, Queue, _, _} = Data) ->
    case Msg of
        checkin ->
            handle_checkin(Holder, Extra, Data);
        disconnect ->
            disconnect_holder(Holder, Extra),
            {noreply, Data}
    end;

handle_info({timeout, Deadline, {Queue, Holder, _Pid, _Len}}, {_, Queue, _, _} = Data) ->
    %% Check that timeout refers to current holder (and not previous)
    case ets:lookup_element(Holder, ?HOLDER_KEY, 3) of
        Deadline ->
            ets:update_element(Holder, ?HOLDER_KEY, {3, undefined}),
            disconnect_holder(Holder, {error, client_timeout});
        _ ->
            ok
    end,
    {noreply, Data};

handle_info({timeout, Poll, {Time, LastSent}}, {_, _, #{poll := Poll}, _} = Data) ->
    {Status, Queue, Codel, Path} = Data,
    case ets:first(Queue) of
        {Sent, _, _} when Sent =< LastSent andalso Status == busy ->
            Delay = Time - Sent,
            timeout(Delay, Time, Queue, start_poll(Time, Sent, Codel), Path);
        {Sent, _, _} ->
            {noreply, {Status, Queue, start_poll(Time, Sent, Codel), Path}};
        _ ->
            {noreply, {Status, Queue, start_poll(Time, Time, Codel), Path}}
    end;

handle_info({timeout, Idle, {Time, LastSent}}, {_, Queue, #{idle := Idle}, _} = Data) ->
    {Status, Queue, Codel, Path} = Data,
    case ets:first(Queue) of
        {Sent, Holder} = Key when Sent =< LastSent andalso Status == ready ->
            ets:delete(Queue, Key),
            ping(Holder, Queue, start_idle(Time, LastSent, Codel), Path);
        {Sent, _} ->
            {noreply, {Status, Queue, start_idle(Time, Sent, Codel), Path}};
        _ ->
            {noreply, {Status, Queue, start_idle(Time, Time, Codel), Path}}
    end;

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, {_, Queue, _, _}) ->
    %% Close all connections in the queue
    close_all_connections(Queue),
    ok;
terminate(_Reason, _State) ->
    ok.

%% Internal functions

normalize_pool_config(PoolConfig) when is_list(PoolConfig) ->
    normalize_pool_config(maps:from_list(PoolConfig));
normalize_pool_config(PoolConfig) when is_map(PoolConfig) ->
    PoolConfig;
normalize_pool_config(_) ->
    #{}.

create_connections(_Path, 0, Acc) ->
    {ok, Acc};
create_connections(Path, Count, Acc) ->
    case sqlight_ffi:open(Path) of
        {ok, Conn} ->
            create_connections(Path, Count - 1, [Conn | Acc]);
        {error, Reason} ->
            %% Close any connections we already opened
            lists:foreach(fun(C) -> sqlight_ffi:close(C) end, Acc),
            {error, Reason}
    end.

close_all_connections(Queue) ->
    case ets:first(Queue) of
        '$end_of_table' ->
            ok;
        {_, Holder} = Key ->
            case ets:lookup(Holder, ?HOLDER_KEY) of
                [{_, Conn, _, _}] ->
                    sqlight_ffi:close(Conn);
                _ ->
                    ok
            end,
            ets:delete(Holder),
            ets:delete(Queue, Key),
            close_all_connections(Queue)
    end.

create_holder(Pool, Conn, Queue) ->
    Holder = ets:new(?MODULE, [public, ordered_set]),
    true = ets:insert_new(Holder, {?HOLDER_KEY, Conn, undefined, Pool}),
    ets:setopts(Holder, {heir, Pool, Queue}),
    Holder.

timeout(Delay, Time, Queue, Codel, Path) ->
    case Codel of
        #{delay := MinDelay, next := Next, target := Target, interval := Interval}
          when Time >= Next andalso MinDelay > Target ->
            Codel1 = Codel#{slow := true, delay := Delay, next := Time + Interval},
            drop_slow(Time, Target * 2, Queue),
            {noreply, {busy, Queue, Codel1, Path}};
        #{next := Next, interval := Interval} when Time >= Next ->
            Codel1 = Codel#{slow := false, delay := Delay, next := Time + Interval},
            {noreply, {busy, Queue, Codel1, Path}};
        _ ->
            {noreply, {busy, Queue, Codel, Path}}
    end.

drop_slow(Time, Timeout, Queue) ->
    MinSent = Time - Timeout,
    Match = {{'$1', '_', '$2'}},
    Guards = [{'<', '$1', MinSent}],
    SelectSlow = [{Match, Guards, [{{'$1', '$2'}}]}],
    [drop(Time - Sent, From) || {Sent, From} <- ets:select(Queue, SelectSlow)],
    ets:select_delete(Queue, [{Match, Guards, [true]}]).

ping(Holder, Queue, Codel, Path) ->
    %% For SQLite, we just verify the connection is still valid
    case ets:lookup(Holder, ?HOLDER_KEY) of
        [{_, Conn, _, _}] ->
            %% Try a simple query to check connection health
            case sqlight_ffi:exec(<<"SELECT 1">>, Conn) of
                {ok, _} ->
                    %% Connection is good, put it back
                    Now = erlang:monotonic_time(?TIME_UNIT),
                    ets:insert(Queue, {{Now, Holder}}),
                    {noreply, {ready, Queue, Codel, Path}};
                {error, _} ->
                    %% Connection is bad, close it and create a new one
                    sqlight_ffi:close(Conn),
                    ets:delete(Holder),
                    case sqlight_ffi:open(Path) of
                        {ok, NewConn} ->
                            NewHolder = create_holder(self(), NewConn, Queue),
                            Now = erlang:monotonic_time(?TIME_UNIT),
                            ets:insert(Queue, {{Now, NewHolder}}),
                            {noreply, {ready, Queue, Codel, Path}};
                        {error, _} ->
                            %% Failed to create new connection, continue without it
                            {noreply, {ready, Queue, Codel, Path}}
                    end
            end;
        _ ->
            ets:delete(Holder),
            {noreply, {ready, Queue, Codel, Path}}
    end.

handle_checkin(Holder, Now, {ready, Queue, _, _} = Data) ->
    ets:insert(Queue, {{Now, Holder}}),
    {noreply, Data};
handle_checkin(Holder, Now, {busy, Queue, Codel, Path}) ->
    dequeue(Now, Holder, Queue, Codel, Path).

dequeue(Time, Holder, Queue, Codel, Path) ->
    case Codel of
      #{next := Next, delay := Delay, target := Target} when Time >= Next  ->
        dequeue_first(Time, Delay > Target, Holder, Queue, Codel, Path);
      #{slow := false} ->
        dequeue_fast(Time, Holder, Queue, Codel, Path);
      #{slow := true, target := Target} ->
        dequeue_slow(Time, Target * 2, Holder, Queue, Codel, Path)
    end.

dequeue_first(Time, Slow, Holder, Queue, Codel, Path) ->
    #{interval := Interval} = Codel,
    Next = Time + Interval,
    case ets:first(Queue) of
        {Sent, _, From} = Key ->
            ets:delete(Queue, Key),
            Delay = Time - Sent,
            Codel1 =  Codel#{next => Next, delay => Delay, slow => Slow},
            go(Delay, From, Time, Holder, Queue, Codel1, Path);
        '$end_of_table' ->
            Codel1 = Codel#{next => Next, delay => 0, slow => Slow},
            ets:insert(Queue, {{Time, Holder}}),
            {noreply, {ready, Queue, Codel1, Path}}
    end.

dequeue_fast(Time, Holder, Queue, Codel, Path) ->
    case ets:first(Queue) of
        {Sent, _, From} = Key ->
            ets:delete(Queue, Key),
            go(Time - Sent, From, Time, Holder, Queue, Codel, Path);
        '$end_of_table' ->
            ets:insert(Queue, {{Time, Holder}}),
            {noreply, {ready, Queue, Codel#{delay => 0}, Path}}
    end.

dequeue_slow(Time, Timeout, Holder, Queue, Codel, Path) ->
    case ets:first(Queue) of
        {Sent, _, From} = Key when Time - Sent > Timeout ->
            ets:delete(Queue, Key),
            drop(Time - Sent, From),
            dequeue_slow(Time, Timeout, Holder, Queue, Codel, Path);
        {Sent, _, From} = Key ->
            ets:delete(Queue, Key),
            go(Time - Sent, From, Time, Holder, Queue, Codel, Path);
        '$end_of_table' ->
            ets:insert(Queue, {{Time, Holder}}),
            {noreply, {ready, Queue, Codel#{delay => 0}, Path}}
    end.

go(Delay, From, Time, Holder, Queue, Codel, Path) ->
    #{delay := Min} = Codel,
    case checkout_holder(Holder, From, Queue) of
        true when Delay < Min ->
            {noreply, {busy, Queue, Codel#{delay => Delay}, Path}};
        true ->
            {noreply, {busy, Queue, Codel, Path}};
        false ->
            dequeue(Time, Holder, Queue, Codel, Path)
    end.

drop(_Delay, From) ->
    gen_server:reply(From, {error, none_available}).

abs_timeout(Now, Opts) ->
    case proplists:get_value(timeout, Opts, ?TIMEOUT) of
        infinity ->
            proplists:get_value(deadline, Opts);
        Timeout ->
            min(Now + Timeout, proplists:get_value(deadline, Opts))
    end.

start_deadline(undefined, _, _, _, _) ->
    undefined;
start_deadline(Timeout, Pid, Ref, Holder, Start) ->
    Deadline = erlang:start_timer(Timeout, Pid, {Ref, Holder, self(), Start}, [{abs, true}]),
    ets:update_element(Holder, ?HOLDER_KEY, {3, Deadline}),
    Deadline.

cancel_deadline(undefined) ->
    ok;
cancel_deadline(Deadline) ->
    erlang:cancel_timer(Deadline, [{async, true}, {info, false}]).

start_poll(Now, LastSent, #{interval := Interval} = Codel) ->
    Timeout = Now + Interval,
    Poll = erlang:start_timer(Timeout, self(), {Timeout, LastSent}, [{abs, true}]),
    Codel#{poll => Poll}.

start_idle(Now, LastSent, Codel=#{idle_interval := Interval}) ->
    Timeout = Now + Interval,
    Idle = erlang:start_timer(Timeout, self(), {Timeout, LastSent}, [{abs, true}]),
    Codel#{idle => Idle}.

do_checkout(Pool, Queue, Start, Timeout) when is_pid(Pool) ->
    %% Pool is already a pid, use it directly
    case node(Pool) of
        Node when Node == node() ->
            checkout_call(Pool, Queue, Start, Timeout);
        Node ->
            {exit, {badnode, Node}}
    end;
do_checkout(Pool, Queue, Start, Timeout) when is_atom(Pool) ->
    %% Pool is a registered name, look it up
    case erlang:whereis(Pool) of
        Pid when is_pid(Pid) andalso node(Pid) == node() ->
            checkout_call(Pid, Queue, Start, Timeout);
        Pid when is_pid(Pid) andalso node(Pid) =/= node() ->
            {exit, {badnode, node(Pid)}};
        undefined ->
            {exit, noproc}
    end.

checkout_call(Pid, Queue, Start, Timeout) ->
    MRef = erlang:monitor(process, Pid),
    erlang:send(Pid, {db_connection, {self(), MRef}, {checkout, Start, Queue}}),
    receive
        {'ETS-TRANSFER', Holder, Owner, {MRef, Ref}} ->
            erlang:demonitor(MRef, [flush]),
            Deadline = start_deadline(Timeout, Owner, Ref, Holder, Start),
            PoolRef = {Owner, Ref, Deadline, Holder},
            checkout_info(Holder, PoolRef);
        {MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            {exit, Reason}
    end.

checkout_info(Holder, PoolRef) ->
    try ets:lookup(Holder, ?HOLDER_KEY) of
        [{_, Conn, _, _}] ->
            {ok, PoolRef, Conn}
    catch
      _:_ ->
        {error, deadline_reached}
    end.

checkout_holder(Holder, {Pid, MRef}, Ref) ->
    try
        ets:give_away(Holder, Pid, {MRef, Ref})
    catch
        error:badarg ->
            false
    end.

checkin_holder(Holder, Pool, Conn, Msg) ->
    try
        ets:update_element(Holder, ?HOLDER_KEY, [{3, undefined}, {2, Conn}]),
        ets:give_away(Holder, Pool, Msg),
        ok
    catch
        error:badarg ->
            ok
    end.

disconnect_holder(Holder, _Err) ->
    case ets:lookup(Holder, ?HOLDER_KEY) of
        [{_, Conn, Deadline, _}] ->
            cancel_deadline(Deadline),
            sqlight_ffi:close(Conn);
        _ ->
            ok
    end,
    ets:delete(Holder).
