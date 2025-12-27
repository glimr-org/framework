%% Supervisor for SQLite connection pools.
%% Uses simple_one_for_one strategy to dynamically manage pool children.
-module(sqlite_pool_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_pool/3, stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% Start the supervisor
start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% Start a new pool under the supervisor
%% Name: atom to register the pool under
%% Path: path to SQLite database file
%% Config: pool configuration map
start_pool(Name, Path, Config) ->
    %% Ensure supervisor is started
    ensure_started(),
    ChildSpec = #{
        id => Name,
        start => {sqlite_pool, start_link, [Name, Path, Config]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [sqlite_pool]
    },
    case supervisor:start_child(?SUPERVISOR, ChildSpec) of
        {ok, _Pid} -> {ok, Name};
        {error, {already_started, _Pid}} -> {ok, Name};
        {error, Reason} -> {error, Reason}
    end.

%% Stop a pool
stop_pool(Name) ->
    case supervisor:terminate_child(?SUPERVISOR, Name) of
        ok -> supervisor:delete_child(?SUPERVISOR, Name);
        Error -> Error
    end.

%% Supervisor callback
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    {ok, {SupFlags, []}}.

%% Internal functions

%% Ensure the supervisor is started
ensure_started() ->
    case whereis(?SUPERVISOR) of
        undefined ->
            %% Start as a standalone process (not ideal but works for now)
            %% In a full OTP app, this would be started by the application supervisor
            case start_link() of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                Error -> Error
            end;
        _Pid ->
            ok
    end.
