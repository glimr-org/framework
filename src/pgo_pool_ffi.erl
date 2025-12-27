%% FFI helper for pgo pool checkout/checkin
-module(pgo_pool_ffi).

-export([checkout/1, checkin/2, stop_pool/1, identity/1]).

%% Checkout a connection from the pool.
%% Returns {ok, {Ref, {single_connection, Conn}}} on success,
%% or {error, Reason} on failure.
checkout(Name) when is_atom(Name) ->
    case pgo:checkout(Name) of
        {ok, Ref, Conn} ->
            %% Wrap conn as pog's SingleConnection type
            SingleConn = {single_connection, Conn},
            {ok, {Ref, SingleConn}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Checkin a connection back to the pool.
%% Unwraps the SingleConnection tuple before passing to pgo.
checkin(Ref, {single_connection, Conn}) ->
    pgo:checkin(Ref, Conn);
checkin(Ref, Conn) ->
    %% Fallback if already unwrapped
    pgo:checkin(Ref, Conn).

%% Stop a pool by name.
%% Terminates the pool process via the pgo_sup supervisor.
stop_pool(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            {error, not_found};
        Pid ->
            supervisor:terminate_child(pgo_sup, Pid)
    end.

%% Identity function for type coercion
identity(X) -> X.
