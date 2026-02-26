-module(glimr_fail_ffi).
-export([raise/1, rescue/1]).

raise(Status) ->
    erlang:error({glimr_fail, Status}).

rescue(Fun) ->
    try Fun() of
        Result -> {ok, Result}
    catch
        error:{glimr_fail, Status} -> {fail, Status};
        Class:Reason:Stacktrace ->
            erlang:raise(Class, Reason, Stacktrace)
    end.
