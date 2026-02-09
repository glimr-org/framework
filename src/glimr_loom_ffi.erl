-module(glimr_loom_ffi).
-export([safe_apply/3]).

%% Safely applies a function on a module, catching any errors.
%% Returns {ok, Result} or {error, Reason}.
%%
safe_apply(Module, Function, Args) ->
    try
        Result = apply(Module, Function, Args),
        {ok, Result}
    catch
        error:undef ->
            {error, {undef, Module, Function}};
        error:Reason ->
            {error, Reason};
        throw:Reason ->
            {error, Reason};
        exit:Reason ->
            {error, Reason}
    end.
