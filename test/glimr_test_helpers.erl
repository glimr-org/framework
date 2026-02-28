-module(glimr_test_helpers).
-export([panic_to_result/1]).

panic_to_result(F) ->
    try
        F(),
        {ok, nil}
    catch
        error:{gleam_error, {panic, _Msg}, _Stacktrace} ->
            {error, <<"panicked">>};
        error:_ ->
            {error, <<"panicked">>}
    end.
