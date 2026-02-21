-module(glimr_auth_test_ffi).
-export([clear_auth_config/0]).

clear_auth_config() ->
    try persistent_term:erase(glimr_auth_config) of
        _ -> nil
    catch
        error:badarg -> nil
    end.
