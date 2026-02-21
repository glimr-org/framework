-module(glimr_app_config_test_ffi).
-export([clear_app_config/0]).

clear_app_config() ->
    try persistent_term:erase(glimr_app_config) of
        _ -> nil
    catch
        error:badarg -> nil
    end.
