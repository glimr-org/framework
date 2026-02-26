-module(glimr_command_test_ffi).
-export([clear_cache_config/0]).

clear_cache_config() ->
    try persistent_term:erase(glimr_cache_config) of
        _ -> nil
    catch
        error:badarg -> nil
    end.
