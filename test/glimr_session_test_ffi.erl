-module(glimr_session_test_ffi).
-export([clear_session_config/0, clear_session_store/0, clear_cache_config/0]).

clear_session_config() ->
    try persistent_term:erase(glimr_session_config) of
        _ -> nil
    catch
        error:badarg -> nil
    end.

clear_session_store() ->
    try persistent_term:erase(glimr_session_store) of
        _ -> nil
    catch
        error:badarg -> nil
    end.

clear_cache_config() ->
    try persistent_term:erase(glimr_cache_config) of
        _ -> nil
    catch
        error:badarg -> nil
    end.
