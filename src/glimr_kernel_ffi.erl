-module(glimr_kernel_ffi).
-export([store_commands/1, get_stored_commands/0]).
-export([cache_db_config/1, get_cached_db_config/0, clear_db_config/0]).
-export([cache_route_groups/1, get_cached_route_groups/0]).
-export([cache_cache_config/1, get_cached_cache_config/0]).

store_commands(Commands) ->
    erlang:put(glimr_commands, Commands),
    nil.

get_stored_commands() ->
    case erlang:get(glimr_commands) of
        undefined -> [];
        Commands -> Commands
    end.

cache_db_config(Connections) ->
    persistent_term:put(glimr_db_config, Connections),
    nil.

get_cached_db_config() ->
    try persistent_term:get(glimr_db_config) of
        Connections -> {ok, Connections}
    catch
        error:badarg -> {error, nil}
    end.

clear_db_config() ->
    persistent_term:erase(glimr_db_config),
    nil.

cache_route_groups(Groups) ->
    persistent_term:put(glimr_route_groups, Groups),
    nil.

get_cached_route_groups() ->
    try persistent_term:get(glimr_route_groups) of
        Groups -> {ok, Groups}
    catch
        error:badarg -> {error, nil}
    end.

cache_cache_config(Stores) ->
    persistent_term:put(glimr_cache_config, Stores),
    nil.

get_cached_cache_config() ->
    try persistent_term:get(glimr_cache_config) of
        Stores -> {ok, Stores}
    catch
        error:badarg -> {error, nil}
    end.
