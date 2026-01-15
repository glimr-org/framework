-module(glimr_kernel_ffi).
-export([store_commands/1, get_stored_commands/0]).

store_commands(Commands) ->
    erlang:put(glimr_commands, Commands),
    nil.

get_stored_commands() ->
    case erlang:get(glimr_commands) of
        undefined -> [];
        Commands -> Commands
    end.
