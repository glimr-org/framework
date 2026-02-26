%% FFI for dynamic pool dispatch.
%% Console commands need to start adapter pools at runtime
%% without compile-time imports. This module converts a module
%% name string to an atom and calls try_start_from_config/1
%% on it, bridging Gleam's module system with Erlang's
%% dynamic dispatch.
-module(glimr_command_ffi).

-export([dynamic_start_pool/2, dynamic_start_cache/2, suppress_pool_shutdown_reports/0]).

dynamic_start_pool(ModuleBin, Config) ->
    Module = binary_to_atom(ModuleBin, utf8),
    Module:try_start_from_config(Config).

dynamic_start_cache(ModuleBin, Store) ->
    Module = binary_to_atom(ModuleBin, utf8),
    Module:try_start_cache(Store).

suppress_pool_shutdown_reports() ->
    logger:add_primary_filter(suppress_pgo_shutdown, {fun(LogEvent, _) ->
        case LogEvent of
            #{msg := {report, #{report := Report}}} when is_list(Report) ->
                case lists:keyfind(supervisor, 1, Report) of
                    {supervisor, {_, pgo_pool_sup}} -> stop;
                    _ -> ignore
                end;
            _ -> ignore
        end
    end, []}),
    nil.
