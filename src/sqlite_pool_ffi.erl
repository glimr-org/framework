%% FFI helper for SQLite pool configuration
-module(sqlite_pool_ffi).

-export([make_config/1]).

make_config(PoolSize) ->
    #{pool_size => PoolSize}.
