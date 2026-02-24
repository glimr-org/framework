%% FFI for pool connection type coercion.
%% Gleam generics are erased at runtime (compiles to Erlang),
%% so an identity function safely coerces Dynamic back to typed
%% functions at call sites.
-module(glimr_pool_ffi).

-export([identity/1]).

identity(X) -> X.
