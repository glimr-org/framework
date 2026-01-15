-module(glimr_port_ffi).
-export([start_gleam_run/0, stop_port/1, start_output_reader/1]).

start_gleam_run() ->
    % Start gleam in background, wait for stdin EOF (port close), then kill process group
    % FORCE_COLOR=1 ensures colored output even when not connected to TTY
    Cmd = "FORCE_COLOR=1 gleam run & CHILD=$!; cat >/dev/null; kill -TERM -$$ 2>/dev/null; wait $CHILD",
    open_port({spawn_executable, "/bin/sh"},
              [{args, ["-c", Cmd]},
               binary, stream, exit_status, use_stdio, stderr_to_stdout,
               {line, 1024}]).

stop_port(Port) ->
    case erlang:port_info(Port, os_pid) of
        {os_pid, Pid} ->
            % Kill entire process group (negative PID), which includes gleam and beam.smp
            os:cmd("kill -TERM -" ++ integer_to_list(Pid) ++ " 2>/dev/null"),
            timer:sleep(300),
            % Follow up with SIGKILL if still alive
            os:cmd("kill -9 -" ++ integer_to_list(Pid) ++ " 2>/dev/null"),
            timer:sleep(200);
        undefined ->
            ok
    end,
    nil.

start_output_reader(Port) ->
    Pid = spawn(fun() -> output_reader_loop(Port) end),
    erlang:port_connect(Port, Pid),
    nil.

output_reader_loop(Port) ->
    receive
        {Port, {data, {_, Line}}} ->
            case binary:match(Line, <<"SIGTERM received">>) of
                nomatch -> io:put_chars([Line, $\n]);
                _ -> ok
            end,
            output_reader_loop(Port);
        _ ->
            ok
    end.
