-module (server).
-export ([start/1, server/1, stop/1]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            ok;
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


