-module (server).
-export ([start/1, server/1, stop/1]).

start(Port) -> register(server, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.




