-module (server).
-export ([start/1, server/1, stop/1]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            io:fwrite("Server listening! Waiting for connections...\n"),
            accept(LSock);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


accept(LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, ClientSocket} ->
            io:fwrite("Client connected: ~p~n", [ClientSocket]),
            % IMPORTANTE: Aqui deverias iniciar um novo processo para lidar com este cliente!
            % Exemplo muito simples: spawn(fun() -> handle_client(ClientSocket) end),
            gen_tcp:send(ClientSocket, "Welcome to the server!\n"), % Envia mensagem ao cliente (com newline!)
            % Chamada recursiva para aceitar a próxima conexão
            accept(LSocket);
        {error, Reason} ->
            io:fwrite("Error accepting connection: ~p~n", [Reason])
            % Talvez parar o servidor ou tentar novamente dependendo do erro
    end.


