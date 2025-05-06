-module (server).

-import(file_functions, [read_content/1]).
-import(user_functions, [accounts_manager/1]).

-export ([start/1, server/1, stop/1]).



start(Port) -> register(servidor, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            %register(match_manager, spawn(fun() -> lobby([]) end)),

            Filename = "teste",
            Accounts = file_functions:read_content(Filename),
            io:fwrite("Accounts: ~p~n", [Accounts]), %debug
            register(accountsProcess ,spawn(fun() -> accounts_manager(Accounts) end)),

            %register(file_process, spawn(fun() -> file_management() end)),
            acceptor(LSock);
        {error, Reason} ->
            io:fwrite("Error generating socket\n"),
            Reason
    end.


acceptor(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            spawn(fun() -> acceptor(LSock) end),
            comunicator(Socket);
        {error, Reason} ->
            io:fwrite("Error accepting connection: ~p~n", [Reason]),
            ok
    end.

%Receve as mensagens do cliente
comunicator(Socket) ->
    receive
        {tcp, _, Message} ->
            io:fwrite("Message received: ~p~n", [Message]), %debug
            Message1 = binary_to_list(Message),
            io:fwrite("Message1: ~p~n", [Message1]), %debug
            Message2 = string:trim(Message1), %remove os espaços em branco incluindo os \r e \n
            Message3 = string:tokens(Message2, ","), %separa a mensagem em tokens
        case Message3 of
            ["login_user", Username, Password] ->
                accountsProcess ! {login, Username, Password, self()},
                receive
                    success ->
                        io:fwrite("Login successful~n"),
                        gen_tcp:send(Socket, "Login successful\n");
                    invalid_user ->
                        io:fwrite("Invalid user~n"),
                        gen_tcp:send(Socket, "Invalid user\n");
                    invalid_password ->
                        io:fwrite("Invalid password~n"),
                        gen_tcp:send(Socket, "Invalid password\n")
                end,
                comunicator(Socket) %continua a receber mensagens
        end 
    end.
