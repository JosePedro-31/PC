-module (server).

-import(file_functions, [read_content/1]).
-import(user_functions, [accounts_manager/1]).
-import(game, [init/2]).

-export ([start/1, server/1, stop/1]).



start(Port) -> register(servidor, spawn(fun() -> server(Port) end)).

stop(Server) -> Server ! stop.

server(Port) ->
    Result = gen_tcp:listen(Port, [binary, {packet, line}]),
    case Result of
        {ok, LSock} ->
            register(lobbyProcess, spawn(fun() -> lobby([]) end)),
            Filename = "teste",
            Accounts = file_functions:read_content(Filename),
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
            Message1 = binary_to_list(Message),
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
                    comunicator(Socket); %continua a receber mensagens
                ["register_user", Username, Password] ->
                    accountsProcess ! {register, Username, Password, self()},
                    receive
                        registration_successful ->
                            io:fwrite("Registration successful~n"),
                            gen_tcp:send(Socket, "Registration successful\n");
                        user_already_exists ->
                            io:fwrite("User already exists~n"),
                            gen_tcp:send(Socket, "User already exists\n")
                    end,
                    comunicator(Socket); %continua a receber mensagens~
                ["join_lobby", Username] -> 
                    accountsProcess ! {getLevel, Username, self()},
                    receive
                        Level ->
                            lobbyProcess ! {join, Username, Level, self()},
                            receive
                                success ->
                                    io:fwrite("Join lobby successful~n"),
                                    gen_tcp:send(Socket, "Join lobby successful\n")
                            end
                    end,
                    comunicator(Socket) %continua a receber mensagens
            end;
        {matchStarted, Match_pid} ->
            io:fwrite("Match started~n"),
            gen_tcp:send(Socket, "Match started\n"),
            match_comunicator(Socket, Match_pid);
        _ ->
            io:fwrite("Unknown message received~n"),
            comunicator(Socket)
    end.


lobby(Pids) ->
    io:fwrite("Lobby: ~p~n", [Pids]), %debug
    receive
        {join, Username, User_level, From} ->
            case Pids of
                [] ->
                    From ! success,
                    lobby([{Username, User_level, From}]);
                _ ->
                    Pids2 = [{Username, User_level, From} | Pids],
                    From ! success,
                    case matchMaking(Pids2) of
                        {matchFound, User1, User2} ->
                            io:fwrite("Match found: ~p and ~p~n", [User1, User2]),
                            {Username1, _, Pid1} = User1,
                            {Username2, _, Pid2} = User2,
                            spawn(fun() -> game:init({Pid1, Username1}, {Pid2, Username2}) end),
                            Pids3 = lists:delete(User1, Pids2),
                            Pids4 = lists:delete(User2, Pids3),
                            lobby(Pids4);
                        noMatch ->
                            io:fwrite("No match found~n"),
                            lobby(Pids2)
                    end
            end
    end.


matchMaking([]) -> noMatch;
matchMaking ([H | T]) ->
    Result = same_level(H, T),
    case Result of
        {matchFound, User1, User2} ->
            {matchFound, User1, User2};
        noMatch ->
            noMatch
    end.

same_level(_, []) -> noMatch;
same_level(User1, [User2 | T]) ->
    {_, Level1, _} = User1,
    case User2 of
        {_, Level2, _} ->
            if
                Level1 == Level2 ->
                    {matchFound, User1, User2};
                true ->
                    same_level(User1, T)
            end
    end.


match_comunicator(Socket, Match_pid) ->
    receive
        {tcp, _, Message} ->
            Message1 = binary_to_list(Message),
            Message2 = string:trim(Message1), %remove os espaços em branco incluindo os \r e \n
            Message3 = string:tokens(Message2, ","), %separa a mensagem em tokens
            case Message3 of
                ["key_press,", Key] ->
                    io:fwrite("Key pressed: ~p~n", [Key]);

                ["key_release,", Key] ->
                    io:fwrite("Key released: ~p~n", [Key])

            end,
            match_comunicator(Socket, Match_pid);
        {matchOver} ->
            io:fwrite("Match over~n"),
            gen_tcp:send(Socket, "Match over\n"),
            comunicator(Socket);
        _ ->
            io:fwrite("Unknown message received~n"),
            match_comunicator(Socket, Match_pid)
    end.