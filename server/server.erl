-module (server).

-import(file_functions, [read_content/1]).
-import(user_functions, [accounts_manager/1]).
-import(game, [init/2]).

-export ([s/0, server/1, stop/1]).


% mudar para como estava
s() -> register(servidor, spawn(fun() -> server(1111) end)).

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


matchMaking([_ | []]) -> noMatch;
matchMaking ([User1 | T]) ->
    {_, Level1, _} = User1,
    [User2 | T1] = T,
    {_, Level2, _} = User2,
    Level_diference = abs(Level1 - Level2),
    if
        Level_diference =< 1 ->
            {matchFound, User1, User2};
        true ->
            matchMaking([User1 | T1]) %continua a procurar por um utilizador para fazer match
    end.


match_comunicator(Socket, Match_pid) ->
    receive
        {tcp, _, Message} ->
            Message1 = binary_to_list(Message),
            Message2 = string:trim(Message1), %remove os espaços em branco incluindo os \r e \n
            Message3 = string:tokens(Message2, ","), %separa a mensagem em tokens
            case Message3 of
                ["key_press", Key] ->
                    io:fwrite("Key pressed: ~p~n", [Key]),
                    Match_pid ! {keyPress, Key, self()};

                ["key_release", Key] ->
                    io:fwrite("Key released: ~p~n", [Key]),
                    Match_pid ! {keyRelease, Key, self()};
                ["shot", X, Y] ->
                    io:fwrite("Shot fired: ~p, ~p~n", [X, Y]),
                    Match_pid ! {shot, X, Y, self()}

            end,
            match_comunicator(Socket, Match_pid);
        {game_update, Match_data, CurrentTime} ->
            Player_data = extract_player_data(Match_data),
            {ShotsPlayer, ShotsOponent} = extract_shots(Match_data),
            Time = extract_time(CurrentTime),
            io:fwrite("Game update: ~p~n", [Player_data]),
            io:fwrite("Shots player: ~p~n", [ShotsPlayer]),
            io:fwrite("Shots oponent: ~p~n", [ShotsOponent]),
            gen_tcp:send(Socket, Player_data),
            gen_tcp:send(Socket, ShotsPlayer),
            gen_tcp:send(Socket, ShotsOponent),
            gen_tcp:send(Socket, Time),
            match_comunicator(Socket, Match_pid);

        {game_won, Username} ->
            gen_tcp:send(Socket, "GameWon\n"),
            accountsProcess ! {game_won, Username},
            comunicator(Socket);
        {game_lost, Username} ->
            gen_tcp:send(Socket, "GameLost\n"),
            accountsProcess ! {game_lost, Username},
            comunicator(Socket);
        {game_draw} ->
            gen_tcp:send(Socket, "GameDraw\n"),
            comunicator(Socket);
        _ ->
            io:fwrite("Unknown message received~n"),
            match_comunicator(Socket, Match_pid)
    end.


extract_player_data(Match_data) ->
    Players = maps:get(players, Match_data),
    [{_, P1}, {_, P2}] = maps:to_list(Players),
    Username1 = maps:get(name, P1),
    X1 = integer_to_list(maps:get(x, P1)),
    Y1 = integer_to_list(maps:get(y, P1)),
    Points1 = integer_to_list(maps:get(points, P1)),
    Username2 = maps:get(name, P2),
    X2 = integer_to_list(maps:get(x, P2)),
    Y2 = integer_to_list(maps:get(y, P2)),
    Points2 = integer_to_list(maps:get(points, P2)),
    Data = lists:flatten(["Players,", Username1, ",", X1, ",", Y1, ",", Points1, ",",
                          Username2, ",", X2, ",", Y2, ",", Points2, "\n"]),
    Data.

extract_shots(Match_data) ->
    Shots = maps:get(shots, Match_data),
    [{Pid1, ShotsPlayer1}, {Pid2, ShotsPlayer2}] = maps:to_list(Shots),
    Players = maps:get(players, Match_data),
    if 
        Pid1 == self() ->
            ShotsPlayer = ShotsPlayer1,
            ShotsOpponent = ShotsPlayer2,
            Player = maps:get(Pid1, Players),
            Oponent = maps:get(Pid2, Players);
        true ->
            ShotsPlayer = ShotsPlayer2,
            ShotsOpponent = ShotsPlayer1,
            Player = maps:get(Pid2, Players),
            Oponent = maps:get(Pid1, Players)
    end,
    NumberShotsPlayer = integer_to_list(maps:get(numberShots, Player)),
    NumberShotsOponent = integer_to_list(maps:get(numberShots, Oponent)),

    DataPlayer = lists:flatten(["ShotsPlayer,", NumberShotsPlayer]),
    DataOponent = lists:flatten(["ShotsOpponent,", NumberShotsOponent]),
    
    DataPlayer2 = extract_shots_data(ShotsPlayer, DataPlayer),
    DataOponent2 = extract_shots_data(ShotsOpponent, DataOponent),
    DataPlayer3 = lists:flatten([DataPlayer2, "\n"]),
    DataOponent3 = lists:flatten([DataOponent2, "\n"]),
    {DataPlayer3, DataOponent3}.
    


extract_shots_data([], Data) -> Data;
extract_shots_data([H | T], Data) ->
    io:fwrite("Shot: ~p~n", [H]),
    [X, Y, _] = H,
    X1 = float_to_list(X, [{decimals, 2}]), % Converte o float para string com 2 casas decimais
    Y1 = float_to_list(Y, [{decimals, 2}]),
    Data1 = lists:flatten([",", X1, ",", Y1]),
    Data2 = lists:flatten([Data, Data1]),
    extract_shots_data(T, Data2).

extract_time(CurrentTime) ->
    Time = integer_to_list(CurrentTime),
    Time1 = lists:flatten(["Time,", Time, "\n"]),
    Time1.