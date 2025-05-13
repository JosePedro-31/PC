-module(game).

-export([init/2]).

init(Player1, Player2) ->
    Players = initialize_players(Player1, Player2),
    {Pid1, _} = Player1,
    {Pid2, _} = Player2,
    Pid1 ! {matchStarted, self()}, % Envia mensagem para o jogador 1 a dizer que o jogo começou
    Pid2 ! {matchStarted, self()}, % Envia mensagem para o jogador 2 a dizer que o jogo começou
    Match_data = #{players => Players}, % preciso adicionar tempo, items a este mapa final
    Shots = #{Pid1 => [], Pid2 => []},
    Match_data1 = maps:put(shots, Shots, Match_data), % {shots => {Pid1 => [lista com listas de tiros], Pid2 => [lista com listas de tiros]}},
    StartTime = os:timestamp(),
    Match_data2 = maps:put(startTime, StartTime, Match_data1),
    server_comunicator(Match_data2).


initialize_players({Pid1, Username1}, {Pid2, Username2}) ->
    P1 = #{name => Username1, x => 250, y => 300, points => 0,
           w => false, s => false, a => false, d => false, numberShots => 0,
           accelerationX => 0, accelerationY => 0, spawnX => 250, spawnY => 300},
    P2 = #{name => Username2, x => 750, y => 300, points => 0,
           w => false, s => false, a => false, d => false, numberShots => 0,
           accelerationX => 0, accelerationY => 0, spawnX => 750, spawnY => 300},
    Players = #{Pid1 => P1, Pid2 => P2},
    Players.


server_comunicator(Match_data) ->
       receive
              {keyPress, Key, PlayerPid} ->
                     Match_data1 = key_updater(keyPress, Match_data, Key, PlayerPid),
                     io:fwrite("Key updated: ~p~n", [Key]), %debug
                     io:fwrite("Match_data1: ~p~n", [Match_data1]), %debug
                     Match_data2 = game_logic(Match_data1),
                     server_comunicator(Match_data2);

              {keyRelease, Key, PlayerPid} ->
                     Match_data1 = key_updater(keyRelease, Match_data, Key, PlayerPid),
                     io:fwrite("Key released: ~p~n", [Key]), %debug
                     io:fwrite("Match_data1: ~p~n", [Match_data1]), %debug
                     Match_data2 = game_logic(Match_data1),
                     server_comunicator(Match_data2);
              {shot, X, Y, PlayerPid} ->
                     X1 = list_to_integer(X),
                     Y1 = list_to_integer(Y),
                     Match_data1 = create_shot(Match_data, X1, Y1, PlayerPid),
                     Match_data2 = game_logic(Match_data1),
                     server_comunicator(Match_data2)
              after 
                     25 ->
                            Match_data1 = game_logic(Match_data),
                            server_comunicator(Match_data1)
       end.


key_updater(TypeOfPress, Match_data, Key, PlayerPid) ->

       Players = maps:get(players, Match_data),
       Player = maps:get(PlayerPid, Players), 
       case TypeOfPress of
              keyPress ->
                     case Key of
                            "w" ->
                                   Player1 = maps:put(w, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "s" ->
                                   Player1 = maps:put(s, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "a" ->
                                   Player1 = maps:put(a, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "d" ->
                                   Player1 = maps:put(d, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]) %debug
                     end;
              keyRelease ->
                     case Key of
                            "w" ->
                                   Player1 = maps:put(w, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "s" ->
                                   Player1 = maps:put(s, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "a" ->
                                   Player1 = maps:put(a, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]); %debug
                            "d" ->
                                   Player1 = maps:put(d, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]) %debug
                     end
       end,
       Match_data1 = maps:put(players, Players1, Match_data),
       Match_data1.

create_shot(Match_data, X, Y, PlayerPid) ->
       Players = maps:get(players, Match_data),
       Shots = maps:get(shots, Match_data),
       Player = maps:get(PlayerPid, Players),
       PlayerShots = maps:get(PlayerPid, Shots),
       PlayerNumberShots = maps:get(numberShots, Player),
       NewPlayerNumberShots = PlayerNumberShots + 1,
       Player1 = maps:put(numberShots, NewPlayerNumberShots, Player),
       PlayerX = maps:get(x, Player1),
       PlayerY = maps:get(y, Player1),
       X1 = X - PlayerX, % fazer "translate" ao ponto do tiro para a sua posição relativa à origem
       Y1 = Y - PlayerY, % ser a mesma que a sua posição inicial relativa ao jogador
       Angle = math:atan2(Y1, X1), % Assim podemos calcular o ângulo do tiro relativo a (0,0) que será  mesmo
                                   % que o angulo do seu ponto inicial relativo ao jogador
       NewShot = [PlayerX, PlayerY, Angle],
       PlayerShots1 = [NewShot | PlayerShots],
       PlayerShots2 = lists:sublist(PlayerShots1, 7), % Limita o número de tiros a 7
       Shots1 = maps:put(PlayerPid, PlayerShots2, Shots),
       Players1 = maps:put(PlayerPid, Player1, Players),
       Match_data1 = maps:put(players, Players1, Match_data),
       Match_data2 = maps:put(shots, Shots1, Match_data1),
       Match_data2.
       


game_logic(Match_data) ->
       Players = maps:get(players, Match_data),
       [{Pid1, P1}, {Pid2, P2}] = maps:to_list(Players),

       % Atualizar a posição dos jogadores
       {X1, Y1, AccelerationX1, AccelerationY1} = movement(P1),
       {X2, Y2, AccelerationX2, AccelerationY2} = movement(P2),
       P1_2 = maps:put(x, X1, P1),
       P1_3 = maps:put(y, Y1, P1_2),
       P1_4 = maps:put(accelerationX, AccelerationX1, P1_3),
       P1_5 = maps:put(accelerationY, AccelerationY1, P1_4),
       P2_2 = maps:put(x, X2, P2),
       P2_3 = maps:put(y, Y2, P2_2),
       P2_4 = maps:put(accelerationX, AccelerationX2, P2_3),
       P2_5 = maps:put(accelerationY, AccelerationY2, P2_4),

       % atualizar a posição dos tiros
       Shots = maps:get(shots, Match_data),
       Player1_Shots = maps:get(Pid1, Shots),
       Player2_Shots = maps:get(Pid2, Shots),
       Player1_Shots2 = movement_shots(Player1_Shots, []),
       Player2_Shots2 = movement_shots(Player2_Shots, []),

       % Verificar Colisoes
       {P1_6, P2_6, Player2_Shots3} = colision_player_shot(P1_5, P2_5, Player2_Shots2, Player2_Shots2),
       {P2_7, P1_7, Player1_Shots3} = colision_player_shot(P2_6, P1_6, Player1_Shots2, Player1_Shots2),

       {P1_8, Player1_Shots4} = collision_shot_wall(P1_7 ,Player1_Shots3, []),
       {P2_8, Player2_Shots4} = collision_shot_wall(P2_7, Player2_Shots3, []),

       {P1_9, P2_9} = collision_player_wall(P1_8, P2_8),

       Players1 = maps:put(Pid1, P1_9, Players),
       Players2 = maps:put(Pid2, P2_9, Players1),
       Shots1 = maps:put(Pid1, Player1_Shots4, Shots),
       Shots2 = maps:put(Pid2, Player2_Shots4, Shots1),
       Match_data1 = maps:put(players, Players2, Match_data),
       Match_data2 = maps:put(shots, Shots2, Match_data1),

       StartTime = maps:get(startTime, Match_data2),
       CurrentTime = calculate_time(StartTime),

       if 
              CurrentTime >= 20 ->
                     end_game(Match_data2);
              true ->
                     Pid1 ! {game_update, Match_data2, CurrentTime}, % Envia mensagem para o jogador 1 com os dados atualizados
                     Pid2 ! {game_update, Match_data2, CurrentTime}, % Envia mensagem para o jogador 2 com os dados atualizados
                     Match_data2
       end.

       


movement(P) ->
       W = maps:get(w, P),
       S = maps:get(s, P),
       A = maps:get(a, P),
       D = maps:get(d, P),
       X = maps:get(x, P),
       Y = maps:get(y, P),
       AccelerationX = maps:get(accelerationX, P),
       AccelerationY = maps:get(accelerationY, P),
       case {A, D} of
              {false, true} ->
                     if 
                            (AccelerationX < 30) and (AccelerationX >= 0) -> 
                                   AccelerationX1 = AccelerationX + 1;
                            (AccelerationX < 0) ->
                                   AccelerationX1 = AccelerationX + 2;% Aumenta a força de travagem
                            true ->
                                   AccelerationX1 = AccelerationX
                     end;
              {true, false} ->
                     if 
                            (AccelerationX > -30) and (AccelerationX =< 0) ->
                                   AccelerationX1 = AccelerationX - 1;
                            (AccelerationX > 0) ->
                                   AccelerationX1 = AccelerationX - 2; % Aumenta a força de travagem
                            true ->
                            AccelerationX1 = AccelerationX
                     end;
              {_, _} ->
                     case AccelerationX of
                            ValueX when ValueX > 0 -> 
                                   AccelerationX1 = AccelerationX - 1;
                            ValueX when ValueX < 0 ->
                                   AccelerationX1 = AccelerationX + 1;
                            _ ->
                                   AccelerationX1 = 0
                     end
       end,
       case {W, S} of
              {false, true} ->
                     if 
                            (AccelerationY < 30) and (AccelerationY >= 0) ->
                                   AccelerationY1 = AccelerationY + 1;
                            (AccelerationY < 0) ->
                                   AccelerationY1 = AccelerationY + 2; % Aumenta a força de travagem
                            true ->
                            AccelerationY1 = AccelerationY
                     end;
              {true, false} ->
                     if 
                            (AccelerationY > -30) and (AccelerationY =< 0) ->
                                   AccelerationY1 = AccelerationY - 1;
                            (AccelerationY > 0) ->
                                   AccelerationY1 = AccelerationY - 2; % Aumenta a força de travagem
                            true ->
                                   AccelerationY1 = AccelerationY
                     end;
              {_, _} ->
                     case AccelerationY of
                            ValueY when ValueY > 0 -> 
                                   AccelerationY1 = AccelerationY - 1;
                            ValueY when ValueY < 0 ->
                                   AccelerationY1 = AccelerationY + 1;
                            _ ->
                                   AccelerationY1 = 0
                     end
       end,
       X1 = X + AccelerationX1,
       Y1 = Y + AccelerationY1,
       Result = {X1, Y1, AccelerationX1, AccelerationY1},
       Result.


movement_shots([], PlayerShots) -> PlayerShots;
movement_shots([H | T], PlayerShots) ->
       io:fwrite("H: ~p~n", [H]), %debug
       [X, Y, Angle] = H,
       X1 = X + 30 * math:cos(Angle),
       io:fwrite("X1: ~p~n", [X1]), %debug
       Y1 = Y + 30 * math:sin(Angle),
       io:fwrite("Y1: ~p~n", [Y1]), %debug
       PlayerShots1 = [X1, Y1, Angle],
       io:fwrite("PlayerShots1: ~p~n", [PlayerShots1]), %debug
       PlayerShots2 = [PlayerShots1 | PlayerShots],
       movement_shots(T, PlayerShots2).

colision_player_shot(Player, Opponent, [], ShotsOpponent) -> {Player, Opponent, ShotsOpponent};
colision_player_shot(Player, Opponent, [H | T], ShotsOpponent) ->
       PlayerX = maps:get(x, Player),
       PlayerY = maps:get(y, Player),
       OpponentPoints = maps:get(points, Opponent),
       NumberShotsOpponent = maps:get(numberShots, Opponent),
       PlayerRadius = 20,
       [ShotX, ShotY, _] = H,
       ShotRadius = 10,
       Distance = math:sqrt(math:pow(ShotX - PlayerX, 2) + math:pow(ShotY - PlayerY, 2)),
       if
              Distance =< (PlayerRadius + ShotRadius) ->
                     Opponent1 = maps:put(points, OpponentPoints + 1, Opponent),
                     Opponent2 = maps:put(numberShots, NumberShotsOpponent - 1, Opponent1),
                     ShotsOpponent1 = lists:delete(H, ShotsOpponent);
              true ->
                     Opponent2 = Opponent,
                     ShotsOpponent1 = ShotsOpponent
       end,
       colision_player_shot(Player, Opponent2, T, ShotsOpponent1).
                     

collision_shot_wall(Player, [], Shots) -> {Player, Shots};
collision_shot_wall(Player, [H | T], Shots) ->
       [X, Y, _] = H,
       ShotRadius = 10,
       NumberShotsPlayer = maps:get(numberShots, Player),
       if     % Verifica se o tiro colidiu com a parede
              ((X + ShotRadius) >= 1000) or ((X - ShotRadius) =< 0) or ((Y + ShotRadius) >= 600) or ((Y - ShotRadius) =< 0) ->
                     Shots1 = Shots,
                     Player1 = maps:put(numberShots, NumberShotsPlayer - 1, Player);
       true ->% Caso não tenha havido colisão
                     Shots1 = [H | Shots],
                     Player1 = Player
       end,
       collision_shot_wall(Player1, T, Shots1).


collision_player_wall(Player, Oponent) ->
       PlayerX = maps:get(x, Player),
       PlayerY = maps:get(y, Player),
       Player_spawnX = maps:get(spawnX, Player),
       Player_spawnY = maps:get(spawnY, Player),
       PlayerPoints = maps:get(points, Player),
       Radius = 20,
       OponentX = maps:get(x, Oponent),
       OponentY = maps:get(y, Oponent),
       Oponent_spawnX = maps:get(spawnX, Oponent),
       Oponent_spawnY = maps:get(spawnY, Oponent),
       OponentPoints = maps:get(points, Oponent),
       if
              ((PlayerX + Radius) >= 1000) or ((PlayerX - Radius) =< 0) or ((PlayerY + Radius) >= 600) or ((PlayerY - Radius) =< 0) ->
                     % Dá respawn e adiciona 2 pontos ao adversario
                     Player1 = maps:put(x, Player_spawnX, Player), 
                     Player3 = maps:put(y, Player_spawnY, Player1),
                     Player4 = maps:put(accelerationX, 0, Player3),
                     Player5 = maps:put(accelerationY, 0, Player4),
                     Oponent1 = maps:put(x, Oponent_spawnX, Oponent),
                     Oponent2 = maps:put(y, Oponent_spawnY, Oponent1),
                     Oponent3 = maps:put(accelerationX, 0, Oponent2),
                     Oponent4 = maps:put(accelerationY, 0, Oponent3),
                     Oponent5 = maps:put(points, OponentPoints + 2, Oponent4);
              
              ((OponentX + Radius) >= 1000) or ((OponentX - Radius) =< 0) or ((OponentY + Radius) >= 600) or ((OponentY - Radius) =< 0) ->
                     % Dá respawn e adiciona 2 pontos ao Player
                     Player1 = maps:put(x, Player_spawnX, Player), 
                     Player2 = maps:put(y, Player_spawnY, Player1),
                     Player3 = maps:put(accelerationX, 0, Player2),
                     Player4 = maps:put(accelerationY, 0, Player3),
                     Player5 = maps:put(points, PlayerPoints + 2, Player4),
                     Oponent1 = maps:put(x, Oponent_spawnX, Oponent), 
                     Oponent3 = maps:put(y, Oponent_spawnY, Oponent1),
                     Oponent4 = maps:put(accelerationX, 0, Oponent3),
                     Oponent5 = maps:put(accelerationY, 0, Oponent4);

              true ->
                     Player5 = Player,
                     Oponent5 = Oponent
       end,
       {Player5, Oponent5}.

calculate_time(StartTime) ->
       CurrentTime = os:timestamp(),
       % Devolve a diferença em microssegundos, divisão inteira por 1000000 converte para segundos
       Time = timer:now_diff(CurrentTime, StartTime) div 1000000, 
       Time.

end_game(Match_data) ->
       Players = maps:get(players, Match_data),
       [{Pid1, P1}, {Pid2, P2}] = maps:to_list(Players),
       P1_points = maps:get(points, P1),
       P2_points = maps:get(points, P2),
       P1_username = maps:get(name, P1),
       P2_username = maps:get(name, P2),
       if
              P1_points > P2_points ->
                     Pid1 ! {game_won, P1_username},
                     Pid2 ! {game_lost, P2_username};
              P1_points < P2_points ->
                     Pid1 ! {game_lost, P1_username},
                     Pid2 ! {game_won, P2_username};
              true ->
                     Pid1 ! {game_draw},
                     Pid2 ! {game_draw}
       end,
       exit(0). % Termina o processo do jogo