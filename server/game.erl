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
    Items = #{blue => [], red => [], green => [], orange => []},
    Match_data3 = maps:put(items, Items, Match_data2),
    server_comunicator(Match_data3).


initialize_players({Pid1, Username1}, {Pid2, Username2}) ->
    P1 = #{name => Username1, x => 250, y => 300, points => 0,
           w => false, s => false, a => false, d => false, numberShots => 0,
           speedX => 0, speedY => 0, spawnX => 250, spawnY => 300,
           shotCooldown => 300, previousShotTime => os:timestamp(), shotSpeed => 20},
    P2 = #{name => Username2, x => 750, y => 300, points => 0,
           w => false, s => false, a => false, d => false, numberShots => 0,
           speedX => 0, speedY => 0, spawnX => 750, spawnY => 300,
           shotCooldown => 300, previousShotTime => os:timestamp(), shotSpeed => 20},
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
       PreviousShotTime = maps:get(previousShotTime, Player),
       CurrentTime = os:timestamp(),
       Time = timer:now_diff(CurrentTime, PreviousShotTime) div 1000, % Converter para milisegundos
       ShotCooldown = maps:get(shotCooldown, Player),
       if
              Time >= ShotCooldown ->              
                     PlayerShots = maps:get(PlayerPid, Shots),
                     PlayerNumberShots = maps:get(numberShots, Player),
                     NewPlayerNumberShots = PlayerNumberShots + 1,
                     Player1 = maps:put(numberShots, NewPlayerNumberShots, Player),
                     Player2 = maps:put(previousShotTime, os:timestamp(), Player1),
                     PlayerX = maps:get(x, Player1),
                     PlayerY = maps:get(y, Player1),
                     X1 = X - PlayerX, % fazer "translate" ao ponto do tiro para a sua posição relativa à origem
                     Y1 = Y - PlayerY, % ser a mesma que a sua posição inicial relativa ao jogador
                     Angle = math:atan2(Y1, X1), % Assim podemos calcular o ângulo do tiro relativo a (0,0) que será  mesmo
                                                 % que o angulo do seu ponto inicial relativo ao jogador
                     NewShot = [PlayerX, PlayerY, Angle],
                     PlayerShots1 = [NewShot | PlayerShots],
                     Shots1 = maps:put(PlayerPid, PlayerShots1, Shots),
                     Players1 = maps:put(PlayerPid, Player2, Players),
                     Match_data1 = maps:put(players, Players1, Match_data),
                     Match_data2 = maps:put(shots, Shots1, Match_data1);
              true ->
                     Match_data2 = Match_data
       end,
       Match_data2.
       


game_logic(Match_data) ->
       Players = maps:get(players, Match_data),
       [{Pid1, P1}, {Pid2, P2}] = maps:to_list(Players),

       % Atualizar a posição dos jogadores
       {X1, Y1, SpeedX1, SpeedY1} = movement(P1),
       {X2, Y2, SpeedX2, SpeedY2} = movement(P2),
       P1_2 = maps:put(x, X1, P1),
       P1_3 = maps:put(y, Y1, P1_2),
       P1_4 = maps:put(speedX, SpeedX1, P1_3),
       P1_5 = maps:put(speedY, SpeedY1, P1_4),
       P2_2 = maps:put(x, X2, P2),
       P2_3 = maps:put(y, Y2, P2_2),
       P2_4 = maps:put(speedX, SpeedX2, P2_3),
       P2_5 = maps:put(speedY, SpeedY2, P2_4),

       % atualizar a posição dos tiros
       Shots = maps:get(shots, Match_data),
       Player1_Shots = maps:get(Pid1, Shots),
       Player2_Shots = maps:get(Pid2, Shots),
       Player1_Shots2 = movement_shots(Player1_Shots, [], P1_5),
       Player2_Shots2 = movement_shots(Player2_Shots, [], P2_5),

       % Spawn de items
       Items = maps:get(items, Match_data),
       Items1 = spawn_items(Items),

       % Verificar Colisoes
       {P1_6, P2_6, Player2_Shots3} = colision_player_shot(P1_5, P2_5, Player2_Shots2, Player2_Shots2),
       {P2_7, P1_7, Player1_Shots3} = colision_player_shot(P2_6, P1_6, Player1_Shots2, Player1_Shots2),

       {P1_8, Player1_Shots4} = collision_shot_wall(P1_7 ,Player1_Shots3, []),
       {P2_8, Player2_Shots4} = collision_shot_wall(P2_7, Player2_Shots3, []),

       {P1_9, P2_9} = collision_player_wall(P1_8, P2_8),
       
       {P1_10, Items2} = collision_player_item(P1_9, Items1),
       {P2_10, Items3} = collision_player_item(P2_9, Items2),

       P1_11 = item_effect_updater(P1_10),
       P2_11 = item_effect_updater(P2_10),

       Players1 = maps:put(Pid1, P1_11, Players),
       Players2 = maps:put(Pid2, P2_11, Players1),
       Shots1 = maps:put(Pid1, Player1_Shots4, Shots),
       Shots2 = maps:put(Pid2, Player2_Shots4, Shots1),
       Match_data1 = maps:put(players, Players2, Match_data),
       Match_data2 = maps:put(shots, Shots2, Match_data1),
       Match_data3 = maps:put(items, Items3, Match_data2),

       StartTime = maps:get(startTime, Match_data3),
       CurrentTime = calculate_time(StartTime),

       if 
              CurrentTime >= 60 ->
                     end_game(Match_data3);
              true ->
                     Pid1 ! {game_update, Match_data3, CurrentTime}, % Envia mensagem para o jogador 1 com os dados atualizados
                     Pid2 ! {game_update, Match_data3, CurrentTime}, % Envia mensagem para o jogador 2 com os dados atualizados
                     Match_data3
       end.

       


movement(P) ->
       W = maps:get(w, P),
       S = maps:get(s, P),
       A = maps:get(a, P),
       D = maps:get(d, P),
       X = maps:get(x, P),
       Y = maps:get(y, P),
       SpeedX = maps:get(speedX, P),
       SpeedY = maps:get(speedY, P),
       case {A, D} of
              {false, true} ->
                     if 
                            (SpeedX < 30) and (SpeedX >= 0) -> 
                                   SpeedX1 = SpeedX + 1;
                            (SpeedX < 0) ->
                                   SpeedX1 = SpeedX + 2;% Aumenta a força de travagem
                            true ->
                                   SpeedX1 = SpeedX
                     end;
              {true, false} ->
                     if 
                            (SpeedX > -30) and (SpeedX =< 0) ->
                                   SpeedX1 = SpeedX - 1;
                            (SpeedX > 0) ->
                                   SpeedX1 = SpeedX - 2; % Aumenta a força de travagem
                            true ->
                            SpeedX1 = SpeedX
                     end;
              {_, _} ->
                     case SpeedX of
                            ValueX when ValueX > 0 -> 
                                   SpeedX1 = SpeedX - 1;
                            ValueX when ValueX < 0 ->
                                   SpeedX1 = SpeedX + 1;
                            _ ->
                                   SpeedX1 = 0
                     end
       end,
       case {W, S} of
              {false, true} ->
                     if 
                            (SpeedY < 30) and (SpeedY >= 0) ->
                                   SpeedY1 = SpeedY + 1;
                            (SpeedY < 0) ->
                                   SpeedY1 = SpeedY + 2; % Aumenta a força de travagem
                            true ->
                            SpeedY1 = SpeedY
                     end;
              {true, false} ->
                     if 
                            (SpeedY > -30) and (SpeedY =< 0) ->
                                   SpeedY1 = SpeedY - 1;
                            (SpeedY > 0) ->
                                   SpeedY1 = SpeedY - 2; % Aumenta a força de travagem
                            true ->
                                   SpeedY1 = SpeedY
                     end;
              {_, _} ->
                     case SpeedY of
                            ValueY when ValueY > 0 -> 
                                   SpeedY1 = SpeedY - 1;
                            ValueY when ValueY < 0 ->
                                   SpeedY1 = SpeedY + 1;
                            _ ->
                                   SpeedY1 = 0
                     end
       end,
       X1 = X + SpeedX1,
       Y1 = Y + SpeedY1,
       Result = {X1, Y1, SpeedX1, SpeedY1},
       Result.


movement_shots([], PlayerShots, _) -> PlayerShots;
movement_shots([H | T], PlayerShots, Player) ->
       [X, Y, Angle] = H,
       ShotSpeed = maps:get(shotSpeed, Player),
       X1 = X + ShotSpeed * math:cos(Angle),
       Y1 = Y + ShotSpeed * math:sin(Angle),
       PlayerShots1 = [X1, Y1, Angle],
       PlayerShots2 = [PlayerShots1 | PlayerShots],
       movement_shots(T, PlayerShots2, Player).


spawn_items(Items) ->
       Blue = maps:get(blue, Items),
       Red = maps:get(red, Items),
       Green = maps:get(green, Items),
       Orange = maps:get(orange, Items),
       Spawn_blue = rand:uniform(100),
       Spawn_red = rand:uniform(100),
       Spawn_green = rand:uniform(100),
       Spawn_orange = rand:uniform(100),
       if
              (Spawn_blue =< 1) and (length(Blue) < 2) ->
                     % Adiciona um item azul
                     Blue1 = [[rand:uniform(900), rand:uniform(500)] | Blue];
              true ->
                     Blue1 = Blue
       end,
       if
              (Spawn_red =< 1) and (length(Red) < 2) ->
                     % Adiciona um item vermelho
                     Red1 = [[rand:uniform(900), rand:uniform(500)] | Red];
              true ->
                     Red1 = Red
       end,
       if
              (Spawn_green =< 1) and (length(Green) < 2) ->
                     % Adiciona um item verde
                     Green1 = [[rand:uniform(900), rand:uniform(500)] | Green];
              true ->
                     Green1 = Green
       end,
       if
              (Spawn_orange =< 1) and (length(Orange) < 2) ->
                     % Adiciona um item laranja
                     Orange1 = [[rand:uniform(900), rand:uniform(500)] | Orange];
              true ->
                     Orange1 = Orange
       end,
       Items1 = maps:put(blue, Blue1, Items),
       Items2 = maps:put(red, Red1, Items1),
       Items3 = maps:put(green, Green1, Items2),
       Items4 = maps:put(orange, Orange1, Items3),
       Items4.


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
                     Player4 = maps:put(speedX, 0, Player3),
                     Player5 = maps:put(speedY, 0, Player4),
                     Oponent1 = maps:put(x, Oponent_spawnX, Oponent),
                     Oponent2 = maps:put(y, Oponent_spawnY, Oponent1),
                     Oponent3 = maps:put(speedX, 0, Oponent2),
                     Oponent4 = maps:put(speedY, 0, Oponent3),
                     Oponent5 = maps:put(points, OponentPoints + 2, Oponent4);
              
              ((OponentX + Radius) >= 1000) or ((OponentX - Radius) =< 0) or ((OponentY + Radius) >= 600) or ((OponentY - Radius) =< 0) ->
                     % Dá respawn e adiciona 2 pontos ao Player
                     Player1 = maps:put(x, Player_spawnX, Player), 
                     Player2 = maps:put(y, Player_spawnY, Player1),
                     Player3 = maps:put(speedX, 0, Player2),
                     Player4 = maps:put(speedY, 0, Player3),
                     Player5 = maps:put(points, PlayerPoints + 2, Player4),
                     Oponent1 = maps:put(x, Oponent_spawnX, Oponent), 
                     Oponent3 = maps:put(y, Oponent_spawnY, Oponent1),
                     Oponent4 = maps:put(speedX, 0, Oponent3),
                     Oponent5 = maps:put(speedY, 0, Oponent4);

              true ->
                     Player5 = Player,
                     Oponent5 = Oponent
       end,
       {Player5, Oponent5}.

collision_player_item(Player, Items) ->
       Blue = maps:get(blue, Items),
       Red = maps:get(red, Items),
       Green = maps:get(green, Items),
       Orange = maps:get(orange, Items),
       {Player1, Blue1} = collision_player_item_aux(Player, Blue, [], blue),
       {Player2, Red1} = collision_player_item_aux(Player1, Red, [], red),
       {Player3, Green1} = collision_player_item_aux(Player2, Green, [], green),
       {Player4, Orange1} = collision_player_item_aux(Player3, Orange, [], orange),
       Items1 = maps:put(blue, Blue1, Items),
       Items2 = maps:put(red, Red1, Items1),
       Items3 = maps:put(green, Green1, Items2),
       Items4 = maps:put(orange, Orange1, Items3),

       {Player4, Items4}.

collision_player_item_aux(Player, [], Item, _) -> {Player, Item};
collision_player_item_aux(Player, [H | T], Item, Type) ->
       PlayerX = maps:get(x, Player),
       PlayerY = maps:get(y, Player),
       [ItemX, ItemY] = H,
       ItemRadius = 15,
       PlayerRadius = 20,
       Distance = math:sqrt(math:pow(ItemX - PlayerX, 2) + math:pow(ItemY - PlayerY, 2)),
       if
              Distance =< (ItemRadius + PlayerRadius) ->
                     if
                            Type =:= blue ->
                                   NewShotCooldown = 50,
                                   Item1 = Item,
                                   Player1 = maps:put(shotCooldown, NewShotCooldown, Player);
                            Type =:= red ->
                                   Item1 = Item,
                                   NewShotCooldown = 550,
                                   Player1 = maps:put(shotCooldown, NewShotCooldown, Player);
                            Type =:= green ->
                                   Item1 = Item,
                                   NewShotSpeed = 50,
                                   Player1 = maps:put(shotSpeed, NewShotSpeed, Player);
                            Type =:= orange ->
                                   Item1 = Item,
                                   NewShotSpeed = 5,
                                   Player1 = maps:put(shotSpeed, NewShotSpeed, Player);
                            true ->
                                   Item1 = Item,
                                   Player1 = Player
                     end;          
              true ->
                     Player1 = Player,
                     Item1 = [H | Item]
       end,
       collision_player_item_aux(Player1, T, Item1, Type).


item_effect_updater(Player) ->
       ShotCooldown = maps:get(shotCooldown, Player),
       ShotSpeed = maps:get(shotSpeed, Player),
       if
              ShotCooldown > 300 ->
                     NewShotCooldown = ShotCooldown - 1,
                     Player1 = maps:put(shotCooldown, NewShotCooldown, Player);
              ShotCooldown < 300 ->
                     NewShotCooldown = ShotCooldown + 1,
                     Player1 = maps:put(shotCooldown, NewShotCooldown, Player);
              true ->
                     Player1 = Player
       end,
       if
              ShotSpeed > 30 ->
                     NewShotSpeed = ShotSpeed - 0.1,
                     Player2 = maps:put(shotSpeed, NewShotSpeed, Player1);
              ShotSpeed < 30 ->
                     NewShotSpeed = ShotSpeed + 0.1,
                     Player2 = maps:put(shotSpeed, NewShotSpeed, Player1);
              true ->
                     Player2 = Player
       end,
       Player2.


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