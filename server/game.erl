-module(game).

-export([init/2]).

init(Player1, Player2) ->
    Players = initialize_players(Player1, Player2),
    {Pid1, _} = Player1,
    {Pid2, _} = Player2,
    Pid1 ! {matchStarted, self()}, % Envia mensagem para o jogador 1 a dizer que o jogo começou
    Pid2 ! {matchStarted, self()}, % Envia mensagem para o jogador 2 a dizer que o jogo começou
    Match_data = #{players => Players}, % preciso adicionar tempo, items a este mapa final
    Keys_process = spawn(fun() -> key_updater() end),
    server_comunicator(Match_data, Keys_process).


initialize_players({Pid1, Username1}, {Pid2, Username2}) ->
    P1 = #{name => Username1, x => 50, y => 450, points => 0,
           w => false, s => false, a => false, d => false, speed => 0},
    P2 = #{name => Username2, x => 950, y => 450, points => 0,
           w => false, s => false, a => false, d => false, speed => 0},
    Players = #{Pid1 => P1, Pid2 => P2},
    Players.


server_comunicator(Match_data, Keys_process) ->
       receive
              {key_press, Key, PlayerPid} ->
                     Keys_process ! {key_press, Match_data, Key, PlayerPid, self()},
                     receive
                            {updated, Players1} ->
                                   io:fwrite("Key updated: ~p~n", [Key]), %debug
                                   Match_data1 = maps:put(players, Players1, Match_data),
                                   io:fwrite("Match_data1: ~p~n", [Match_data1]), %debug
                                   Match_data2 = game_logic(Match_data1),
                                   server_comunicator(Match_data2, Keys_process);
                            _ ->
                                   io:fwrite("Error updating key~n"), %debug
                                   server_comunicator(Match_data, Keys_process)
                     end;
              {key_release, Key, PlayerPid} ->
                     Keys_process ! {key_release, Match_data, Key, PlayerPid, self()},
                     receive
                            {updated, Players1} ->
                                   io:fwrite("Key updated: ~p~n", [Key]), %debug
                                   Match_data1 = maps:put(players, Players1, Match_data),
                                   io:fwrite("Match_data1: ~p~n", [Match_data1]), %debug
                                   Match_data2 = game_logic(Match_data1),
                                   server_comunicator(Match_data2, Keys_process);
                            _ ->
                                   io:fwrite("Error updating key~n"), %debug
                                   server_comunicator(Match_data, Keys_process)
                     end
       end.


key_updater() ->
       receive
              {key_press, Match_data, Key, PlayerPid, From} ->
                     Players = maps:get(players, Match_data),
                     Player = maps:get(PlayerPid, Players), 
                     io:fwrite("Key pressed: ~p~n", [Key]), %debug
                     case Key of
                            "w" ->
                                   Player1 = maps:put(w, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};       
                            "s" ->
                                   Player1 = maps:put(s, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};
                            "a" ->
                                   Player1 = maps:put(a, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};
                            "d" ->
                                   Player1 = maps:put(d, true, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1}
                     end,
                     key_updater();
              {key_release, Match_data, Key, PlayerPid, From} ->
                     Players = maps:get(players, Match_data),
                     Player = maps:get(PlayerPid, Players), 
                     io:fwrite("Key released: ~p~n", [Key]), %debug
                     case Key of
                            "w" ->
                                   Player1 = maps:put(w, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};    
                            "s" ->
                                   Player1 = maps:put(s, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};
                            "a" ->
                                   Player1 = maps:put(a, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1};
                            "d" ->
                                   Player1 = maps:put(d, false, Player),
                                   Players1 = maps:put(PlayerPid, Player1, Players),
                                   io:fwrite("Players1: ~p~n", [Players1]), %debug
                                   From ! {updated, Players1}
                     end,
                     key_updater()
       end.


game_logic(Match_data) ->
       Players = maps:get(players, Match_data),
       [{Pid1, P1}, {Pid2, P2}] = maps:to_list(Players),
       {X1, Y1} = movement(P1),
       {X2, Y2} = movement(P2),
       P1_2 = maps:put(x, X1, P1),
       P1_3 = maps:put(y, Y1, P1_2),
       P2_2 = maps:put(x, X2, P2),
       P2_3 = maps:put(y, Y2, P2_2),
       Players1 = maps:put(Pid1, P1_3, Players),
       Players2 = maps:put(Pid2, P2_3, Players1),
       Match_data1 = maps:put(players, Players2, Match_data),
       Pid1 ! {game_update, Match_data1}, % Envia mensagem para o jogador 1 com os dados atualizados
       Pid2 ! {game_update, Match_data1}, % Envia mensagem para o jogador 2 com os dados atualizados
       io:fwrite("Match_data1: ~p~n", [Match_data1]), %debug
       Match_data1.
       


movement(P) ->
       W = maps:get(w, P),
       S = maps:get(s, P),
       A = maps:get(a, P),
       D = maps:get(d, P),
       X = maps:get(x, P),
       Y = maps:get(y, P),
       case {W, S, A, D} of
              {true, false, false, false} ->
                     {X, Y - 1};
              {false, true, false, false} ->
                     {X, Y + 1};
              {false, false, true, false} ->
                     {X - 1, Y};
              {false, false, false, true} ->
                     {X + 1, Y};
              _ ->
                     {X, Y}
       end.
