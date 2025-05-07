-module(game).

-export([init/2]).

init(Player1, Player2) ->
    Players = initialize_players(Player1, Player2),
    {Pid1, _} = Player1,
    {Pid2, _} = Player2,
    Pid1 ! {matchStarted, self()}, % Envia mensagem para o jogador 1 a dizer que o jogo começou
    Pid2 ! {matchStarted, self()}, % Envia mensagem para o jogador 2 a dizer que o jogo começou
    Match_data = #{players => Players}. % preciso adicionar tempo, items a este mapa final


initialize_players({Pid1, Username1}, {Pid2, Username2}) ->
    P1 = #{name => Username1, x => 50, y => 450, points => 0,
           w => false, s => false, a => false, d => false, speed => 0},
    P2 = #{name => Username2, x => 950, y => 450, points => 0,
           w => false, s => false, a => false, d => false, speed => 0},
    Players = #{Pid1 => P1, Pid2 => P2},
    Players.