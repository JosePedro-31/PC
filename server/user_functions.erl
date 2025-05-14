-module(user_functions).

-export([accounts_manager/1]).

-import(file_functions, [write_content/2]).

accounts_manager(Accounts) ->
    receive
        {login, Username, Password, From} ->
            login(Accounts, Username, Password, From),
            accounts_manager(Accounts);
        {register, Username, Password, From} ->
            New_accounts = register(Accounts, Username, Password, From),
            accounts_manager(New_accounts);
        {getLevel, Username, From} ->
            case maps:find(Username, Accounts) of
                {ok, {_, Level, _}} ->
                    Level1 = list_to_integer(Level),
                    From ! Level1;
                _ ->
                    From ! invalid_user
            end,
            accounts_manager(Accounts);
        {game_won, Username} ->
            New_accounts = game_won(Accounts, Username),
            accounts_manager(New_accounts);
        {game_lost, Username} ->
            New_accounts = game_lost(Accounts, Username),
            accounts_manager(New_accounts)
    end.

login(Accounts, Username, Password, From) ->
    case maps:find(Username, Accounts) of
        {ok, {PasswordStored, _, _}} ->
            if
                Password == PasswordStored ->
                    From ! success;
                true ->
                    From ! invalid_password
            end;
        _ ->
            From ! invalid_user
    end.

register(Accounts, Username, Password, From) ->
    case maps:find(Username, Accounts) of
        {ok, _} -> % Se o utilizador já existe
            New_accounts = Accounts, % Retorna o mapa original sem alterações
            From ! user_already_exists;
        _ ->
            % Adiciona o novo utilizador ao mapa com nivel 1 e sequencia de vitorias/derrotas 0
            New_accounts = maps:put(Username, {Password, "1", "0"}, Accounts), 
            From ! registration_successful,
            file_functions:write_content("teste", New_accounts) % Adiciona o novo utilizador ao ficheiro
    end,
    New_accounts.

game_won(Accounts, Username) ->
    {ok, {Password, Level, WinLossSequence}} = maps:find(Username, Accounts),
    WinLossSequence1 = list_to_integer(WinLossSequence),
    Level1 = list_to_integer(Level),
    if
        WinLossSequence1 < 0 -> % Se estiver numa serie de derrotas começa serie de vitorias
            WinLossSequence2 = 1;
        true -> % Se estiver numa serie de vitorias aumenta a serie
            WinLossSequence2 = WinLossSequence1 + 1
    end,
    if 
        WinLossSequence2 == Level1 -> % Se a serie de vitorias for igual ao nivel do jogador
            Level2 = Level1 + 1, % Aumenta o nivel
            WinLossSequence3 = 0; % Reinicia a serie de vitorias
        true ->
            Level2 = Level1, % Mantem o nivel
            WinLossSequence3 = WinLossSequence2 % Mantem a serie de vitorias
    end,
    % Atualiza o mapa com os novos valores
    New_accounts = maps:put(Username, {Password, integer_to_list(Level2), integer_to_list(WinLossSequence3)}, Accounts),
    % Guarda no ficheiro os novos valores
    file_functions:write_content("teste", New_accounts),
    New_accounts.


game_lost(Accounts, Username) ->
    {ok, {Password, Level, WinLossSequence}} = maps:find(Username, Accounts),
    WinLossSequence1 = list_to_integer(WinLossSequence),
    Level1 = list_to_integer(Level),
    if
        WinLossSequence1 > 0 -> % Se estiver numa serie de vitorias começa serie de derrotas
            WinLossSequence2 = -1;
        true -> % Se estiver numa serie de derrotas aumenta a serie
            WinLossSequence2 = WinLossSequence1 - 1
    end,
    if 
        (WinLossSequence2 == -(Level1 div 2)) and (Level1 > 1) -> % Se a serie de derrotas for igual a metade do nivel do jogador
            Level2 = Level1 - 1, % Diminui o nivel
            WinLossSequence3 = 0; % Reinicia a serie de derrotas
        true ->
            Level2 = Level1, % Mantem o nivel
            WinLossSequence3 = WinLossSequence2 % Mantem a serie de derrotas
    end,
    % Atualiza o mapa com os novos valores
    New_accounts = maps:put(Username, {Password, integer_to_list(Level2), integer_to_list(WinLossSequence3)}, Accounts),
    % Guarda no ficheiro os novos valores
    file_functions:write_content("teste", New_accounts),
    New_accounts.
    