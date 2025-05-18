-module(user_functions).

-export([accounts_manager/1, get_top10/1]).

-import(file_functions, [write_content/2]).

accounts_manager(Accounts) ->
    receive
        {login, Username, Password, From} ->
            login(Accounts, Username, Password, From),
            accounts_manager(Accounts);
        {register, Username, Password, From} ->
            New_accounts = register(Accounts, Username, Password, From),
            accounts_manager(New_accounts);
        {delete_user, Username, Password, From} ->
            New_accounts = delete_user(Accounts, Username, Password, From),
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
            accounts_manager(New_accounts);
        {get_top10, From} ->
            Top10 = get_top10(Accounts),
            From ! Top10,
            accounts_manager(Accounts)
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
            file_functions:write_content("contas", New_accounts) % Adiciona o novo utilizador ao ficheiro
    end,
    New_accounts.

delete_user(Accounts, Username, Password, From) ->
    case maps:find(Username, Accounts) of
        {ok, {PasswordStored, _, _}} ->
            if
                Password == PasswordStored ->
                    New_accounts = maps:remove(Username, Accounts),
                    file_functions:write_content("contas", New_accounts), % Remove o utilizador do ficheiro
                    From ! success;
                true ->
                    New_accounts = Accounts,
                    From ! invalid_password
            end;
        _ ->
            New_accounts = Accounts,
            From ! invalid_user
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
    file_functions:write_content("contas", New_accounts),
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
    file_functions:write_content("contas", New_accounts),
    New_accounts.
    
get_top10(Accounts) ->
    Accounts_list = maps:to_list(Accounts), % [{Username, {Password, Level, WinLossSequence}}]
    Accounts_list1 = convert_to_list_of_tuples(Accounts_list, []), %[{Username, Level, WinLossSequence}]
    Top = lists:sort(sort_fun(), Accounts_list1), % ordena a lista por nivel e sequencia de vitorias/derrotas
    Top10 = lists:sublist(Top, 10), % fica com os 10 primeiros
    Top10.
    

convert_to_list_of_tuples([], List) -> List;
convert_to_list_of_tuples([H | T], List) ->
    {Username, {_, Level, WinLossSequence}} = H,
    List1 = [{Username, list_to_integer(Level), list_to_integer(WinLossSequence)} | List],
    convert_to_list_of_tuples(T, List1).

% Funçao de comparação para ordenar a lista
% lists:sort usa os resultados da funçao de comparação para ordenar a lista
% normalmente a funçao de comparação devolve true se o primeiro elemento for menor que o segundo
% pois a sort devolve a lista ordenada do menor para o maior
% neste caso devolve true se o primeiro elemento for maior que o segundo
% fazendo já assim uma especie de inverse na lista e ficando do maior para o menor
sort_fun() ->
    fun(User1, User2) ->
        {_, Level1, WinLossSequence1} = User1,
        {_, Level2, WinLossSequence2} = User2,
        if 
            Level1 > Level2 -> 
                true;
            Level1 < Level2 ->
                false;
            true ->
                if
                    WinLossSequence1 > WinLossSequence2 ->
                        true;
                    true ->
                        false
                end
        end
    end.
        