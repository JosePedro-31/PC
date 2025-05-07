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
                {ok, {_, Level}} ->
                    io:fwrite("Level: ~p~n", [Level]), %debug
                    From ! Level;
                _ ->
                    From ! invalid_user
            end,
            accounts_manager(Accounts)
    end.

login(Accounts, Username, Password, From) ->
    case maps:find(Username, Accounts) of
        {ok, {PasswordStored, _}} ->
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
        {ok, _} ->
            New_accounts = Accounts, % Retorna o mapa original sem alterações
            From ! user_already_exists;
        _ ->
            % Adiciona o novo utilizador ao mapa com nivel 1
            New_accounts = maps:put(Username, {Password, "1"}, Accounts), 
            From ! registration_successful,
            file_functions:write_content(teste, New_accounts) % Adiciona o novo utilizador ao ficheiro
    end,
    New_accounts.