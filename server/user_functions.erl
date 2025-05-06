-module(file_functions).

-export([accounts_manager/1]).

accounts_manager(Accounts) ->
    receive
        {login, Username, Password, From} ->
            login(Accounts, Username, Password, From),
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