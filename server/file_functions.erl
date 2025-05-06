-module (file_functions).

-export ([write_content/2, read_content/1]).

write_content(Content, Filename) ->
    file:write_file(Filename, Content).


read_content(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            % eliminar os \r e \n do conteudo lido e criar uma lista com as linhas
            Data = re:split(Content, "\r"), % Separa o conteudo pelos \r e cria uma lista com os elementos
            %Data1 = lists:flatten(Data), % junta todos os elementos da lista em uma só
            %io:fwrite("Data1: ~p~n", [Data1]), %debug
            Data2 = re:split(Data, "\n"), % Separa o conteudo pelos \n e cria uma lista com os elementos
            case Data2 of
                [<<>>] -> % Caso ficheiro lido esteja vazio cria map vazio
                    #{};
                _ -> % Caso contrário, chama a função parser e cria um map com os dados dos utilizadores
                    parser(Data2, #{}) % #{Username => {Password, Nivel}}
            end;
        {error, Reason} ->
            io:fwrite("Error reading file: ~p~n", [Reason]),
            #{}
    end.


parser([], Data) -> Data;
parser([H | T], Data) ->
    if
        H == [<<>>] ->
            parser(T, Data);
        true ->
            [U, P, N] = re:split(H, ","),
            Username = binary_to_list(U),
            Password = binary_to_list(P),
            Level = binary_to_list(N),
            io:fwrite("Username: ~p, Password: ~p, Level: ~p~n", [Username, Password, Level]),
            if 
                Username == [<<>>] ->
                    Data1 = Data;
                true ->
                    Data1 = maps:put(Username, {Password, Level}, Data)
            end,
            if 
                T == [] ->
                    Data1;
                true ->
                    parser(T, Data1)
            end
        end.
