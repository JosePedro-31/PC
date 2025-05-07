-module (file_functions).

-export ([write_content/2, read_content/1]).


write_content(Filename, Content) ->
    Content1 = maps:to_list(Content), % Converte o mapa numa lista de tuplos [{Key, Value}]
    Data = "", % Cria uma lista vazia para armazenar os dados formatados
    Data1 = map_formater(Content1, Data), % cria uma lista com as linhas a escrever no ficheiro
    Data2 = lists:flatten(Data1), % junta todos os elementos da lista numa só
    file:write_file(Filename, Data2).


% Cria uma lista com as linhas a escrever no ficheiro
% Formato: Username,Password,Nivel\n
% nota que o \n não é adicionado à ultima linha 
map_formater([], Data) -> Data;
map_formater([H|T], Data) ->   
    {Username, PasswordNivel} = H, % H é um tuplo {Username, {Password, Nivel}}
    {Password, Nivel} = PasswordNivel, % PasswordNivel é um tuplo {Password, Nivel}
    case T of
        [] -> % Se H for o último elemento da lista, não adiciona o \n
            Content = [Username, ",", Password, ",", Nivel];
        _ -> % Se H não for o último elemento da lista, adiciona o \n
            Content = [Username, ",", Password, ",", Nivel, "\n"]
    end,
    Content1 = lists:flatten(Content), % junta todos os elementos da lista em uma só
    io:fwrite("Content: ~p~n", [Content1]), %debug
    map_formater(T, [Data | Content1]).


read_content(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            % eliminar os \r e \n do conteudo lido e criar uma lista com as linhas
            Data = binary_to_list(Content), % Converte o conteudo lido para uma lista
            Data1 = string:tokens(Data, "\r"), % Separa o conteudo pelos \r e cria uma lista com os elementos
            Data2 = lists:flatten(Data1), % junta todos os elementos da lista em uma só
            io:fwrite("Data1: ~p~n", [Data1]), %debug
            Data3 = string:tokens(Data2, "\n"), % Separa o conteudo pelos \n e cria uma lista com os elementos
            case Data3 of
                [] -> % Caso ficheiro lido esteja vazio cria map vazio
                    #{};
                _ -> % Caso contrário, chama a função parser e cria um map com os dados dos utilizadores
                    parser(Data3, #{}) % #{Username => {Password, Nivel}}
            end;
        {error, Reason} ->
            io:fwrite("Error reading file: ~p~n", [Reason]),
            #{}
    end.


% Cria um mapa com os dados dos utilizadores
% Formato: Username => {Password, Nivel}
parser([], Data) -> Data;
parser([H | T], Data) ->
    
    [Username, Password, Level] = string:tokens(H, ","),
    io:fwrite("Username: ~p, Password: ~p, Level: ~p~n", [Username, Password, Level]),
    Data1 = maps:put(Username, {Password, Level}, Data),
    parser(T, Data1).
