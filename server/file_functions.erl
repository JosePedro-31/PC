-module (file_functions).

-export ([write_content/2, read_content/1]).


write_content(Filename, Content) ->
    Content1 = maps:to_list(Content), % Converte o mapa numa lista de tuplos [{Key, Value}]
    Data = "", % Cria uma lista vazia para armazenar os dados formatados
    Data1 = map_formater(Content1, Data), % cria uma lista com as linhas a escrever no ficheiro
    Data2 = lists:flatten(Data1), % junta todos os elementos da lista num só
    file:write_file(Filename, Data2).


% Cria uma lista com as linhas a escrever no ficheiro
% Formato: Username,Password,Nivel,Sequencia\n
% nota: o \n não é adicionado à ultima linha 
map_formater([], Data) -> Data;
map_formater([H|T], Data) ->   
    {Username, PasswordNivelSequencia} = H, % H é um tuplo {Username, {Password, Nivel, WinLossSequence}}
    {Password, Nivel, WinLossSequence} = PasswordNivelSequencia, % PasswordNivel é um tuplo {Password, Nivel, WinLossSequence}
    case T of
        [] -> % Se H for o último elemento da lista, não adiciona o \n para não criar uma linha vazia no fim
            Content = [Username, ",", Password, ",", Nivel, ",", WinLossSequence];
        _ -> % Se H não for o último elemento da lista, adiciona o \n
            Content = [Username, ",", Password, ",", Nivel, ",", WinLossSequence, "\n"]
    end,
    Content1 = lists:flatten(Content), % junta todos os elementos da lista em uma só
    io:fwrite("Content: ~p~n", [Content1]), %debug
    map_formater(T, [Data | Content1]).


read_content(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            % eliminar os \r do conteudo lido e separar as linhas pelos \n para criar uma lista com as linhas
            Data = binary_to_list(Content), % Converte o conteudo lido de binario para lista
            Data1 = string:tokens(Data, "\r"), % cria uma lista com os elementos separados pelos \r 
            Data2 = lists:flatten(Data1), % junta todos os elementos da lista em um só (sem \r)
            Data3 = string:tokens(Data2, "\n"), % Separa o conteudo pelos \n e cria uma lista com as linhas
            case Data3 of
                [] -> % Caso ficheiro lido esteja vazio cria map vazio
                    #{};
                _ -> % Caso contrário, chama a função parser e cria um map com os dados dos utilizadores
                    parser(Data3, #{}) % #{Username => {Password, Nivel, serie de vitorias/derrotas}}
            end;
        {error, Reason} ->
            io:fwrite("Error reading file: ~p~n", [Reason]),
            #{}
    end.


% Cria um mapa com os dados dos utilizadores
% Formato: Username => {Password, Nivel, serie de vitorias/derrotas}
parser([], Data) -> Data;
parser([H | T], Data) ->
    
    [Username, Password, Level, WinLossSequence] = string:tokens(H, ","),
    Data1 = maps:put(Username, {Password, Level, WinLossSequence}, Data),
    parser(T, Data1).
