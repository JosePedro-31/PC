-module (file_options).

-export ([write_content/2, read_content/1]).

write_content(Content, Filename) ->
    file:write_file(Filename, Content).


read_content(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            io:fwrite("File content: ~p~n", [Content]);
        {error, Reason} ->
            io:fwrite("Error reading file: ~p~n", [Reason])
    end.