-module(duel_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% teste

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port = 12345, % Definir porta
    Opts = [binary, {packet, 0}, {active, true}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, ListenSocket} ->
            io:format("Server listening on port ~p~n", [Port]),
            {ok, #{listen_socket => ListenSocket}};
        {error, Reason} ->
            io:format("Failed to listen: ~p~n", [Reason]),
            {stop, Reason}
    end.

% Aceitar novas conexões (simplificado - precisa de mais lógica)
handle_info({tcp_accepted, ListenSocket, ClientSocket}, State = #{listen_socket := ListenSocket}) ->
     io:format("Client connected: ~p~n", [ClientSocket]),
     gen_tcp:send(ClientSocket, <<"Hello from Erlang!">>),
     inet:setopts(ClientSocket, [{active, true}]), % Esperar por dados
    {noreply, State}; % Deveria iniciar um processo para o cliente

handle_info({tcp, Socket, Data}, State) ->
    io:format("Received from ~p: ~s~n", [Socket, Data]),
    % Aqui processaria a mensagem do cliente
    gen_tcp:send(Socket, <<"Got it!">>),
    {noreply, State};

handle_info({tcp_closed, Socket}, State) ->
     io:format("Client disconnected: ~p~n", [Socket]),
     % Limpar estado associado ao Socket
     {noreply, State};

% Outras callbacks gen_server (handle_call, handle_cast, etc.)
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.