-module(lockman_listen_serv).

-behaviour(gen_server).

-export([start_link/1]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-define(LOCKMAN_SERV, lockman_serv).


start_link(Socket) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Socket, []).


init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, Socket}.


handle_call(_Msg, _From, S) ->
    {noreply, S}.


handle_cast(accept, Socket) ->
    {ok, AcceptedSocket} = gen_tcp:accept(Socket),
    gen_tcp:controlling_process(AcceptedSocket, whereis(?LOCKMAN_SERV)),
    gen_server:cast(?LOCKMAN_SERV, {accepted_sock, AcceptedSocket}),
    gen_server:cast(self(), accept),
    {noreply, Socket}.


handle_info(_Event, State) ->
    {noreply, State}.  


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.
    
