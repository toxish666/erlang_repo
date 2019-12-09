-module(mdht_server).
-behaviour(gen_server).

%% behaviour
-export([
	 init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2
	]).

%% public api
%-export([
%	 start_link/0
%	]).

-define(ENCRYPTION_SUP, encryption_sup).
-define(SPEC_ENCRYPTION_SUP,
        #{
	  id => ?ENCRYPTION_SUP,
	  start => {?ENCRYPTION_SUP, start_link, [self()]},
	  restart => permanent,
	  shutdown => 3000,
	  type => supervisor,
	  modules => [?ENCRYPTION_SUP]
	}
       ).

-record(server, {
		 %% own secret key
		 sk,
		 %% own public key
		 pk,
		 %% close nodes list which contains nodes close to own pk
		 close_nodes
		}).


init(ServSuperVisor) ->
    self() ! {start_atp_supervisor, ServSuperVisor},
    {ok, #server{}}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	    
handle_call({get_pk}, _From, #server{pk = PK} = State) ->
    {reply, {ok, PK}, State};
handle_call({get_sk}, _From, #server{sk = SK} = State) ->
    {reply, {ok, SK}, State}.


handle_cast(Msg, State) ->
    {noreply, State}.
    

handle_info({start_atp_supervisor, Sup}, State) ->
    case supervisor:start_child(Sup, ?SPEC_ENCRYPTION_SUP) of
	{ok, _} ->
	    {noreply, State};
	{error, {already_started, AtmSup}} ->
	    ok = supervisor:terminate_child(Sup, AtmSup),
	    self() ! {start_atp_supervisor, Sup}
    end;
handle_info(Msg, State) ->
    logger:debug("Unexpected message in mdht_server: ~p", [Msg]),
    {noreply, State}.
