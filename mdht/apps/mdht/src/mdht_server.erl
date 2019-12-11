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
-export([
	 start_link/1
	]).

%% private functions 
-export([
	 notify_pinged/1,
	 check_from_ping/3
	]).

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
		 close_nodes,
		 %% supervisor
		 supervisor_pid
		}).

start_link(SupervisorPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SupervisorPid], []).

%% private functions
notify_pinged(PackedNode) ->
    gen_server:cast(?MODULE, {notify_pinged, PackedNode}),
    ok.

check_from_ping(PublicKey, IP, Port) ->
    ok.

init([ServSuperVisor]) ->
    self() ! {start_encryption_supervisor, ServSuperVisor},
    {ok, #server{supervisor_pid = ServSuperVisor}}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
	    
handle_call({get_pk}, _From, #server{pk = PK} = State) ->
    {reply, {ok, PK}, State};
handle_call({get_sk}, _From, #server{sk = SK} = State) ->
    {reply, {ok, SK}, State}.

%% this comes right after initiation of encryption_server
handle_cast({assigned_keys, PK, SK}, #server{sk = SKA, pk = PKA} = State) ->
    case {SKA, PKA} of 
	{undefined, undefined} ->
	    ktree_server:start_ktree(PK),
	    {noreply, State#server{sk = SK, pk = PK}};
	_ ->
	    {noreply, State}
    end;
%% node were pinged and it sent response back
handle_cast({notify_pinged, PackedNode}, State) ->
    ktree_server:notify_pinged(PackedNode),
    {noreply, State}.

    
handle_info({start_encryption_supervisor, Sup}, State) ->
    case supervisor:start_child(Sup, ?SPEC_ENCRYPTION_SUP) of
	{ok, _} ->
	    {noreply, State};
	{error, {already_started, AtmSup}} ->
	    ok = supervisor:terminate_child(Sup, AtmSup),
	    self() ! {start_encryption_supervisor, Sup}
    end;
handle_info(Msg, State) ->
    logger:debug("Unexpected message in mdht_server: ~p", [Msg]),
    {noreply, State}.
