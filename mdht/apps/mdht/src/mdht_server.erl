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
	 start_link/1,
	 find/1
	]).

%% private functions 
-export([
	 notify_pinged/1,
	 check_from_ping/3,
	 notify_nodes_responded/3,
	 number_of_nodes_to_propagate/0,
	 finalize_find/3
	]).

-define(FINDTIMEOUT, 5000).
-define(FINDNODEPROPAGATION, 2).
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
		 supervisor_pid,
		 %% gen events pid to handle find requests in network
		 event_pids = #{} :: #{reference() => pid()}
		}).

%% api
start_link(SupervisorPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [SupervisorPid], []).

find(PublicKeyToFind) ->
    %% spawn handler manager for listening the progress of finding process
    gen_server:call(?MODULE, {init_find_proc, PublicKeyToFind}, ?FINDTIMEOUT).
    

%% private functions
notify_pinged(PackedNode) ->
    gen_server:cast(?MODULE, {notify_pinged, PackedNode}),
    ok.

check_from_ping(PublicKey, IP, Port) ->
    PackedNode = case binary:list_to_bin(tuple_to_list(IP)) of
		     IPB when size(IPB) == 4 ->
			 packed_node:create_node_packed_ipv4(integer_to_list(Port),
							     IPB, PublicKey);
		     IPB when size(IPB) == 8 ->
			 packed_node:create_node_packed_ipv6(integer_to_list(Port),
							     IPB, PublicKey)
		 end,
    gen_server:cast(?MODULE, {notify_pinged, PackedNode}),
    ok.

notify_nodes_responded(SenderPublicKey, PackedNodes, ManagerId) ->
    gen_server:cast(?MODULE, {notify_nodes_responded, SenderPublicKey,
			     PackedNodes, ManagerId}),
    ok.

finalize_find(From, Answer, ManagerRef) ->
    gen_server:cast(?MODULE, {answer_find, From, Answer, ManagerRef}).

number_of_nodes_to_propagate() ->
    ?FINDNODEPROPAGATION.

%%%%%%%
%% behaviour
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
    {reply, {ok, SK}, State};
handle_call({init_find_proc, PublicKeyToFind}, From, #server{event_pids = EventPids} = State) ->
    {ManagerPid, Ref} = find_manager:start_link({PublicKeyToFind, make_ref()}, From),
    %% get N local known nodes closest to given PK
    ClosestNodes = ktree_server:get_closest(PublicKeyToFind),
    ClosestNodesDefine = trim_nodes(ClosestNodes),
    %% send get_closest request to these nodes
    case ClosestNodesDefine of
	[] ->
	    {reply, {error, no_close_nodes}, State};
	_ ->
	    lists:foreach(fun(N) -> 
				  NMDHT = mdht_node:new_mdht_node(N),
				  network_server:get_closest(NMDHT, 
							     PublicKeyToFind, 
							     {Ref}) 
			  end, ClosestNodesDefine),
	    {noreply, State#server{event_pids = EventPids#{Ref => ManagerPid}}}
    end.


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
    {noreply, State};
%% notify find handler about the progress
handle_cast({notify_nodes_responded, SenderPublicKey, PackedNodes, ManagerRef},
	   #server{event_pids = EventPids} = State) ->
    case maps:get(ManagerRef, EventPids, none) of
	none ->
	    ignore;
	ManagerPid ->
	    find_manager:nodes_response(ManagerPid, SenderPublicKey, PackedNodes)	    
    end,
    {noreply, State};
%% event handler finished it's work; send result, remove handler
handle_cast({answer_find, From, Result, Ref}, #server{event_pids = EventPids} = State) ->
    gen_server:reply(From, {find_result, Result}),
    HandlerPid = maps:get(Ref, EventPids),
    gen_event:stop(HandlerPid),
    {noreply, State#server{event_pids = maps:remove(Ref, EventPids)}}.

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



%% handlers
trim_nodes(FilteredNodes) ->
    NumberOfNodesTo = ?FINDNODEPROPAGATION,
    FilteredNodesLength = length(FilteredNodes),
    OnlyNNodes = if FilteredNodesLength == NumberOfNodesTo orelse FilteredNodesLength <  NumberOfNodesTo ->
			 FilteredNodes;
		    true ->
			 {Res, _} = lists:split(NumberOfNodesTo, FilteredNodes),
			 Res
		 end,
    OnlyNNodes.
