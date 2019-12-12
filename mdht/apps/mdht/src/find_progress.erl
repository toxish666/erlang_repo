-module(find_progress).
-behaviour(gen_event).

-export([init/1, 
	 handle_event/2, 
	 handle_call/2, 
	 handle_info/2, 
	 code_change/3,
         terminate/2]).

-record(state, {
		key_to_find,
		from,
		ref
	       }).

init([KeyToFind, From, Ref]) ->
    {ok, #state{key_to_find = KeyToFind, from = From, ref = Ref}}.

handle_event({nodes_response, SenderPublicKey, PackedNodes},
	     #state{key_to_find = KeyToFind, ref = Ref, from = From} = State) ->
    %% when sender's public key is the closest key to key_to_find
    %% it makes no sense to search any longer
    io:format("PACKED NODES ARE ~p~n", [PackedNodes]),
    FilteredNodes = 
	lists:filter(fun(PackedNode) ->
			     case kbucket:distance_impl(KeyToFind, SenderPublicKey,
							packed_node:get_pk(PackedNode)) of
				 less ->
				     false;
				 greater ->
				     true;
				 equal ->
				     true
			     end
		     end, PackedNodes),
    io:format("FILTERED NODES ~p~n", [FilteredNodes]),
    IsFound = lists:search(fun(PackedN) -> packed_node:get_pk(PackedN) == KeyToFind end,
			   FilteredNodes),
    io:format("IS FOUND ~p~n", [IsFound]),
    case IsFound of
	false ->
	    NodesTo = trim_nodes(FilteredNodes),
	    lists:foreach(fun(N) ->
				  NMDHT = mdht_node:new_mdht_node(N),
				  network_server:get_closest(NMDHT, KeyToFind, {Ref}) end, NodesTo);
	{value, Value} ->
	    io:format("SHOULD FIND A VALUE ~n"),
	    mdht_server:finalize_find(From, Value, Ref)
    end,  
    {ok, State}.

handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%% handlers
trim_nodes(FilteredNodes) ->
    NumberOfNodesTo = mdht_server:number_of_nodes_to_propagate(),
    FilteredNodesLength = length(FilteredNodes),
    OnlyNNodes = if FilteredNodesLength == NumberOfNodesTo orelse FilteredNodesLength <  NumberOfNodesTo ->
			 FilteredNodes;
		    true ->
			 {Res, _} = lists:split(NumberOfNodesTo, FilteredNodes),
			 Res
		 end,
    OnlyNNodes.
    
    

