-module(network_server).
-include("common_records.hrl").

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
	 ping/1,
	 get_closest/2
	]).

-record(state, {
		socket :: any(),
		waiting_queries :: #{
				     {RequestId :: non_neg_integer()}
				     => 
				     {packed_node:packed_node(),
				      SentMessage :: any()}
				    }
	       }).

-define(QUERY_TIMEOUT, 2000).


start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

%% @doc Ping node 
ping(MDhtNode) ->
    docommand_async(MDhtNode, ping_request).

%% @doc Get closest nodes to some given key
get_closest(MDhtNode, PKIntrested) ->   
    docommand_async(MDhtNode, {get_closest, PKIntrested}).

%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server callbacks
init([Port]) ->
    {ok, Socket} = gen_udp:open(Port, [binary, inet, {active, true}]),
    {ok, #state{socket = Socket, waiting_queries = #{}}}.
    
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({get_state}, _From, #state{waiting_queries = WaitingQueries} = State) ->
    {reply, {ok, WaitingQueries}, State}.

handle_cast({docommand, Target, ping_request}, State) ->
    RequestInner = ping_request,
    send_query_gen(Target, RequestInner, State);
handle_cast({docommand, Target, {nodes_request, _PublicKey} = ReqI}, State) ->
    RequestInner = ReqI,
    send_query_gen(Target, RequestInner, State).
		   

%% Message from timer
handle_info({check_query_exp, RequestId}, #state{waiting_queries = WaitingQueries} = State) ->
    Key = {RequestId},
    case maps:get(Key, WaitingQueries, none) of
	none ->
	    %% if there is no record in waiting query map, so cool it's get handled
	    {noreply, State};
	{_PackedNode, _InnerRequest} ->
	    io:format("got timeout ~n"),
	    %% there is no ping answer yet so throw it from waiting queries
	    WaitingQueriesThrown = maps:remove(Key, WaitingQueries),
	    {noreply, State#state{waiting_queries = WaitingQueriesThrown}}
    end;
%% udp connection messages
handle_info({udp, Socket, IP, Port, Packet}, #state{waiting_queries = WaitingQueries} = State) ->
    io:format("got received message ~n"),
    case mdht_proto:decode(Packet) of
	{error, _} ->
	    {noreply, State};
	%% if the ping response has came -- there should be the ping request
	{ok, _SenderPublicKey, {ping_response, RequestId}} ->
	    io:format("got ping response ~n"),
	    KeysList = maps:keys(WaitingQueries),
	    case lists:keyfind(RequestId, 1, KeysList) of
		%% if there is no value found, than it's probably timeouted
		%% so ignore it
		false ->
		    logger:debug("Ignored ping response");
		%% found it
		FoundKey ->
		    %% notify mdht_server that node was pinged and returned res
		    {PackedNode, _} = maps:get(FoundKey, WaitingQueries),
		    mdht_server:notify_pinged(PackedNode),
		    WaitingQueriesThrown = maps:remove(FoundKey, WaitingQueries),
		    {noreply, State#state{waiting_queries = WaitingQueriesThrown}}
	    end;
	%% if it's a ping request -- send ping response
	{ok, SenderPublicKey, {ping_request, RequestId}} ->
	    io:format("ping request has come"),
	    %% we may receive pind request from unknown node
	    %% so we need to check if this is one of the closest nodes
	    %% let mdht_server handle this case 
	    %%
	    %% there might be the case when this ping request has came
	    %% from absolutely new node and when the ktree is full
	    %% server should propagate this node further through the net
	    mdht_server:check_from_ping(SenderPublicKey, IP, Port),
	    %% assemble response message
	    InnerMessage = ping_response,
	    case assemble_message(SenderPublicKey, RequestId, InnerMessage) of
		{error, ErrReason} ->
		    logger:debug("Wrong ping response assembled with err ~p", [ErrReason]),
		    {noreply, State};
		Binary when is_binary(Binary) ->
		    logger:debug("Send ping response"),
		    gen_udp:send(Socket, IP, Port, Binary),
		    {noreply, State}
	    end
    end;
handle_info(UnknownMsg, State) ->
    io:format("UNKNOWN MESSAGE ~p~n", [UnknownMsg]),
    {noreply, State}.

	

%% helpers 
%% do some command asyncly
docommand_async(Target, Comm) ->
    gen_server:cast(?MODULE, {docommand, Target, Comm}).

%% send to a target service packet
send_query_gen(Target, RequestInner, #state{waiting_queries = WaitingQueries, socket = Socket} = State) ->
    {ok, RequestId} = encryption_server:get_request_id(),
    %% assuming Target is in mdht_node format here
    TargetPK = mdht_node:get_pk(Target),
    TargetAddress = mdht_node:get_socket_addr(Target),
    {IP, Port} = extract_ip_port(TargetAddress),
    EncodeRes = mdht_proto:encode({RequestInner, RequestId}, TargetPK),
    case gen_udp:send(Socket, IP, Port, EncodeRes) of
	ok ->
	    _TimerRef = time:send_after(?QUERY_TIMEOUT, ?MODULE, {check_query_exp, RequestId}),
	    PackedNodeTarget = mdht_node:to_packed_node(Target),
	    {noreply, State#state{waiting_queries = 
				      WaitingQueries#{
						      {RequestId} 
						      => {PackedNodeTarget, RequestInner}
						     }
				 }
	    };
	_ ->
	    {noreply, State}
    end.	

%% create packet to answer to the message
assemble_message(SenderPublicKey, RequestId, InnerMessage) ->
    mdht_proto:encode({InnerMessage ,RequestId}, SenderPublicKey).
        
extract_ip_port(TargetAddress) ->
    case TargetAddress of
	#socket{socket_pre_info = SocketPreInfo} ->
	    Port = SocketPreInfo#socket_pre_info.port,
	    IP = case SocketPreInfo#socket_pre_info.ip4_address of
		     undefined ->
			 SocketPreInfo#socket_pre_info.ip6_address;
		     IPV4 ->
			 IPV4
		 end,
	    {list_to_tuple(binary:bin_to_list(IP)), list_to_integer(Port)};
	_ ->
	    {none,none}
    end.
