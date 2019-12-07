%%% @doc Data structure used by KBucket.
%% PackedNode contains PK and SocketAddress.
%% PackedNode does not contain status of Node, this struct contains status of node.
%% Bucket needs status of node, because BAD status node should be replaced with higher proirity than GOOD node.
%% Here, GOOD node is the node responded within 160 seconds, BAD node is the node not responded over 160 seconds.
%%% @end

-module(mdht_node).
-include("common_records.hrl").

%% sock_and_time manipulations
-export([
	 new_sat/1,
	 is_bad_sat/1,
	 is_discarded_sat/1,
	 is_ping_interval_passed/1,
	 ping_addr/1
	]).

%% dht_node manipulations
-export([
	 new_mdht_node/1,
	 is_bad_mnode/1,
	 is_discarded_mnode/1,
	 get_socket_addr/1,
	 get_pk/1,
	 to_packed_node/1
	]).

%% Ping interval for each node in our lists.
-define(PING_INTERVAL_SEC, 60).

%% Interval of time for a non responsive node to become bad.
-define(BAD_NODE_TIMEOUT, ?PING_INTERVAL_SEC * 2).

%% The timeout after which a node is discarded completely.
-define(KILL_NODE_TIMEOUT, ?BAD_NODE_TIMEOUT + ?PING_INTERVAL_SEC).


%% Struct conatains SocketAddrs and timestamps for sending and receiving packet.
-record (sock_and_time, {
			 saddr :: mdht:option(socket()), % Socket addr of node
			 last_resp_time :: mdht:instant(), % Last received ping/nodes-response time
			 last_ping_req_time :: mdht:option(mdht:instant()), % Last sent ping-req time 
			 ret_saddr :: mdht:option(socket()), % Returned by this node
			 ret_last_resp_time :: mdht:option(mdht:instant()) % Last time for receiving returned packet
			}).
-type sock_and_time() :: #sock_and_time{}.


%%  Struct used by kbucket, DHT maintains close node list, when we got new node,
%% we should make decision to add new node to close node list, or not.
%% The decision is made based on PK's distance and status of node.
-record (mdht_node, {
		    assoc4 :: sock_and_time(), % Socket addr and times of node for IPv4.
		    assoc6 :: sock_and_time(), % Socket addr and times of node for IPv6.
		    pk :: mdht:public_key() % Public Key of the node.
		   }).
-type mdht_node() :: #mdht_node{}.


%%-----------------------------sock_and_time------------------------------
%% @doc Create SockAndTime object.
-spec new_sat(mdht:option(mdht:socket())) -> sock_and_time().
new_sat(SAddr) ->
    LastRespTime = case SAddr of
		       none -> 
			   none;
		       _ -> 
			   time:clock_now()
		   end,
    #sock_and_time{
       saddr = SAddr,
       last_resp_time = LastRespTime,
       last_ping_req_time = none,
       ret_saddr = none,
       ret_last_resp_time = none
      }.
    

%% @doc Check if the address is considered bad i.e. it does not answer on
%% addresses for `BAD_NODE_TIMEOUT`.
%% @end
-spec is_bad_sat(sock_and_time()) -> boolean().
is_bad_sat(SockAndTime) ->
    LastRespTime = SockAndTime#sock_and_time.last_resp_time,
    utils:map_opt_or_default(
      LastRespTime,
      fun(Inst) -> time:clock_elapsed(Inst) > ?BAD_NODE_TIMEOUT end,
      true
     ).
 

%% @doc Check if the address is considered discarded i.e. it does not answer on
%% addresses for `KILL_NODE_TIMEOUT`.
%% @end
-spec is_discarded_sat(sock_and_time()) -> boolean().
is_discarded_sat(SockAndTime) ->
    LastRespTime = SockAndTime#sock_and_time.last_resp_time,
    utils:map_opt_or_default(
      LastRespTime,
      fun(Inst) -> time:clock_elapsed(Inst) > ?KILL_NODE_TIMEOUT end,
      true
     ).
  

%% @doc Check if `PING_INTERVAL` is passed after last ping request.
-spec is_ping_interval_passed(sock_and_time()) -> boolean().
is_ping_interval_passed(SockAndTime) ->
    LastPingReqTime = SockAndTime#sock_and_time.last_ping_req_time,
    utils:map_opt_or_default(
      LastPingReqTime,
      fun(Inst) -> time:clock_elapsed(Inst) >= ?PING_INTERVAL_SEC end,
      true
     ).

%% @doc Get address if it should be pinged and update `last_ping_req_time`.
-spec ping_addr(sock_and_time()) -> sock_and_time().
ping_addr(SockAndTime) ->
    SAddr = SockAndTime#sock_and_time.saddr,
    utils:map_opt_or_default(
      SAddr,
      fun(_) ->
	      case {is_discarded_sat(SockAndTime), is_ping_interval_passed(SockAndTime)} of
		  {false, true} ->
		      SockAndTime#sock_and_time{last_ping_req_time = time:clock_now()};
		  _ ->
		      none
	      end
      end,
      none
     ).

%%-----------------------------mdht_node--------------------------------
%% @doc Create new mdht node with given socket and pk.
-spec new_mdht_node(packed_node:packed_node()) -> mdht_node().
new_mdht_node(PackedNode) ->    
    PK = packed_node:get_pk(PackedNode),
    Sock = packed_node:get_saddr(PackedNode),
    SockPreInfo = Sock#socket.socket_pre_info,
    {SAddrV4, SAddrV6} = case {SockPreInfo#socket_pre_info.ip4_address,
			       SockPreInfo#socket_pre_info.ip6_address} of
			     {undefined, undefined} ->
				 throw(mdht_node_incorrect);
			     {IpV4, undefined} ->
				 {IpV4, none};
			     {undefined, IpV6} ->
				 {none, IpV6}
			 end,
    #mdht_node{
       assoc4 = new_sat(SAddrV4),
       assoc6 = new_sat(SAddrV6),
       pk = PK
      }.


%% @doc Check if the node is considered bad (it does not answer both on all addresses for BAD_NODE_TIMEOUT sec).
-spec is_bad_mnode(mdht_node()) -> boolean().
is_bad_mnode(MNode) ->
    is_bad_sat(MNode#mdht_node.assoc4) and is_bad_sat(MNode#mdht_node.assoc6).
	
    
%% @doc Check if the node is considered discarded (it does not answer both on all addresses for KILL_NODE_TIMEOUT sec).
-spec is_discarded_mnode(mdht_node()) -> boolean().
is_discarded_mnode(MNode) ->
    is_discarded_sat(MNode#mdht_node.assoc4) and is_discarded_sat(MNode#mdht_node.assoc6).


%% @doc Return socket for mdht node based on last response time
-spec get_socket_addr(mdht_node()) -> mdht:option(socket()).
get_socket_addr(MNode) ->
    Assoc4 = MNode#mdht_node.assoc4,
    Assoc6 = MNode#mdht_node.assoc6,
    Assoc4LastRespTime = Assoc4#sock_and_time.last_resp_time,
    Assoc6LastRespTime = Assoc6#sock_and_time.last_resp_time,
    if Assoc4LastRespTime >= Assoc6LastRespTime ->
	    Assoc4#sock_and_time.saddr;
       true ->
	    Assoc6#sock_and_time.saddr
    end.


%% @doc Getter for extracting pk out of a mdht node
-spec get_pk(mdht_node()) -> mdht:public_key().
get_pk(MNode) ->
    MNode#mdht_node.pk.
    

%% @doc mdht_node to packed_node
-spec to_packed_node(mdht_node()) -> mdht:option(packed_node:packed_node()).
to_packed_node(MNode) ->
    PK = MNode#mdht_node.pk,
    case get_socket_addr(MNode) of 
	none ->
	    none;
	Sock ->
	    packed_node:new_packed_node(Sock, PK)
    end.
	    
