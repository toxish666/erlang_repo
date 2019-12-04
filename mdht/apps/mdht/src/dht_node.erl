%%% @doc Data structure used by KBucket.
%% PackedNode contains PK and SocketAddress.
%% PackedNode does not contain status of Node, this struct contains status of node.
%% Bucket needs status of node, because BAD status node should be replaced with higher proirity than GOOD node.
%% Here, GOOD node is the node responded within 162 seconds, BAD node is the node not responded over 162 seconds.
%%% @end

-module(dht_node).

%% sock_and_time manipulations
-export([
	 new/1,
	 is_bad/1,
	 is_discarded/1,
	 is_ping_interval_passed/1,
	 ping_addr/1
	]).

%% Ping interval for each node in our lists.
-define(PING_INTERVAL_SEC, 60).

%% Interval of time for a non responsive node to become bad.
-define(BAD_NODE_TIMEOUT, ?PING_INTERVAL_SEC * 2 + 2).

%% The timeout after which a node is discarded completely.
-define(KILL_NODE_TIMEOUT, ?BAD_NODE_TIMEOUT + ?PING_INTERVAL_SEC).


%% Struct simulating socket info
-record (socket, {
		  socket_preinfo :: atom(), %% socket info while it's in closed form
		  socket_opened = undefined :: atom() %% socket in it's opened form
		 }).
-type socket() :: #socket{}.


%% Struct conatains SocketAddrs and timestamps for sending and receiving packet;
-record (sock_and_time, {
			 saddr :: mdht:option(socket()), % Socket addr of node
			 last_resp_time :: mdht:instant(), % Last received ping/nodes-response time
			 last_ping_req_time :: mdht:option(mdht:instant()), % Last sent ping-req time 
			 ret_saddr :: mdht:option(socket()), % Returned by this node
			 ret_last_resp_time :: mdht:option(mdht:instant()) % Last time for receiving returned packet
			}).
-type sock_and_time() :: #sock_and_time{}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                             PUBLIC                                 %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%% @doc Create SockAndTime object.
-spec new(mdht:option(mdht:socket())) -> sock_and_time().
new(SAddr) ->
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
-spec is_bad(sock_and_time()) -> boolean().
is_bad(SockAndTime) ->
    LastRespTime = SockAndTime#sock_and_time.last_resp_time,
    time:clock_elapsed(LastRespTime) > ?BAD_NODE_TIMEOUT.
 

%% @doc Check if the address is considered discarded i.e. it does not answer on
%% addresses for `KILL_NODE_TIMEOUT`.
%% @end
-spec is_discarded(sock_and_time()) -> boolean().
is_discarded(SockAndTime) ->
    LastRespTime = SockAndTime#sock_and_time.last_resp_time,
    time:clock_elapsed(LastRespTime) > ?KILL_NODE_TIMEOUT.
    

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
	      case {is_discarded(SockAndTime), is_ping_interval_passed(SockAndTime)} of
		  {false, true} ->
		      SockAndTime#sock_and_time{last_ping_req_time = time:clock_now()};
		  _ ->
		      none
	      end
      end,
      none
     ).
      






