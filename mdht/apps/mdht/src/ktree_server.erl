%%% @doc Gen-server for serving request on a single node.
%% It is assumed that there will be only one KTree structure,
%% so it makes sense to do it as a local named instance.
%%% @end

-module(ktree_server).
-behaviour(gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
%% For ktree record
-include("ktree_shared_records.hrl").

%% behaviour exports
-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2
	]).

%% api
-export([
	 start_link/0,
	 stop/0,
	 get_node/1,
	 start_ktree/1,
	 notify_pinged/1
	]).


%% Server will get PublicKey and create ktree from it.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_node(PK) ->
    gen_server:call(?MODULE, {get_node, PK}).

start_ktree(PK) ->
    gen_server:cast(?MODULE, {start_ktree, PK}).

notify_pinged(PackedNode) ->
    gen_server:cast(?MODULE, {notify_pinged, PackedNode}).

%% Gen server callbacks
%% Initialize new ets that will be out 
init([]) ->
    {ok, {}}.

handle_call({get_node, PK}, _From, KTree) ->
    GetNodeRes = ktree:get_node(KTree, PK),
    {reply, GetNodeRes, KTree};
handle_call({try_add, PackedNode}, _From, KTree) ->
    GetTryAdd = ktree:try_add(KTree, PackedNode),
    {reply, GetTryAdd, KTree}.

handle_cast({start_ktree, PK}, _) ->
    ?MODULE = ets:new(?MODULE, [
				ordered_set, 
				named_table, 
				protected,
				{keypos, #ind_kbucket.index}
			       ]),
    %% creating new ktree struc that will be the state
    NewKTree = ktree:new_ktree(PK, ?MODULE),
    {noreply, NewKTree};
handle_cast({notify_pinged, PackedNode}, KTree) ->
    PK = packed_node:get_pk(PackedNode),
    io:format("Start pinging a node ~n"),
    case ktree:ping_node(KTree, PK) of
        false ->
	    io:format("Notify pinged node can't be found, trying to add it ~n"),
	    ktree:try_add(KTree, PackedNode),
	    {noreply, KTree};
	true ->
	    io:format("Pinged successfuly updated"),
	    {noreply, KTree}
    end.
	    
	   

handle_info(_, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.






