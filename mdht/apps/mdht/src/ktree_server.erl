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
	 start_link/1,
	 stop/0,
	 get_node/1
	]).


%% Server will get PublicKey and create ktree from it.
start_link(PublicKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PublicKey], []).

stop() ->
    gen_server:call(?MODULE, stop).

get_node(PK) ->
    gen_server:call(?MODULE, {get_node, PK}).


%% Gen server callbacks
%% Initialize new ets that will be out 
init(PublicKey) ->
    ?MODULE = ets:new(?MODULE, [
				ordered_set, 
				named_table, 
				protected,
				{keypos, #ind_kbucket.index}
			       ]),
    %% creating new ktree struc that will be the state
    NewKTree = ktree:new_ktree(PublicKey, ?MODULE),
    {ok, NewKTree}.


handle_call({get_node, PK}, _From, KTree) ->
    GetNodeRes = ktree:get_node(KTree, PK),
    {reply, GetNodeRes, KTree};
handle_call({try_add, PackedNode}, _From, KTree) ->
    GetTryAdd = ktree:try_add(KTree, PackedNode),
    {reply, GetTryAdd, KTree}.



handle_cast(_, State) ->
    {noreply, State}.


handle_info(_, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.






