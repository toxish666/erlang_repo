-module(kvdistrib_serv).

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-export([
	 start_link/0,
	 store_local/1,
	 store_replicated/1,
	 get_local/1,
	 get_replicated/1,
	 get_with_fun_local/1,
	 get_with_fun_replicated/1
	]).


%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


store_local({Key, Value}) ->
    gen_server:call(?MODULE, {store_local, Key, Value}).


store_replicated({Key, Value}) ->
    gen_server:call(?MODULE, {store_replicated, Key, Value}).


get_local(Key) ->
    gen_server:call(?MODULE, {get_local, Key}).


get_replicated(Key) ->
    gen_server:call(?MODULE, {get_replicated, Key}).


get_with_fun_local(MatchSpec) ->
    gen_server:call(?MODULE, {get_with_fun_local, MatchSpec}).


get_with_fun_replicated(MatchSpec) ->
    gen_server:call(?MODULE, {get_with_fun_replicated, MatchSpec}).


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([]) ->
    ?MODULE = ets:new(?MODULE, [set, named_table, protected]),
    {ok, ?MODULE}.


handle_call({store_local, Key, Value}, _From, Tid) ->
    Res = update_local_tid({Key, Value}, Tid),
    {reply, Res, Tid};
handle_call({store_replicated, Key, Value}, _From, Tid) ->
    FullResList = distributed_routine({update, {Key, Value}}, Tid),
    IsAnyUpdate = lists:any(fun({_, R}) -> R == ok end, FullResList),
    case IsAnyUpdate of
	true ->
	    {reply, {ok, FullResList}, Tid};
	false ->
	    {reply, {error, FullResList}, Tid}
    end;
handle_call({get_local, Key}, _From, Tid) ->
    Res = find_local_tid(Key, Tid),
    {reply, Res, Tid};
handle_call({get_replicated, Key}, _From, Tid) ->
    FullResList = distributed_routine({get, Key}, Tid),
    IsAnyUpdate = lists:any(fun({_, {R, _}}) -> R == ok end, FullResList),
    case IsAnyUpdate of
	true ->
	    {reply, {ok, FullResList}, Tid};
	false ->
	    {reply, {error, FullResList}, Tid}
    end;
handle_call({get_with_fun_local, MatchSpec}, _From, Tid) ->
    Res = get_with_fun(MatchSpec, Tid),
    {reply, Res, Tid};
handle_call({get_with_fun_replicated, MatchSpec}, _From, Tid) ->
    FullResList = distributed_routine({spec, MatchSpec}, Tid),
    IsAnyUpdate = lists:any(fun({_, {R, _}}) -> R == ok end, FullResList),
    case IsAnyUpdate of
	true ->
	    {reply, {ok, FullResList}, Tid};
	false ->
	    {reply, {error, FullResList}, Tid}
    end.
    

handle_cast(_, Tid) ->
    {noreply, Tid}.


handle_info(ok = _Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
%% assert that application is up and running on another node
check_nodes_for_app(Nodes) ->
    AssertList = pmap(
    		   fun(Node) -> 
			   Res = rpc:block_call(
				   Node, 
				   application, 
				   loaded_applications,
				   []
				  ),
			   AppFind = lists:keyfind(kvdistrib, 1, Res),
			   case AppFind of
			       false ->
				   notfound;
			       Found when is_tuple(Found) ->
				   Node
			   end
		   end,
		   Nodes
		  ),
    lists:filter(fun(V) ->
			 case V of
			     notfound ->
				 false;
			     _ ->
				 true
			 end
		 end, AssertList).


update_local_tid({Key, Value}, Tid) ->
    MatchSpec = ets:fun2ms(fun({K,V}) when K==Key -> K end),
    case ets:select(Tid, MatchSpec) of
	[] -> % free to insert
	    ets:insert(Tid, {Key, Value}),
	    ok;
	_ ->
	    {error, key_taken}
    end.


find_local_tid(Key, Tid) ->
    case ets:lookup(Tid, Key) of
	[{Key,Value}] ->
	    {ok, Value};
	[] ->
	    {error, not_found}
    end.


get_with_fun(MatchSpec, Tid) ->
    case MatchSpec of
	{error, _} = Err ->
	    Err;
	_ ->
	  case ets:select(Tid, MatchSpec) of
	      [] ->
		  {error, fun_spec_not_found};
	      Value ->
		  {ok, Value}
	  end
    end.


%% StoreOrUpdateAtom -- get|update|spec
%% V -- Key|{Key, Value}|Spec
distributed_routine({StoreOrUpdateAtom, V}, Tid) ->
    %% Res --> make local routine
    case StoreOrUpdateAtom of
	get ->
	    Res = find_local_tid(V, Tid),
	    FunAtom = get_local;
	update ->	 
	    Res = update_local_tid(V, Tid),
	    FunAtom = store_local;
	spec ->
	    Res = get_with_fun(V, Tid),
	    FunAtom = get_with_fun_local	    
    end,
    %% updates all nodes in cluster if there's any
    %% AsyncFun --> async call to all Nodes of particular kind
    AsyncFun = fun(Nodes) ->
		       lists:map(fun({ok, Node}) ->
					 {Node, rpc:async_call(
						  Node, 
						  ?MODULE, 
						  FunAtom,
						  [V]
						 )}
				 end, Nodes)	 
	       end,
    AllOtherNodes = nodes(),
    NodesWithApp = check_nodes_for_app(AllOtherNodes),
    AwaitingList = AsyncFun(NodesWithApp),
    ResList = lists:map(fun({N, K}) ->
				Re = rpc:yield(K),
				{N, Re}
			end, AwaitingList),
    [{node(), Res} | ResList].


pmap(F, Es) ->
    Parent = self(),
    Running = [spawn_monitor(fun() -> Parent ! {self(),  F(E)} end) || E <- Es],
    collect(Running, 5000).
    

collect([], _Timeout) -> [];
collect([{Pid, MRef} | Next], Timeout) ->
    receive
        {Pid, Res} ->
           erlang:demonitor(MRef, [flush]),
           [{ok, Res} | collect(Next, Timeout)];
        {'DOWN', MRef, process, Pid, Reason} ->
           [{error, Reason} | collect(Next, Timeout)]
    after Timeout ->
        exit(pmap_timeout)
    end.
