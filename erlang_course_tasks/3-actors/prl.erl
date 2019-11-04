-module(prl).

-export([map/4,
	 fold/3,
	 fold/4,
	 fold/5,
	 mapreduce/3]).


map(F, List, PartialResult, Timeout) ->
    Self = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun() -> Self ! {map_result, Ref, do_map(F, List, PartialResult, Timeout)} end),
    receive
        {map_result, Ref, Result} -> Result;
        {'EXIT', Pid, _} = Msg -> self() ! Msg, error
    after Timeout ->
	    exit(Pid, kill),
	    {error, timeout}
    end.

do_map(F, List, PartialResult, Timeout) ->
    Self = self(),
    process_flag(trap_exit, PartialResult),
    Pids = [ spawn_link(fun() ->
				Result = F(X),
				Self ! {result, self(), Result}
			end) || X <- List ],
    collect(Pids, Timeout).

collect([], _) ->
    [];
collect([NextPid|Tail], Timeout) ->
    Result = receive
		 {result, NextPid, X} -> X;
		 {'EXIT', NextPid, Reason} -> {error, Reason}
	     after Timeout -> {error, timeout}
	     end,
    [Result|collect(Tail, Timeout)].

%%--------------------------------------------------------------

fold(Fun, InitAcc, List) ->
    fold(Fun, Fun, InitAcc, List, 1).

fold(Fun, InitAcc, List, Malt) ->
    fold(Fun, Fun, InitAcc, List, Malt).

fold(Fun, Fuse, InitAcc, List, Malt) ->
    Fun2 = fun (L) -> lists:foldl(Fun, InitAcc, L) end,
    runfold(Fun2, Fuse, List, Malt).


runfold(Fun, Fuse, List, Granularity) when is_integer(Granularity) ->
    List2 = splitmany(List, Granularity),
    runfold(Fun, Fuse, List2).


runfold(Fun, Fuse, List) ->
    Parent = self (),
    Pids = lists:map(fun (L) ->
			     F = fun () ->
					 Parent !
					     {self (), Fun(L)}
				 end,
			     {Pid, _} = erlang:spawn_monitor(F),
			     Pid
		     end,
		     List),
    Answers = try 
		  lists:map(fun collect_from/1, Pids)
	      catch throw:Message ->
		      {BadPid, Reason} = Message,
		      handle_error(BadPid, Reason, Pids)
	      end,
    lists:foreach(fun (Pid) ->
			  normal_cleanup(Pid)
		  end, Pids),
    fuse(Fuse, Answers).

%% get error mesages here, but normal processes ends handled in normal_cleanup
collect_from(Pid) ->
    receive
	{Pid, R} ->
	    R;
	{'DOWN', _, _, BadPid, Reason} when Reason =/= normal ->
	    throw({BadPid, Reason})
    end.


%% exit all spawned processes
handle_error(BadPid, Reason, Pids) ->
    lists:foreach(fun (Pid) ->
			  exit(Pid, sibling_died)
		  end, Pids),
    %% wait till all processes will finish with result/end up lifetime
    lists:foreach(fun (Pid) ->
			  error_cleanup(Pid, BadPid)
		  end, Pids),
    exit(Reason).


normal_cleanup(Pid) ->
    receive
	{'DOWN', _, _, Pid, _Reason} ->
	    ok
    end.

error_cleanup(BadPid, BadPid) ->
    ok;
error_cleanup(Pid, BadPid) ->
    receive
	{Pid, _} ->
	    error_cleanup(Pid, BadPid);
	{'DOWN', _, _, Pid, _Reason} ->
	    ok
    end.


splitmany(L, S) ->
    splitmany(L, [], S).

splitmany([], A, _) ->
    lists:reverse(A);
splitmany(L, A, S) ->
    {First, Else} = split(S, L),
    splitmany(Else, [First|A], S).


split(S, L) ->
    split(S, L, []).

split(0, L, A) ->
    {lists:reverse(A), L};
split(S, [H|T], A) ->
    split(S - 1, T, [H|A]);
split(_, [], A) ->
    {lists:reverse(A), []}.


fuse(_, []) ->
    [];
fuse(FuseF, [E1|Res]) ->
    fuse(FuseF, Res, E1).

fuse(FuseF, [E2|Res], E1) ->
    fuse(FuseF, Res, FuseF(E1, E2));
fuse(_, [], R) ->
    R.



%%--------------------------------------------------------------
%% MapF - should produces {K,V} or [{K,V},...]
mapreduce(MapF, List, Granularity) ->
    mapreduce(MapF, List, dict:new(), fun add_key/3, Granularity).


mapreduce(MapF, List, InitState, ReduceF, Granularity) ->
    Parent = self(),
    {Reducer, ReducerRef} =
	erlang:spawn_monitor(fun () ->
				     reducer(Parent, 0, InitState, ReduceF)
			     end),
    MapF2 = fun (L) ->
		    Reducer ! lists:map(MapF, L),
		    1
	    end,
    SentMessages = try 
		       runfold(MapF2, fun (A, B) -> A + B end, List, Granularity)
		   catch
		       exit:Reason ->
			   erlang:demonitor(ReducerRef, []),
			   Reducer ! die,
			   exit(Reason)
		   end,
    Reducer ! {mappers, done, SentMessages},
    Results = receive
		  {Reducer, Results2} ->
		      Results2;
		  {'DOWN', _, _, Reducer, Reason2} ->
		      exit(Reason2)
	      end,
    receive
	{'DOWN', _, _, Reducer, normal} ->
	    nil
    end,
    Results.


reducer(Parent, NumReceived, State, ReduceF) ->
    receive
	die ->
	    nil;
	{mappers, done, NumReceived} ->
	    Parent ! {self (), State};
	Keys  ->
	    reducer(Parent, NumReceived + 1, foreach_key(State, ReduceF, Keys), ReduceF)
    end.


foreach_key(S, F, {K, V}) ->
    F(S, K, V);
foreach_key(S, F, [L|Ks]) ->
    foreach_key(foreach_key(S, F, L), F, Ks);
foreach_key(S, _, []) ->
    S.


add_key(Dict, K, V) ->
    case dict:is_key(K, Dict) of
	true ->
	    dict:append(K, V, Dict);
	false ->
	    dict:store(K, [V], Dict)
    end.


