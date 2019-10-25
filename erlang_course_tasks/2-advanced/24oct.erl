-module('24oct').

-compile(export_all).

loop_worker() ->
    receive
	_ -> throw(die)
    after 1000 ->
	    tires
    end.

loop_supervisor(0) ->
    'got 0 tries';
loop_supervisor(Retries) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun loop_worker/0),
    receive 
	{'EXIT', _, _} = Msg ->
	    io:format("got message ~p~n", [Msg]),
	    loop_supervisor(Retries - 1);
	Msg ->
	    io:format("got message ~p~n", [Msg])
    end.

start() ->
    Pid = spawn(fun() -> loop_supervisor(2) end),
    timer:sleep(500),
    exit(error, kill).



%%--------------------------------
map(F, List, PartialResult, Timeout) ->
    Self = self(),
    Ref = make_ref(),
    Pid = spawn_link(fun () ->
		       Self ! {map_result, Ref, do_map(F, List, PartialResult, Timeout)} end),
    receive
	{map_result, Ref, Result} ->
	    Result;
	{'EXIT', Pid, _} = Msg -> self() ! Msg,
				  error
    after Timeout ->
	    exit(Pid, kill),
	    {error, timeout}
    end.


do_map(F, List, PartialResult, Timeout) ->
    Self = self(),
    process_flag(trap_exit, true),
    [ spawn_link(fun () ->
			    Result = F(X),
			    Self ! {result, self(), Result}
		    end)
      || X <- List ],
    collect(length(List), timeout).

collect([], _) ->
    [];
collect([NextPid|Tail], Timeout) ->
    Result = receive
		 {result, NextPid, X} ->
		     X;
		 {'EXIT', NextPid, Reason} ->
		     {error, Reason}
	     after Timeout ->
		     {error, timeout}
	     end,
    [Result | collect(Tail, Timeout)].

test() ->
    map(fun(X) ->
		X*2 end, [1,2,3], true, 1000),
    flush.

flush() ->
    receive
	Msg -> 
	    io:format("got message ~p~n", [Msg]),
	    flush()
    after 0 ->
	    ok
    end.

