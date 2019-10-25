-module('22oct').

-export([make_proc/0,
	 ffping/1,
	 ffpong/1,
	 funping/0,
	 funpong/0]).

%% Pid2 = spawn(fun() -> receive Msg -> io:format("Got message: ~p~n", [Msg]) end end).

%% erlang:is_process_alive(Pid2).

%% Pid 2 ! 'hello!'.

%% erlang:is_process_alive(Pid2). 



%% Pid3 = spawn(fun Loop() -> receive Msg -> io:format("ot message ~p~n", [Msg]) end, Loop() end).

%% Pid3 ! 'hello!'.

%% erlang:is_process_alive(Pid3).

%%exit(Pid3, 'fiva').

%% erlang:is_process_alive(Pid3).

flush() ->
    receive
	_ ->
	    flush()
    after 0 ->
	    ok
    end.

make_proc() ->
    spawn(
      fun Loop() ->
	      receive
		  {message, From, Msg} when is_list(Msg) ->
		      From ! {result, Msg ++ Msg};
		  fiva ->
		      io:format("Ne~n");
		  flush ->
		      flush()
	      after 1000 ->
		      io:format("FFFFF~n")
	      end,

	      Loop()
      end
     ).


%% В консоли 
%% Recv = fun() -> receive Msg -> Msg end end.
%% введя Recv() получим то, на что ответил процесс

%% P1 = '22oct':make_proc().
%% P1 ! {message, self(), 33}.
%% Recv(). 




%% self() ! trash
%% self() ! asdasd 
%% flush().

%%spawn(erlang, send, [self(), ok]).


%% после 20 попаданий - пинг
%% после 10 попаданий - понг
%% при получении stat - сколько шариков процесс получил 
%% из двух разных процессов 


ffping(I) ->
    K = I,
    receive
	{ping, From, Msg} -> 
	    io:format("received ping ~n"),
	    K + 1,
	    timer:sleep(1000),
	    From ! {pong, self(), Msg};
	{stat, From} ->
	    From ! {gotstat, I}
    end,
    if (I rem 20) == 0 ->
	    io:format("20 !!~n");
       true ->
	    nothing
    end,
    ffping(K).

ffpong(I) ->
    receive
	{pong, From, Msg} -> 
	    io:format("received PONG ~n"),
	    timer:sleep(1000),
	    From ! {ping, self(), Msg};
	{stat, From} ->
	    From ! {gotstat, I}
    end,
    if (I rem 10) == 0 ->
	    io:format("10 !!");
       true ->
	    nothing
    end,
    ffpong(I).


%% ----------------------------------
funpong() ->
    spawn(fun () -> 
		  V = fun Counter(I) ->
			  receive
			      {pong, From, Msg} -> 
				  timer:sleep(1000),
				  From ! {ping, self(), Msg},
				  io:format("received PONG ~p~n", [I]),
				  if (I rem 10) == 0 ->
					  io:format("10 !!");
				     true ->
					  nothing
				  end,
				  Counter(I+1);
			      {stat, From} ->
				  From ! {gotstat, I},
				  Counter(I)
			  end
		  end,
		  V(1)
	  end).

funping() ->
    spawn(fun () -> 
		  V = fun Counter(I) ->
			  receive
			      {ping, From, Msg} -> 
				  timer:sleep(1000),
				  From ! {pong, self(), Msg},
				  io:format("received PONG ~p~n", [I]),
				  if (I rem 20) == 0 ->
					  io:format("10 !!");
				     true ->
					  nothing
				  end,
				  Counter(I+1);
			      {stat, From} ->
				  From ! {gotstat, I},
				  Counter(I)
			  end
		  end,
		  V(1)
	  end).
