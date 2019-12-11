-module(time).

-export([
	 clock_now/0,
	 clock_elapsed/1,
	 send_after/3
	]).


%% @doc Current time in the system.
-spec clock_now() -> mdht:instant().
clock_now() ->
    erlang:system_time(second).
    

%% @doc Diff between passed time and clock_now().
-spec clock_elapsed(mdht:instant()) -> mdht:instant().
clock_elapsed(Instant) ->
    clock_now() - Instant.
    

%% @doc Send message to the target after some time.
send_after(Time, Target, Msg) ->
    erlang:send_after(Time, Target, Msg).   


