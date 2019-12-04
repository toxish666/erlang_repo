-module(time).

-export([
	 clock_now/0,
	 clock_elapsed/1
	]).


%% @doc Current time in the system
-spec clock_now() -> mdht:instant().
clock_now() ->
    erlang:system_time(second).
    

%% @doc Diff between passed time and clock_now()
-spec clock_elapsed(mdht:instant()) -> mdht:instant().
clock_elapsed(Instant) ->
    clock_now() - Instant.
    

    


