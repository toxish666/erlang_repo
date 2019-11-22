-module(lockman_net).

-behaviour(application).

-export([
	 start/2,
	 stop/1
	]).


start(normal, _Args) ->
    lockman_sup:start_link().

stop(_State) ->
    ok.
