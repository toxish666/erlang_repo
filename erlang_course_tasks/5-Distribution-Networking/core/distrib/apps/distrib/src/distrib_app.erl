%%%-------------------------------------------------------------------
%% @doc distrib public API
%% @end
%%%-------------------------------------------------------------------

-module(distrib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    distrib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
