%%%-------------------------------------------------------------------
%% @doc mdht public API
%% @end
%%%-------------------------------------------------------------------

-module(mdht_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mdht_sup:start_link().

stop(_State) ->
    ok.
