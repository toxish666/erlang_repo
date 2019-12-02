%%%-------------------------------------------------------------------
%% @doc kvdistrib public API
%% @end
%%%-------------------------------------------------------------------

-module(kvdistrib_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    kvdistrib_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
