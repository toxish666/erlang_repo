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

%% internal functions

%erl -pa "../../libsodium/ebin" -eval "application:set_env([{mdht, [{test_port, 7777}]}])" -eval "application:start(mdht)"
