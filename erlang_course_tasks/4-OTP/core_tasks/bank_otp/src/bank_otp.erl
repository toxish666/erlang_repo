%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/4-OTP.md#42-%D0%B1%D0%B0%D0%BD%D0%BA%D0%BE%D0%BC%D0%B0%D1%82-gen_statem"> task itself </a> for more info.
%% 
%%
%% erl -make
%% erl -pa ebin/
%% make:all([load]).
%% application:start(bank_otp).

-module(bank_otp).

-behaviour(application).

%%----------------------------------------------------------------------------
%% APPLICATION CALLBACKS
%%----------------------------------------------------------------------------
-export([
	 start/2,
	 stop/1
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 start_atm/1,
	 stop_atm/1
	]).


start(normal, _Args) ->
    bank_otp_supersup:start_link().

stop(_State) ->
    ok.


start_atm(Name) ->
    bank_serv:start_atm(Name). 

stop_atm(Name) ->
    bank_serv:stop_atm(Name).

