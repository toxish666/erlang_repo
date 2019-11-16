%% erl -make
%% erl -pa ebin/
%% make:all([load]).
%% application:start(bank_otp).

-module(bank_otp).

-behaviour(application).

-export([
	 start/2,
	 stop/1
	]).

-export([
	 start_atm/1,
	 stop_atm/1,
	 insert_card/2,
	 push_button/2
	]).


start(normal, _Args) ->
    bank_otp_supersup:start_link().

stop(_State) ->
    ok.


start_atm(Name) ->
    bank_serv:start_atm(Name). 

stop_atm(Name) ->
    bank_serv:stop_atm(Name).

insert_card(Name, CardNo) ->
    bank_serv:insert_card(Name, CardNo).

push_button(Name, Button) ->
    bank_serv:push_button(Name, Button).
