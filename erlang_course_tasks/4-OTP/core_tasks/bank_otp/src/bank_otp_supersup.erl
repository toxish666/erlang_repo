
-module(bank_otp_supersup).
-behaviour(supervisor).


-export([
	 start_link/0, 
	 init/1
	]).

%init([]) ->
%    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
%    ChildSpecs = [],
%    {ok, 
%     {SupFlags, ChildSpecs}
%    }.
%
%
%start_link() ->
%    supervisor:start_link({local, bank_otp}, ?MODULE, []).
%
%
%start_atm(Name, MFA) ->
%    ChildSpec = #{
%		  id => Name, 
%		  start => {atm_sup, start_link, [Name, MFA]},
%		  restart => permanent,
%		  shutdown => 2000,
%		  type => supervisor,
%		  modules => [atm_sup]
%		 },
%    supervisor:start_child(bank_otp, ChildSpec).
%
%stop_atm(Name) ->
%    supervisor:terminate_child(atm_sup, Name),
%    supervisor:delete_child(atm_sup, Name).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    MaxRestart = 1,
    MaxTime = 3000,
    SupFlags = #{strategy => one_for_all, intensity => MaxRestart, period => MaxTime},
    ChildSpecForBankServer = [#{
		  id => bank_serv, 
		  start => {bank_serv, start_link, [self()]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [bank_serv]
		 }],
    {ok, {SupFlags, ChildSpecForBankServer}}.
		  

