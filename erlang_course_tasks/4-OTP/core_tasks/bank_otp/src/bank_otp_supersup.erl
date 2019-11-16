-module(bank_otp_supersup).
-behaviour(supervisor).


-export([
	 start_link/0, 
	 init/1
	]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    MaxRestart = 1,
    MaxTime = 3000,
    SupFlags = #{strategy => rest_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecForBankServer = [#{
		  id => bank_serv, 
		  start => {bank_serv, start_link, [self()]},
		  restart => permanent,
		  shutdown => 5000,
		  type => worker,
		  modules => [bank_serv]
		 }],
    {ok, {SupFlags, ChildSpecForBankServer}}.
		  

