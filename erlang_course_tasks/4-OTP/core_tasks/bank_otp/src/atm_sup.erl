-module(atm_sup).

-behaviour(supervisor).


-export([
	 start_link/1, 
	 init/1
	]).


start_link(BankServerPid) ->
    supervisor:start_link(?MODULE, BankServerPid).


init(BankServerPid) ->
    MaxRestart = 5,
    MaxTime = 3000,
    SupFlags = #{strategy => simple_one_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecForBankAtp = [#{
			     id => bank_atp, 
			     start => {atm_node, start_link, [BankServerPid]},
			     restart => permanent,
			     shutdown => 5000,
			     type => worker,
			     modules => [atm_node]
			    }],
    {ok, {SupFlags, ChildSpecForBankAtp}}.
