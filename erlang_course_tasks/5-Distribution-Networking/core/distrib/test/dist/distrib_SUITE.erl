-module(distrib_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0, groups/0,
	 init_per_group/2, end_per_group/2,
	 init_per_suite/1, end_per_suite/1]).
-export([test/1]).


suite() ->
    [
     {timetrap, {minutes, 5}}
    ].


%% Setup/Teardown
%% ----------------------------------------------------------------------
init_per_suite(Config) ->
    case erlang:get_cookie() of 
	nocookie ->
	    ct:fail("No cookie has been specified. Use rebar3 ct --sname toxa@localhost --setcookie 'XXXXX' command instead");
	'XXXXX' ->
	    ok;
	_ ->
	    ct:fail("No cookie")
    end,
    ct:pal("SelfPid ~p~n",[os:cmd("pwd")]),
    application:start(rd),
    application:start(statistics),
    application:start(distrib),
    %% spawn some nodes
    create_node(fiva1@localhost),
    create_node(fiva2@localhost),
    create_node(fiva3@localhost),
    Config.


end_per_suite(_Config) ->
    application:stop(rd),
    application:stop(statistics),
    application:stop(distrib),
    rpc:cast('fiva1@localhost', init, stop, []),
    rpc:cast('fiva2@localhost', init, stop, []),
    rpc:cast('fiva3@localhost', init, stop, []),
    rpc:cast('contactnode1@localhost', init, stop, []),
    ok.


init_per_group(distrib, Config) ->
    
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.


all() -> [{group, distrib}].

groups() -> [{distrib,
	      [parallel, {repeat, 1}],
	      [test]
	     }].
	      

test(_Config) ->
    rpc:call('fiva1@localhost', statistics, started_task, [self()]),
    Res1 = rpc:call('fiva1@localhost', statistics, statistics_info, []),
    ct:pal("From fiva1 ~p~n",[Res1]),
    StatisticInfo1 = statistics:statistics_info(),
    StatisticInfo1 = [{started,1},{failed,0},{completed,0},{restarted,0}],
    ct:pal("Got result ~p~n",[StatisticInfo1]),
    rpc:call('fiva2@localhost', statistics, completed_task, [self()]),
    Res2 = rpc:call('fiva2@localhost', statistics, statistics_info, []),
    StatisticInfo2 = statistics:statistics_info(),
    StatisticInfo2 = [{started,1},{failed,0},{completed,1},{restarted,0}],
    ct:pal("Got result ~p~n",[StatisticInfo2]),
    timer:sleep(10).




%% Handlers
%% ----------------------------------------------------------------------
create_node(AtomNodeName) ->
    Cmnd = io_lib:format("erl -sname ~s -setcookie XXXXX -detached", [AtomNodeName]),
    os:cmd(Cmnd),
    timer:sleep(1000),
    %%rd
    rpc:call(AtomNodeName, code, add_patha, ["../../lib/rd/ebin"]),
    rpc:call(AtomNodeName, application, start, [rd]),
    %%statistics
    rpc:call(AtomNodeName, code, add_patha, ["../../lib/statistics/ebin"]),
    rpc:cast(AtomNodeName, application, start, [statistics]),
    %%distrib
    rpc:call(AtomNodeName, code, add_patha, ["../../lib/distrib/ebin"]),
    rpc:cast(AtomNodeName, application, start, [distrib]),
    ok.
