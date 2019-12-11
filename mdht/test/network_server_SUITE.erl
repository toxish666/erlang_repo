-module(network_server_SUITE).

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
    application:start(mdht),
    %% spawn one node
    {Node1Name, Node1Port} = Node1Info = {fiva1@localhost, "7777"},
    create_node(Node1Name, Node1Port),
    [Node1Info|Config].

end_per_suite(_Config) ->
    application:stop(mdht),
    rpc:cast('fiva1@localhost', init, stop, []),
    ok.

init_per_group(_, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

all() -> [{group, somet}].

groups() -> [{somet,
	      [parallel, {repeat, 1}],
	      [test]
	     }].


test(Config) ->
    %% fiva1 node in mdht_node format       
    {Name1, PortT} = lists:keyfind('fiva1@localhost', 1, Config),
    Port1 = erlang:list_to_integer(PortT),
    {ok, PK1} = rpc:call(Name1, gen_server, call, [mdht_server, {get_pk}]),
    SomeNodePacked = packed_node:create_node_packed_ipv4(Port1, <<127:8,0:8,0:8,1:8>>, PK1),
    SomeNodeMdht = mdht_node:new_mdht_node(SomeNodePacked),  
    network_server:ping(SomeNodeMdht),
    timer:sleep(2001),
    
    {ok, PKM} = gen_server:call(mdht_server, {get_pk}),
    MyNodePacked = packed_node:create_node_packed_ipv4(5799, <<127:8,0:8,0:8,1:8>>, PKM),
    MyNodeMdht = mdht_node:new_mdht_node(MyNodePacked),
    Res = rpc:call(Name1, network_server, ping, [MyNodeMdht]),
    timer:sleep(2001),

    ok.


create_node(AtomNodeName, PortNumber) ->
    Cmnd = io_lib:format("erl -sname ~s -setcookie XXXXX -pa '../../lib/libsodium/ebin' -pa '../../lib/mdht/ebin' -eval 'application:set_env([{mdht, [{test_port, ~s}]}])' -eval 'application:start(mdht)' -detached", [AtomNodeName, PortNumber]),
    os:cmd(Cmnd),
    Res = rpc:call(AtomNodeName, os, cmd, ["pwd"]),
    ct:pal("Got ~p~n", [Res]),
    {error,{already_started,mdht}} = rpc:call(AtomNodeName, application, start, [mdht]),
    ok.
    
