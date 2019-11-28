-module(distrib_app).

-behaviour(application).

-export([start/2, stop/1]).

%rebar3 shell --sname fiva@localhost --setcookie 'XXXXX'

-define(SERVER, distrib_serv). 

-define(ContactNodes, ['contactnode@localhost', 'contactnode1@localhost', 'contactnode2@localhost']).


start(_StartType, _StartArgs) ->
    ensure_contact(?ContactNodes),
    distrib_sup:start_link().

stop(_State) ->
    case rd:fetch_resources(?SERVER) of
	{ok, Servers} when length(Servers) == 1 ->
	    lists:foreach(fun(N) ->
				  StrNode = atom_to_list(N),
				  [NodeName|_] = string:split(StrNode, "@"),
				  AtomNode = list_to_atom(NodeName),
				  Command = io_lib:format("./kill-erlang-node.sh ~s", [AtomNode]), 
				  os:cmd(Command)
			  end, ?ContactNodes);
	_ ->
	    ok
    end,
    ok.


ensure_contact(ContactNodes) ->
    io:format(" Contacting other nodes...~n"),
    Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
    case Answering of
        [] ->
	    io:format(" No contact has been made ~n"),
            {error, no_contact_nodes_reachable};
        _ ->
	    io:format(" Got some connection to contactnode ~n"),
            WaitTime = 1000,
            wait_for_nodes(length(Answering), WaitTime)
    end.


wait_for_nodes(MinNodes, WaitTime) ->
    io:format(" Waiting for other nodes to respond...~n"),
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->    
    case length(nodes()) > MinNodes of
        true ->
          ok;
        false ->
            timer:sleep(SliceTime),
            wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.




