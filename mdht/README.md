mdht
=====

DHT-like system that has only one basic capability - finding another peer in the overlay network constructed by nodes with the running application.

Basic infrastructure
----

All the nodes have public and secret keys assigned to them for encryption and routing purposes.

Node has a special kbucket/ktree structure for storing information about closest (based on public key XOR metric) nodes.

Protocol defines only 4 basic functions: ping request, ping response, request for closest to K public key known nodes, and corresponding response. It should be noted that this protocol is easily extendable.

Node sends to its 8 closest nodes ping request every 4 seconds. Moreover node asking every known nodes for their 4 closest nodes whose keys are the closest to the own public key. It happens every 5 seconds.

Find function - is the only public api function, which traverse through the network trying to search for a node with a given public key. Algorithm is heavily based on a xor metric and recursion.

Additional notes and usage
----

If you're a MAC user than application might not run with error message: 'autoconf is required, but wasn't found on this system'. The solution is to install it with 'brew install libtool autoconf automake'.

To debug the system there was added logger:debug(...) commands. In order to see it's output in eunit tests I used the following commands:
- rebar3 as test shell (note that sys.config should be filled accordingly)
- eunit:test(module, [verbose]).


Default port is 5799. But the following sequence of commands let anyone run application with chosen port. Assuming that rebar3 compile or rebar3 shell were called before next commands:
- cd _build/default/lib/mdht/ebin/ (from the root of the project)
- erl -pa "../../libsodium/ebin" -eval "application:set_env([{mdht, [{test_port, 7780}]}])" -eval "application:start(mdht)" (port there is 7780 but may be anything)

From there erl interactive shell is running. The following is used only for demonstrating/debugging purposes.
- {ok, SomePK} = gen_server:call(mdht_server, {get_pk}). (for getting own public key) (use rp(...) for displaying long records)

From another node using this PK: 
- NewPackedNode = packed_node:create_node_packed_ipv4(7780, <<127:8, 0:8, 0:8, 1:8>>, SomePK). (creating info about first node in intermediate format)
- MdhtNode = mdht_node:new_mdht_node(NewPackedNode). (in appropriate inner form)
- network_server:ping(MdhtNode). (it create connection between 2 nodes)

Use mdht:find(SomePublicKey) to find any node with the PK in the network.

TODO
----
- bootstrap nodes
- ipv6 testing
- common tests improvements