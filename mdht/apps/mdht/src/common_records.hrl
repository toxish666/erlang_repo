%%
%% Intermediate structure for socket.
-record (socket_pre_info, {
			   port :: non_neg_integer(),
			   %% {0..255, 0..255, 0..255, 0..255} format
			   ip4_address = undefined :: term(),
			   %% {0..65535, --x7} format
			   ip6_address = undefined :: term()
			  }).

%% Socket structure.
-record (socket, {
		  socket_pre_info :: #socket_pre_info{}, %% socket info while it's in closed form, need to update that field!!!
		  socket_opened = undefined :: atom() %% socket in it's opened form
		 }).
-type socket() :: #socket{}.


%% Way to store the node info in a small format.
%% This record mainly built in packed_node.erl module
-record(packed_node, {
		      saddr :: socket(),
		      pk :: mdht:public_key()
		     }).
-type packed_node() :: #packed_node{}.
