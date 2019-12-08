%%% @doc Structure for holding nodes.
%% Number of nodes it can contain is set during creation.
%% Used in for storing nodes close to given PK
%%% @end

-module(kbucket).
-include_lib("kernel/include/logger.hrl").
-include("consts.hrl").

%% Structure for holding nodes.
%% Number of nodes it can contain is set during creation.
%% Nodes in kbucket are sorted by closeness to the PK; 
%% closest node is the first, while furthest is the last one.
-type mode() :: mdht | packed.
-record(kbucket, {
		  mode :: mode(),
		  capacity :: non_neg_integer(),
		  nodes = [] :: list() %this list should be sorted
		  }).
-type kbucket() :: #kbucket{}.
-export_type([kbucket/0]).

%% kbucket manipulations
-export([
	 new_kbucket/2,
	 is_full/1,
	 is_empty/1,
	 find/3,
	 get_node/3,
	 try_add/4,
	 remove/3,
	 contains/3,
	 len/1,
	 capacity/1,
	 get_nodes/1
	]).

%% kbucket utilities
-export([
	 distance_impl/3,
	 kbucket_index/2
	]).

%% get bucket default size
-export([
	 get_bucket_default_size/0
	]).


%% @doc Construct new kbucket with given capacity.
-spec new_kbucket(non_neg_integer(), mode()) -> kbucket().
new_kbucket(Capacity, Mode) ->
    #kbucket{mode = Mode, capacity = Capacity}.


%% @doc Check if kbucket is full
-spec is_full(kbucket()) -> boolean().
is_full(KBucket) ->
    length(KBucket#kbucket.nodes) == KBucket#kbucket.capacity.


%% @doc Check if kbucket is full
-spec is_empty(kbucket()) -> boolean().
is_empty(KBucket) ->    
    length(KBucket#kbucket.nodes) == 0.


%% @doc Check if node with given PK is in the `Kbucket`.
-spec find(kbucket(), mdht:public_key(), mdht:public_key()) ->
		  mdht:option(non_neg_integer()).
find(KBucket, BasePK, OthPK) ->
    Nodes = KBucket#kbucket.nodes,
    %logger:debug("ASDASDASD ~p", [Nodes]),
    SearchRes = utils:binary_search_by(Nodes, fun(N) ->
						      MPK = mdht_node:get_pk(N),
						      distance_impl(BasePK, MPK, OthPK)
					      end),
    case SearchRes of
	{some, Res} ->
	    Res;
	{error, _} ->
	    none
    end.


%% @doc Get Node by it's PublicKey.
-spec get_node(kbucket(), mdht:public_key(), mdht:public_key()) -> mdht:option(mdht_node:mdht_node()).
get_node(KBucket, BasePK, OthPK) ->
    case find(KBucket, BasePK, OthPK) of
	none ->
	    none;
	Nth ->
	    Nodes = KBucket#kbucket.nodes,
	    lists:nth(Nth, Nodes)
    end.


%% @doc Try to add new node to the kbucket.
%% - if node's public key is already in the bucket - update that node; returned
%% {true, NewKbucket};
%% - if kbucket is not full - new node is appended; returned {true, NewKbucket};
%% - if kbucket is full and evict = true, new node's public key is compared with
%% another public keys in the kbucket, and if it's closer than some node - prepen
%% it to the kbucket and the last node is removed; returned {true, NewKbucket};
%% - if kbucket is full and evict = false or new node is further away than any
%% other node in the bucket returned {false, OldKbucket}.
%%
-spec try_add(
	kbucket(), 
	mdht:public_key(), 
	{mdht_node:mdht_node(), mdht_node} | {mdht_node:packed_node(), packed_node}, 
	boolean()
       ) ->
	{boolean(), kbucket()}.
try_add(KBucket, BasePK, {NewNodeT, MdhtOrPackedAtomT} = Sp , Evict) ->
    logger:debug("Trying to add new node into kbucket ~p", [NewNodeT]),
    %% convert newnode to appropriate format
    {NewNode, MdhtOrPackedAtom}  = case KBucket#kbucket.mode of 
				       mdht ->
					   case MdhtOrPackedAtomT of
					       mdht_node ->
						   Sp;
					       packed_node ->
						   {mdht_node:new_mdht_node(NewNodeT), mdht_node}
					   end;
				       packed ->
					   case MdhtOrPackedAtomT of
					       mdht_node ->
						   {mdht_node:to_packed_node(NewNodeT), packed_node};
					       packed_node ->
						   Sp
					   end
				   end,
    Nodes = KBucket#kbucket.nodes,
    SearchRes = utils:binary_search_by(Nodes, fun(N) ->
						      MPK = MdhtOrPackedAtom:get_pk(N),
						      NewNodePK = MdhtOrPackedAtom:get_pk(NewNode),
						      distance_impl(BasePK, MPK, NewNodePK)
					      end),
    case SearchRes of
	{some, Index} when is_number(Index) ->
	    logger:debug("Updating node in kbucket"),
	    NewNodes = utils:update_list_with_element(Nodes, Index, NewNode),
	    {true, KBucket#kbucket{nodes = NewNodes}}; % edge
	{error, Index} when (Evict == false) orelse (Index == (KBucket#kbucket.capacity + 1)) ->
	    %% index is pointing past the end
	    case is_full(KBucket) of
		true ->
		    NodeToThrow = eviction_index(Nodes, MdhtOrPackedAtom),
		    %% so check if there's any node to throw cuz it's bad (but not evict)
		    case NodeToThrow of
			none ->
			    logger:debug("Node can't be added to the kbucket"),
			    {false, KBucket}; % edge
			NodeIndex ->
			    logger:debug("No free space left in the kbucket, the last bad node removed"),
			    %% replace the farthest bad node
			    NodesDeleted = utils:delete_nth(Nodes, NodeIndex),
			    NewNodes = NodesDeleted ++ [NewNode],
			    {true, KBucket#kbucket{nodes = NewNodes}} % edge
		    end;
		false ->
		    %% distance to the PK was bigger than the other keys, but
		    %% there's still free space in the kbucket for a node
		    logger:debug("Node inserted inside the kbucket"),
		    NewNodes = Nodes ++ [NewNode],
		    {true, KBucket#kbucket{nodes = NewNodes}} % edge
	    end;	
	{error, IndexT}  ->
	    %% index is pointing inside the list
	    NodesNewPre = case is_full(KBucket) of
			      true ->
				  logger:debug("No free space left in the kbucket, the last node removed"),
				  Index = IndexT,
				  lists:droplast(Nodes);
			      false ->
				  Index = IndexT,
				  Nodes
			  end,
	    logger:debug("Node inserted inside the kbucket to the index ~p", [Index]),
	    NewNodes = utils:insert_nth(NodesNewPre, Index, NewNode),
	    {true, KBucket#kbucket{nodes = NewNodes}} % edge
    end.

%% helper function for try_add
eviction_index(Nodes, MdhtOrPackedAtom) ->
    case MdhtOrPackedAtom of 
	packed_node ->
	    none;
	mdht_node ->
	    DiscNodeInd = utils:rfind_index(
			    Nodes, 
			    fun(N) -> MdhtOrPackedAtom:is_discarded_mnode(N) end
			   ),
	    NodeToThrow = 
		case DiscNodeInd of 
		    none ->
			BadNodeInd = utils:rfind_index(
				       Nodes, 
				       fun(N) -> MdhtOrPackedAtom:is_bad_mnode(N) end
				      ),
			BadNodeInd;
		    AnyNumber ->
			AnyNumber
		end,
	    NodeToThrow
    end.


%% @doc Remove Node with given PK from the KBucket.
%% Return that removed node if it's there and kbucket without that node
%% @end
-spec remove(kbucket(), mdht:public_key(), mdht:public_key()) -> mdht:option({mdht_node:mdht_node(), kbucket()}).
remove(KBucket, BasePK, NodePK) ->
    logger:debug("Removing node with PK ~p", [NodePK]),
    Nodes = KBucket#kbucket.nodes,
    SearchRes = utils:binary_search_by(Nodes, fun(N) ->
						      MPK = mdht_node:get_pk(N),
						      distance_impl(BasePK, MPK, NodePK)
					      end),
    case SearchRes of 
	{some, Index} ->
	    {RemovedNode, NewNodes} = utils:delete_nth_tup(Nodes, Index),
	    {RemovedNode, KBucket#kbucket{nodes = NewNodes}};
	{error, _} ->
	    logger:debug("No node to remove with PK: ~p", [NodePK]),
	    none
    end.
    

%% @doc Check if node with given PK is in the KBucket.
-spec contains(kbucket(), mdht:public_key(), mdht:public_key()) -> boolean().
contains(KBucket, BasePK, NodePK) ->
    Nodes = KBucket#kbucket.nodes,
    SearchRes = utils:binary_search_by(Nodes, fun(N) ->
						      MPK = mdht_node:get_pk(N),
						      distance_impl(BasePK, MPK, NodePK)
					      end),
    case SearchRes of 
	{some, _} ->
	    true;
	{error, _} ->
	    false
    end.
	

%% @doc Number of nodes this KBucket contains.
-spec len(kbucket()) -> non_neg_integer().
len(KBucket) ->
    erlang:length(KBucket#kbucket.nodes).


%% @doc Get the capacity of KBucket.
-spec capacity(kbucket()) -> non_neg_integer().
capacity(KBucket) ->
    KBucket#kbucket.capacity.

   	    
%% @doc distance/3 Check whether distance between PK1 and own PK is smaller than distance
%% between PK2 and own PK.
%% @end
-spec distance_impl(OwnPK, PK1, PK2) -> mdht:ordering()
  when
      OwnPK :: mdht:public_key(),
      PK1 :: mdht:public_key(),
      PK2 :: mdht:public_key().
distance_impl(OwnPK, PK1, PK2) ->
    logger:debug("Comparing distance between PKs ~p ~p", [PK1,PK2] ,#{domain => kbucket}),
    <<FOwnPK:8, ElseOwnPK/binary>> = OwnPK,
    <<FPK1:8, ElsePK1/binary>> = PK1,
    <<FPK2:8, ElsePK2/binary>> = PK2,
    case {FPK1 =/= FPK2, ElseOwnPK} of
	{false, <<>>} ->
	    equal;	
	{false, _} ->
	    distance_impl(ElseOwnPK, ElsePK1, ElsePK2);
	{true, _} ->
	    Res1 = FOwnPK bxor FPK1,
	    Res2 = FOwnPK bxor FPK2,
	    if (Res1 < Res2) ->
		    less;
	       true ->
		    greater
	    end
    end.


%% @doc Calculate the ktree index index of a PK compared
%% to "own" PK.
%% None if supplied keys are the same
-spec kbucket_index(mdht:public_key(), mdht:public_key()) -> mdht:option(non_neg_integer()).
kbucket_index(OwnPK, OtherPK) ->
    logger:debug("Calculating ktree index for PKs"),
    OwnPKList = binary:bin_to_list(OwnPK),
    OtherPKList = binary:bin_to_list(OtherPK),
    Xored = lists:zipwith(fun(OwnB, OtherB) -> OwnB bxor OtherB end, OwnPKList, OtherPKList), 
    Enumerated = lists:zip(Xored, lists:seq(0,31)),
    try
	lists:foreach(fun({Byte, I}) ->
			      Offs = lists:seq(0,7),
			      lists:foreach(fun(Of) ->
						    CheckTo = Byte band (16#80 bsr Of),
						    
						    if CheckTo =/= 0 ->
							    ValToReturn = I*8 + Of,
							    throw(ValToReturn);
						       true ->
							    skip
						    end
						   
					    end, Offs)
		      end, Enumerated),	     
	none
    catch
	SomeVal -> SomeVal
    end.

%% @doc Get all nodes in list format
-spec get_nodes(kbucket()) -> list().
get_nodes(KBucket) ->
    KBucket#kbucket.nodes.
    

%% @doc Get bucket default size.
get_bucket_default_size() ->
    ?KBUCKET_DEFAULT_SIZE.
    

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("common_records.hrl").

kbucket_index_test() ->
    {PK1, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    {PK2, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    {PK3, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    ?assertEqual(none, kbucket:kbucket_index(PK1,PK1)),
    ?assertMatch(
       SomeIndex when SomeIndex =< 255 andalso SomeIndex >= 0, 
       kbucket:kbucket_index(PK1,PK2)
    ),
    ?assertMatch(
       SomeIndex when SomeIndex =< 255 andalso SomeIndex >= 0, 
       kbucket:kbucket_index(PK2,PK3)
		),
    PK4 = binary:copy(<<2#10101010:8>>, ?PUBLICKEYBYTES),
    PK5 = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    PK6 = binary:copy(<<2#00101010:8>>, ?PUBLICKEYBYTES),
    ?assertEqual(0, kbucket:kbucket_index(PK4,PK5)),
    ?assertEqual(2, kbucket:kbucket_index(PK5,PK6)).
    
public_key_distance_test() ->
    PK1 = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    PK2 = binary:copy(<<1:8>>, ?PUBLICKEYBYTES),
    PK3 = binary:copy(<<2:8>>, ?PUBLICKEYBYTES),
    PK4 = binary:copy(<<16#ff:8>>, ?PUBLICKEYBYTES),
    PK5 = binary:copy(<<16#fe:8>>, ?PUBLICKEYBYTES),
    ?assertEqual(less, kbucket:distance_impl(PK1,PK2,PK3)),
    ?assertEqual(equal, kbucket:distance_impl(PK3,PK3,PK3)),
    ?assertEqual(less, kbucket:distance_impl(PK3,PK1,PK2)),
    ?assertEqual(greater, kbucket:distance_impl(PK3,PK4,PK5)),
    ?assertEqual(less, kbucket:distance_impl(PK5,PK4,PK3)).

kbucket_try_add_test() ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    FilledKBucket = lists:foldl(fun(Int, KBuck) ->
					SocketPreInfo = #socket_pre_info{
							   port = 12345 + Int,
							   ip4_address = <<1:8, 2:8, 3:8, 4:8>>},
					Socket = #socket{socket_pre_info = SocketPreInfo},
					PKForNode = binary:copy(<<(Int + 2):8>>, ?PUBLICKEYBYTES),
					PackedNode = packed_node:new_packed_node(Socket,PKForNode),
					{Bool, NewKBuck} = kbucket:try_add(
							     KBuck,
							     PK,
							     {PackedNode,packed_node},
							     false
							    ),
					?assert(Bool),
					NewKBuck
				end, KBucket, lists:seq(0,7)),
    %% full kbucket
    ?assertEqual(kbucket:len(FilledKBucket), kbucket:capacity(FilledKBucket)),
    %% closer node
    CloserNode = create_node_packed(12345, <<1:8, 2:8, 3:8, 5:8>>,
				    binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    %% farther node
    FartherNode = create_node_packed(12346, <<1:8, 2:8, 3:8, 5:8>>,
				     binary:copy(<<10:8>>, ?PUBLICKEYBYTES)),
    %% existing node
    ExistingNode = create_node_packed(12347, <<1:8, 2:8, 3:8, 5:8>>,
				      binary:copy(<<2:8>>, ?PUBLICKEYBYTES)),
    %% can't add a new farther node
    {Bool1, OldKBuck1} = kbucket:try_add(FilledKBucket, PK, {FartherNode,packed_node}, false),
    ?assert(not(Bool1)),
    %% can't add a new farther node with eviction
    {Bool2, OldKBuck2} = kbucket:try_add(OldKBuck1, PK, {FartherNode,packed_node}, true),
    ?assert(not(Bool2)),
    %% can't add a new closer node
    {Bool3, OldKBuck3} = kbucket:try_add(OldKBuck2, PK, {CloserNode,packed_node}, false),
    ?assert(not(Bool3)),
    %% can add a new closer node with eviction
    {Bool4, NewKBuck1} = kbucket:try_add(OldKBuck3, PK, {CloserNode,packed_node}, true),
    ?assert(Bool4),
    %% can update a node
    {Bool5, _} = kbucket:try_add(NewKBuck1, PK, {ExistingNode,packed_node}, false),
    ?assert(Bool5).

kbucket_try_add_bad_nodes_test() ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    KBucket = kbucket:new_kbucket(1, mdht),
    Node1 = create_node_packed(12345, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Node2 = create_node_packed(12346, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<2:8>>, ?PUBLICKEYBYTES)),
    {Bool1, KBucket1} = kbucket:try_add(KBucket, PK, {Node2, packed_node}, false),
    ?assert(Bool1),
    {Bool2, KBucket2} = kbucket:try_add(KBucket1, PK, {Node1, packed_node}, false),
    ?assert(not(Bool2)),
    %%mocking time lib
    meck:new(time, [unstick, passthrough]),
    meck:expect(time, clock_elapsed, fun(_) -> 1000 end),
    {Bool3, _} = kbucket:try_add(KBucket2, PK, {Node1, packed_node}, false),
    meck:unload(time),
    ?assert(Bool3).

kbucket_try_add_bad_nodes_evict_test() ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    KBucket = kbucket:new_kbucket(1, mdht),
    Node1 = create_node_packed(12345, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    Node2 = create_node_packed(12346, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<2:8>>, ?PUBLICKEYBYTES)),
    {Bool1, KBucket1} = kbucket:try_add(KBucket, PK, {Node1, packed_node}, true),
    ?assert(Bool1),
    {Bool2, KBucket2} = kbucket:try_add(KBucket1, PK, {Node2, packed_node}, true),
    ?assert(not(Bool2)),
    %%mocking time lib
    meck:new(time, [unstick, passthrough]),
    meck:expect(time, clock_elapsed, fun(_) -> 1000 end),
    {Bool3, _} = kbucket:try_add(KBucket2, PK, {Node1, packed_node}, true),
    meck:unload(time),
    ?assert(Bool3).

kbucket_remove_test() ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    Node1 = create_node_packed(12345, <<1:8, 2:8, 3:8, 4:8>>,
			       binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    ?assertEqual(none,kbucket:remove(KBucket, PK, Node1)),
    ?assert(kbucket:is_empty(KBucket)),
    {_, KBucket1} = kbucket:try_add(KBucket, PK, {Node1, packed_node}, true),
    ?assert(not(kbucket:is_empty(KBucket1))),
    NPK = packed_node:get_pk(Node1),
    {SomeNodeMdht, KBucket2} = kbucket:remove(KBucket1, PK, NPK),
    ?assertEqual(mdht_node:to_packed_node(SomeNodeMdht), Node1),
    ?assert(kbucket:is_empty(KBucket2)).

kbucket_get_node_test() -> 
    {PK, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {PKN, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    Node1 = create_node_packed(12345, <<127:8, 0:8, 0:8, 1:8>>,
			       PKN),
    {Bool1, KBucket1} = kbucket:try_add(KBucket, PK, {Node1, packed_node}, true),
    ?assert(Bool1),
    ?assertNotEqual(none, kbucket:get_node(KBucket1, PK, PKN)).
   
%% Check that insertion order does not affect the result order in the ktree
%% straight order
kbucket_position1_test() ->
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {BasePK, Node1, Node2, Node3} = position_data(),
    {_, KBucket1} = kbucket:try_add(KBucket, BasePK, {Node1, packed_node}, true),
    {_, KBucket2} = kbucket:try_add(KBucket1, BasePK, {Node2, packed_node}, true),
    {_, KBucket3} = kbucket:try_add(KBucket2, BasePK, {Node3, packed_node}, true),
    ?assertEqual(1,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node1))),
    ?assertEqual(2,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(3,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node3))).

%% reverse order
kbucket_position2_test() ->
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {BasePK, Node1, Node2, Node3} = position_data(),
    {Res1, KBucket1} = kbucket:try_add(KBucket, BasePK, {Node3, packed_node}, true),
    ?assertEqual(1,kbucket:find(KBucket1, BasePK, packed_node:get_pk(Node3))),
    {Res2, KBucket2} = kbucket:try_add(KBucket1, BasePK, {Node2, packed_node}, true),
    ?assertEqual(1,kbucket:find(KBucket2, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(2,kbucket:find(KBucket2, BasePK, packed_node:get_pk(Node3))),
    {Res3, KBucket3} = kbucket:try_add(KBucket2, BasePK, {Node1, packed_node}, true),
    ?assert(Res1),?assert(Res2),?assert(Res3),
    ?assertEqual(1,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node1))),
    ?assertEqual(2,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(3,kbucket:find(KBucket3, BasePK, packed_node:get_pk(Node3))).

%% Check that removing order does not affect the order of nodes inside
%% first element
kbucket_position_remove1_test() ->
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {BasePK, Node1, Node2, Node3} = position_data(),
    {_, KBucket1} = kbucket:try_add(KBucket, BasePK, {Node1, packed_node}, true),
    {_, KBucket2} = kbucket:try_add(KBucket1, BasePK, {Node2, packed_node}, true),
    {_, KBucket3} = kbucket:try_add(KBucket2, BasePK, {Node3, packed_node}, true),
    Node1PK = packed_node:get_pk(Node1),
    {_SomeNode ,KBucketNew} = kbucket:remove(KBucket3, BasePK, Node1PK),
    ?assertEqual(none,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node1))),
    ?assertEqual(1,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(2,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node3))).

%% second element
kbucket_position_remove2_test() ->
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {BasePK, Node1, Node2, Node3} = position_data(),
    {_, KBucket1} = kbucket:try_add(KBucket, BasePK, {Node1, packed_node}, true),
    {_, KBucket2} = kbucket:try_add(KBucket1, BasePK, {Node2, packed_node}, true),
    {_, KBucket3} = kbucket:try_add(KBucket2, BasePK, {Node3, packed_node}, true),
    Node2PK = packed_node:get_pk(Node2),
    {_SomeNode ,KBucketNew} = kbucket:remove(KBucket3, BasePK, Node2PK),
    ?assertEqual(1,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node1))),
    ?assertEqual(none,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(2,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node3))).

%% third element
kbucket_position_remove3_test() ->
    KBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
    {BasePK, Node1, Node2, Node3} = position_data(),
    {_, KBucket1} = kbucket:try_add(KBucket, BasePK, {Node1, packed_node}, true),
    {_, KBucket2} = kbucket:try_add(KBucket1, BasePK, {Node2, packed_node}, true),
    {_, KBucket3} = kbucket:try_add(KBucket2, BasePK, {Node3, packed_node}, true),
    Node3PK = packed_node:get_pk(Node3),
    {_SomeNode ,KBucketNew} = kbucket:remove(KBucket3, BasePK, Node3PK),
    ?assertEqual(1,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node1))),
    ?assertEqual(2,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node2))),
    ?assertEqual(none,kbucket:find(KBucketNew, BasePK, packed_node:get_pk(Node3))).



%%-------- helper test functions ---------
create_node_packed(Port, Addr, PK) ->
    SocketPreInfo = #socket_pre_info{
		       port = Port,
		       ip4_address = Addr},
    Socket = #socket{socket_pre_info = SocketPreInfo},
    packed_node:new_packed_node(Socket,PK).

position_data() ->
    PK = binary:copy(<<3:8>>, ?PUBLICKEYBYTES),
    <<_:8, ElsePK/binary>> = PK,
    PK1 = <<1:8, ElsePK/binary>>,
    %% packed node 1
    PK2 = <<(binary:part(PK1, 0, 4))/binary, 1:8, (binary:part(PK1, 5, byte_size(PK1) - 5))/binary>>,
    ?assertEqual(32, size(PK2)),
    PackedNode1 = create_node_packed(12345, <<0:8,0:8,0:8,0:8>>, PK2),
    %% packed node 2
    PK3 = <<(binary:part(PK2, 0, 9))/binary, 2:8, (binary:part(PK2, 10, byte_size(PK2) - 10))/binary>>,
    ?assertEqual(32, size(PK3)),
    PackedNode2 = create_node_packed(12346, <<0:8,0:8,0:8,0:8>>, PK3),
    %% packed node 3
    PK4 = <<(binary:part(PK3, 0, 13))/binary, 4:8, (binary:part(PK3, 14, byte_size(PK2) - 14))/binary>>,
    ?assertEqual(32, size(PK4)),
    PackedNode3 = create_node_packed(12347, <<0:8,0:8,0:8,0:8>>, PK4),
    {PK1, PackedNode1, PackedNode2, PackedNode3}.

    
-endif.
