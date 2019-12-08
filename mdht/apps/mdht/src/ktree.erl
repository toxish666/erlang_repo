%%% @doc K-buckets structure
%%% @end

-module(ktree).
-include_lib("stdlib/include/ms_transform.hrl").
-include("consts.hrl").
%% For ktree record
-include("ktree_shared_records.hrl").


%% ktree manipulations
-export([
	 new_ktree/2,
	 get_node/2,
	 try_add/2,
	 remove/2,
	 pk/1,
	 is_empty/1
	]).
%% some iteration-like functions
-export([
	 on_ets_all/2
	]).
%% constant extracting alias
-export([
	 get_bucket_max_entries/0
	]).


%% @doc Create new ktree structure.
-spec new_ktree(mdht:public_key(), ets:tab()) -> ktree().
new_ktree(PK, Ets) ->
    logger:debug("Creating new Ktree with PK: ~p~n", [PK]),
    #ktree{pk = PK, kbuckets = Ets}.


%% @doc get node by it's PK.
-spec get_node(ktree(), mdht:public_key()) -> mdht:option(mdht_node:mdht_node()).
get_node(KTree, PK) ->
    case kbucket_index(KTree, PK) of
	none ->
	    none;
	Index ->
	    [KBucket | _] = ets:lookup(KTree, Index),
	    SelfPK = KTree#ktree.pk,
	    kbucket:get_node(KBucket, SelfPK, PK)
    end.
  
  
%% @doc Add !packed_node to ktree
-spec try_add(ktree(), packed_node:packed_node()) -> boolean().
try_add(KTree, PackedNode) ->
    logger:debug("Trying to add packed node ~p and ktree ~p", [PackedNode, KTree]),
    case kbucket_index(KTree, packed_node:get_pk(PackedNode)) of
	none ->
	    logger:debug("Failed to add packed node ~p", [PackedNode]),
	    false;
	Index ->
	    KBuckets = KTree#ktree.kbuckets,
	    BasePK = KTree#ktree.pk,
	    %% updating ets if condition is true
	    MS = ets:fun2ms(fun(#ind_kbucket{index = IndexB} = N) when IndexB == Index -> N end),
	    case ets:select(KBuckets, MS) of
		[] ->
		    logger:debug("Creating new ind_bucket record"),
		    NewKBucket = kbucket:new_kbucket(?KBUCKET_DEFAULT_SIZE, mdht),
		    {true, KBucketAdded} = kbucket:try_add(NewKBucket, BasePK, {PackedNode, packed_node}, false),
		    NewIndKBucket = #ind_kbucket{index = Index, kbucket = KBucketAdded},
		    ets:insert(KBuckets, NewIndKBucket),
		    logger:debug("New KBucket with first node ~p was made", [PackedNode]),
		    true;
		[#ind_kbucket{index = Index, kbucket = KBucket}| _] ->
		    TryAdd = kbucket:try_add(KBucket, BasePK, {PackedNode, packed_node}, false),
		    case TryAdd of
			{true, NewKBucket} ->
			    %NewIndKB = KB#ind_kbucket{kbucket = NewKBucket},
			    Bool = ets:update_element(KBuckets, Index, {#ind_kbucket.kbucket, NewKBucket}),
			    Bool;
			_ ->
			    false
		    end
	    end
    end.   


%% @doc Remove !dht_node with the given PK for the tree.
-spec remove(ktree(), mdht:public_key()) -> mdht:option(mdht_node:mdht_node()).
remove(KTree, PK) ->
    logger:debug("Removing PK ~p from KTree ~p", [PK, KTree]),
    case kbucket_index(KTree, PK) of
	none ->
	    logger:debug("Failed to remove PK ~p", [PK]),
	    none;
	Index ->
	    KBuckets = KTree#ktree.kbuckets,
	    BasePK = KTree#ktree.pk,
	    MS = ets:fun2ms(fun(N = #ind_kbucket{index = IndexB}) when IndexB == Index -> N end),
	    case ets:select(KBuckets, MS) of
		[] ->
		    none;	
		[#ind_kbucket{kbucket = KBucket} | _] ->
		TryRemove = kbucket:remove(KBucket,BasePK,PK),
		case TryRemove of
		    none ->
			none;
		    {_RemovedNode, NewKBucket} = Tup ->
			ets:update_element(KBuckets, Index, {#ind_kbucket.kbucket, NewKBucket}),
		    Tup
		end
	    end
    end.
	    
    
%% @doc Get ktree pk
-spec pk(ktree()) -> mdht:public_key().
pk(KTree) ->
    KTree#ktree.pk.


%% @doc Returns true if all kbuckets are empty.
-spec is_empty(ktree()) -> boolean().
is_empty(KTree) ->
    KBuckets = KTree#ktree.kbuckets,
    on_ets_all(KBuckets, fun(#ind_kbucket{kbucket = KBucket}) ->
				 kbucket:is_empty(KBucket)
			 end).


%% @doc Function on ets that iterates through all values; returns true if 
%% predicate is true for all elements of the ets.
%% @end
-spec on_ets_all(ets:tab(), fun((any()) -> boolean())) -> boolean().
on_ets_all(Ets, Predicate) ->    
    FirstKey = ets:first(Ets),
    on_ets_all_loop(Ets, Predicate, FirstKey).

on_ets_all_loop(_, _, '$end_of_table') ->
    true;
on_ets_all_loop(Ets, Predicate, Key) ->    
    [Rec|_] = ets:lookup(Ets, Key),
    logger:debug("ARWRWR ~p", [Rec]),
    case Predicate(Rec) of
	false ->
	    false;
	true ->
	    on_ets_all_loop(Ets, Predicate, ets:next(Ets, Key))
    end.
    

%% Return the possible internal index of KBucket where the key could be inserted/removed.
%% Same as in kbucket.erl
kbucket_index(KTree, PK) ->
    kbucket:kbucket_index(KTree#ktree.pk, PK).
    

get_bucket_max_entries() ->
    ?KBUCKET_MAX_ENTRIES.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("common_records.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

%% -------- test descriptions ----------
ktree_manipulations_test_() ->
    [
     {"New ktree can be created", ?setup(fun ktree_new_t/1)},
     {"Testing try_add functionals", ?setup(fun ktree_try_add_t/1)},
     {"Can't add to the tree with the same PK", ?setup(fun ktree_try_add_self_t/1)},
     {"Correct removing of elements in ktree", ?setup(fun ktree_remove_t/1)}
    ].

%% --------- setup ------------
start() ->
    Ets = ets:new(?MODULE, [
				ordered_set, 
				named_table, 
				protected,
				{keypos, #ind_kbucket.index}
			       ]),
    Ets.
     
stop(Ets) ->
    ets:delete(Ets).

%% --------- tests ------------
ktree_new_t(Ets) ->
    {PK, _} = libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(),
    NewKTree = ktree:new_ktree(PK, Ets),
    [?_assertEqual(PK, pk(NewKTree))].
    
ktree_try_add_t(Ets) ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    NewKTree = ktree:new_ktree(PK, Ets),
    FilledKTree = lists:foldl(fun(Int, KTree) ->
				      PKForNodeT = binary:copy(<<(Int + 2):8>>, ?PUBLICKEYBYTES),
				      <<_:8, ElsePK/binary>> = PKForNodeT,
				      PKForNode = <<255:8, ElsePK/binary>>,
				      PackedNode = 
					  create_node_packed((12345+Int),
							       <<1:8, 2:8, 3:8, 4:8>>,
							       PKForNode),
				      Bool = ktree:try_add(KTree, PackedNode),
				      KTree
			      end, NewKTree, lists:seq(0,7)),
    %% first bucket is full so it can't add more
    PKForNodeAdditT1 = binary:copy(<<1:8>>, ?PUBLICKEYBYTES),
    <<_:8, ElsePK1/binary>> = PKForNodeAdditT1,
    PKForNodeAddit1 = <<255:8, ElsePK1/binary>>,
    PackedNodeAddit1 = create_node_packed(12346, <<1:8, 2:8, 3:8, 5:8>>, PKForNodeAddit1),
    BoolAddit1 = ktree:try_add(FilledKTree, PackedNodeAddit1),
    %% but nodes can be added to other buckets
    PKForNodeAddit2 = binary:copy(<<1:8>>, ?PUBLICKEYBYTES),
    PackedNodeAddit2 = create_node_packed(12346, <<1:8, 2:8, 3:8, 5:8>>, PKForNodeAddit2),
    BoolAddit2 = ktree:try_add(FilledKTree, PackedNodeAddit2),
    [?_assert(not(BoolAddit1)),
     ?_assert(BoolAddit2)].

ktree_try_add_self_t(Ets) ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    NewKTree = ktree:new_ktree(PK, Ets),
    SomeNode = create_node_packed(12346, <<1:8, 2:8, 3:8, 5:8>>, PK),
    Bool = ktree:try_add(NewKTree, SomeNode),
    [?_assert(not(Bool))].

ktree_remove_t(Ets) ->
    PK = binary:copy(<<0:8>>, ?PUBLICKEYBYTES),
    NewKTree = ktree:new_ktree(PK, Ets),
    SomeNode = create_node_packed(12346, <<1:8, 2:8, 3:8, 5:8>>, 
				 binary:copy(<<1:8>>, ?PUBLICKEYBYTES)),
    RemoveRes1 = ktree:remove(NewKTree, packed_node:get_pk(SomeNode)),
    IsEmptyRes1 = ktree:is_empty(NewKTree),
    ktree:try_add(NewKTree, SomeNode),
    IsEmptyRes2 = ktree:is_empty(NewKTree),
    RemoveRes2 = ktree:remove(NewKTree, packed_node:get_pk(SomeNode)),
    IsEmptyRes3 =  ktree:is_empty(NewKTree),
    [?_assertEqual(none, RemoveRes1),
     ?_assert(IsEmptyRes1),
     ?_assert(not(IsEmptyRes2)),
     ?_assertNotEqual(none, RemoveRes2),
     ?_assert(IsEmptyRes3)
    ].


%%-------- helper test functions ---------
create_node_packed(Port, Addr, PK) ->
    SocketPreInfo = #socket_pre_info{
		       port = Port,
		       ip4_address = Addr},
    Socket = #socket{socket_pre_info = SocketPreInfo},
    packed_node:new_packed_node(Socket,PK).

-endif.
