%%% @doc K-buckets structure
%%% @end

-module(ktree).
-include("consts.hrl").
%% For ktree record
-include("ktree_shared_records.hrl").


%% ktree manipulations
-export([
	 new_ktree/2,
	 get_node/2,
	 try_add/2,
	 remove/2
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
    logger:debug("Trying to add packed_node ~n"),
    logger:debug("Trying to add packed node ~p and ktree ~p~n", [PackedNode, KTree]),
    case kbucket_index(KTree, packed_node:get_pk(PackedNode)) of
	none ->
	    logger:debug("Failed to add packed node ~p~n", [PackedNode]),
	    false;
	Index ->
	    KBuckets = KTree#ktree.kbuckets,
	    BasePK = KTree#ktree.pk,
	    %% updating ets if condition is true
	    MS = ets:fun2ms(fun(N = #ind_kbucket{index = IndexB, kbucket = KBucket}) when IndexB == Index -> 
				    TryAdd = kbucket:try_add(KBucket,BasePK,PackedNode,
							     false, packed_node),
				    case TryAdd of 
					{true, NewKBucket} ->
					    N#ind_kbucket{kbucket = NewKBucket};
					_ ->
					    N 
				    end
			    end),
	    UpdC = ets:select_replace(KBuckets, MS),
	    case UpdC of 
		NN when NN > 0 ->
		    logger:debug("Succsesfully added packed node ~p~n", [PackedNode]),
		    true;
		_ ->
		    logger:debug("Packed node ~p was not added to ~p~n", [PackedNode, KTree]),
		    false
	    end
    end.
	    

%% @doc Remove !dht_node with the given PK for the tree.
-spec remove(ktree(), mdht:public_key()) -> mdht:option(mdht_node:mdht_node()).
remove(KTree, PK) ->
    logger:debug("Removing PK ~p from KTree ~p~n", [PK, KTree]),
    case kbucket_index(KTree, PK) of
	none ->
	    logger:debug("Failed to remove PK ~p", [PK]),
	    none;
	Index ->
	    KBuckets = KTree#ktree.kbuckets,
	    BasePK = KTree#ktree.pk,
	    MS = ets:fun2ms(fun(N = #ind_kbucket{index = IndexB}) when IndexB == Index -> N end),
	    [KBucket | _] = ets:select(KBuckets, MS),
	    TryRemove = kbucket:remove(KBucket,BasePK,PK),
	    case TryRemove of
		none ->
		    none;
		{_RemovedNode, NewKBucket} = Tup ->
		    ets:update_element(KBuckets, Index, {#ind_kbucket.kbucket, NewKBucket}),
		    Tup
	    end
    end.
	    
    
%% Return the possible internal index of KBucket where the key could be inserted/removed.
%% Same as in kbucket.erl
kbucket_index(KTree, PK) ->
    kbucket:kbucket_index(KTree#ktree.pk, PK).


get_bucket_max_entries() ->
    ?KBUCKET_MAX_ENTRIES.
