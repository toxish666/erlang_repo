%%% @doc Structure for holding nodes.
%% Number of nodes it can contain is set during creation.
%% Used in for storing nodes close to given PK
%%% @end

-module(kbucket).


-record(kbucket, {
		  capacity :: non_neg_integer(),
		  nodes = [] :: list() %this list should be sorted
		  }).
-type kbucket() :: #kbucket{}.

%% kbucket manipulations
-export([
	 new_kbucket/1,
	 find/3
	]).

%% kbucket utilities
-export([
	 distance_impl/3,
	 kbucket_index/2
	]).


%% @doc Construct new kbucket with given capacity.
-spec new_kbucket(non_neg_integer()) -> kbucket().
new_kbucket(Capacity) ->
    #kbucket{capacity = Capacity}.


%% @doc Find node
-spec find(mdht_node:mdht_node(), mdht:public_key(), mdht:public_key()) ->
		  mdht:option(non_neg_integer()).
find(KBucket, BasePK, OthPK) ->
    Nodes = KBucket#kbucket.nodes,
    utils:binary_search_by(Nodes, fun(N) ->
					  MPK = mdht_node:get_pk(N),
					  distance_impl(BasePK, MPK, OthPK)
				  end).







%% @doc distance/3 Check whether distance between PK1 and own PK is smaller than distance
%% between PK2 and own PK.
%% @end
-spec distance_impl(OwnPK, PK1, PK2) -> mdht:ordering()
  when
      OwnPK :: mdht:public_key(),
      PK1 :: mdht:public_key(),
      PK2 :: mdht:public_key().
distance_impl(OwnPK, PK1, PK2) ->
    logger:trace("Comparing distance between PKs.~n"),
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
    end,
    ok.


%% @doc Calculate the khash index index of a PK compared
%% to "own" PK.
%% None if supplied keys are the same
-spec kbucket_index(mdht:public_key(), mdht:public_key()) -> mdht:option(non_neg_integer()).
kbucket_index(OwnPK, OtherPK) ->
    logger:debug("Calculating khask index for PKs"),
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
    
    


%-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
    ).

%-endif
