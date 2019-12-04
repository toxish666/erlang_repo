%%% @doc Gen-server for holding nodes.
%% Number of nodes it can contain is set during creation.
%% Used in for storing nodes close to given PK
%%% @end

-module(kbucket_serv).
%-behaviour(gen_server).


%% Behaviour
%%-export([
%%	 init/1,
%%	 handle_call/3,
%%	 handle_cast/2,
%%	 handle_info/2,
%%	 terminate/2,
%%	 code_change/3
%%	]).
%%


-record(kbucket, {
		  capacity :: non_neg_integer(),
		  nodes :: atom()%[#nodes{}]
		  }).
-type kbucket() :: #kbucket{}.


%% @doc new/1 Construct new kbucket with given capacity
-spec new(non_neg_integer()) -> kbucket().
new(Capacity) ->
    #kbucket{capacity = Capacity}.
    

%% @doc distance/3 Check whether distance between PK1 and own PK is smaller than distance
%% between PK2 and own PK.
%% @end
-spec distance_impl(OwnPK, PK1, PK2) -> mdht:ordering()
  when
      OwnPK :: mdht:publickey(),
      PK1 :: mdht:publickey(),
      PK2 :: mdht:publickey().
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
