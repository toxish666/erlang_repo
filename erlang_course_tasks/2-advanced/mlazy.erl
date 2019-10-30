-module(mlazy).
-compile(export_all).
-export([lazy_list/3,
	 lazy_map/2,
	 lazy_foldl/3, 
	 lazy_filter/2,
	 lazy_map2/2,
	 concatenate_lazy_lists/2,
	 lazy_read_file/1,
	 max_sub_sum/1,
	 max_sub_sum_withk/2]).


lazy_list(Begin, End, Step) when Begin =< End, Step > 0 ->
    fun() ->
	    [Begin|lazy_list(Begin + Step, End, Step)]
    end;
lazy_list(_, _, _) ->
    fun() ->
	    []
    end.


lazy_map(_, []) ->
    fun() ->
	    []
    end;
lazy_map(F, [H|T]) ->
    fun() ->
	    [F(H)|lazy_map(F,T)]
    end.


lazy_foldl(_, A, []) ->
    fun() ->
	    A
    end;
lazy_foldl(F, A, [H|T]) ->
    fun() ->
	    lazy_foldl(F, F(H,A), T)
    end.


lazy_filter(_, []) ->
    fun() ->
	    []
    end;
lazy_filter(F, [H|L]) ->
    fun() ->
	    case F(H) of
		true ->
		    [H|lazy_filter(F, L)];
		false ->
		    lazy_filter(F, L)
	    end
    end.


lazy_map2(_, []) ->
    fun() ->
	    []
    end;
lazy_map2(F, L = [_|_]) ->
    lazy_foldl(fun (H, A) -> [F(H)|A] end, [], L).


concatenate_lazy_lists(LL1, LL2) ->
    fun() ->
	    case LL1() of
		[H|LazyT] ->
		    [H|concatenate_lazy_lists(LazyT, LL2)];
		[] ->
		    LL2()
	    end
    end.



lazy_read_file(File) ->
    {ok, Device} = file:open(File, read),
    lazy_read_lines(Device).

lazy_read_lines(IoDev) ->
    fun() ->
	    [read_lines(IoDev, "")|lazy_read_lines(IoDev)]
    end.

read_lines(IoDev, A) ->
    case file:read(IoDev, 1) of
	eof ->
	    file:close(IoDev),
	    lists:reverse(A);
	{ok, "\n"} ->
	    lists:reverse(A);
	{ok, Data} ->
	    read_lines(IoDev, [Data|A]);
	{error, _} ->
	    end_of_file
    end.


%% O(n) time
max_sub_sum(File) ->
    FileStream = lazy_read_file(File),
    max_sub_sum(FileStream(), 0, 0, {[], []}).

max_sub_sum([end_of_file|_], GlobM, _, {NumberListPath, _}) ->
    {GlobM, lists:reverse(lists:flatten(NumberListPath))};
max_sub_sum([[]|L], GlobM, LocM, {NumberListPath, SkippedListPath}) ->
    max_sub_sum(["0"|L()], GlobM, LocM, {NumberListPath, SkippedListPath});
max_sub_sum([H|L], GlobM, LocM, {NumberListPath, SkippedListPath}) ->
    HNumber = list_to_number(H),
    if HNumber > (HNumber + LocM) ->
	    LM = HNumber,
	    LPath = [HNumber],
	    SkipPath = [];
       true ->
	    LM = HNumber + LocM,
	    LPath = [HNumber|NumberListPath],
	    SkipPath = SkippedListPath
    end,  
    if LM > GlobM ->
	    [HH|TT] = LPath,
	    max_sub_sum(L(), LM, LM, {[[HH|SkipPath]|TT], []});
       true ->		  
	    max_sub_sum(L(), GlobM, LM, {NumberListPath, [HNumber|SkipPath]})
    end.


%% K - minimum sub length
%% O(n) time (better refactor ++ operations)
max_sub_sum_withk(File, 0) ->
    max_sub_sum(File);
max_sub_sum_withk(File, K) ->
    FileStream = lazy_read_file(File),
    {Sum, ReadedList} = read_first_k(FileStream(), K, 0, []),
    read_after_k(FileStream(), K, {K, ReadedList}, Sum, Sum, Sum, {ReadedList, []}).


read_first_k([[]|_], K, _, _) when K > 0 ->
    {error, k_longer_than_list};
read_first_k([H|L], K, Sum, A) ->
    HNumber = list_to_number(H),
    if (K-1 == 0) ->
	    {Sum+HNumber, lists:reverse([HNumber|A])};
       true -> read_first_k(L(), K-1, Sum+HNumber, [HNumber|A])
    end.


read_after_k([end_of_file|_], _, _, _, _, Res, {NumberListPath, SkipPath}) ->
    {S, _} = lists:split(length(NumberListPath) - length(SkipPath), NumberListPath),
    {Res, S};
read_after_k([[]|_], _, _, _, _, Res, {NumberListPath, SkipPath}) ->
    {S, _} = lists:split(length(NumberListPath) - length(SkipPath), NumberListPath),
    {Res, S};
read_after_k([H|L], K, {ReadedListLength, ReadedList}, PrevSum, EndV, Res, {NumberListPath, SkippedListPath}) ->
    HNumber = list_to_number(H),
    PrevSumNew = PrevSum + HNumber - lists:nth(ReadedListLength - K + 1, ReadedList),
    if PrevSumNew  > (EndV + HNumber) ->
	    EndVNew = PrevSumNew,
	    {_,TailPath} = lists:split(length(NumberListPath) - K + 1, NumberListPath),
	    LPath = TailPath ++ [HNumber],
	    SkipPath = SkippedListPath;
       true ->
	    EndVNew = EndV + HNumber, 
	    LPath = NumberListPath ++ [HNumber],
	    SkipPath = SkippedListPath ++ [HNumber]
    end,
    if EndVNew > Res ->
	    read_after_k(L(), K, {ReadedListLength + 1, ReadedList ++ [HNumber]}, PrevSumNew, EndVNew, EndVNew,  {LPath, []});
       true ->
	    read_after_k(L(), K, {ReadedListLength + 1, ReadedList ++ [HNumber]}, PrevSumNew, EndVNew, Res, {LPath, SkipPath})
    end.


list_to_number(L) when is_list(L) ->
    list_to_number(L, 0).

list_to_number([], A) ->
    A;
list_to_number([" "|_], A) ->
    A;
list_to_number(["-"|L], _) ->
    listnumbers_nonempty(L, minus);
list_to_number(L, A) when is_list(L) ->
    listnumbers_to_number(L, A).

%% A - minus | number()
listnumbers_nonempty(L = [_|_], A) ->
    listnumbers_to_number(L, A);
listnumbers_nonempty(_, _) ->
    {error, non_empty_after_minus_required}.
%% A - minus | number()  
listnumbers_to_number([], A) ->
    A;
listnumbers_to_number([H|L], A) ->
    {N, _} = string:to_integer(H),
    case A of
	minus ->
	    if is_number(N) ->
		    list_to_number(L, -N);
	       true -> 
		    {error, parse_error_number_required}
	    end;
	_ ->
	    if is_number(N) ->
		    if (A < 0) ->
			    list_to_number(L, (A*10) - N);  
		       true ->
			    list_to_number(L, (A*10) + N)
		    end;
	       true -> 
		    {error, parse_error_number_required}
	    end
    end.

