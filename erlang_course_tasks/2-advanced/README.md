### Упражнения


#### 2.0. База данных
- [mdb.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/2-advanced/mdb.erl)
Basic functions with given interface were implemented.

#### 2.1. Новая функциональность в базе данных
- [mdb.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/2-advanced/mdb.erl)
Function new were improved. Functions batch_delete, batch_read were added. 

#### 2.2 Сделайте так, чтобы база данных работала с JSON объектами, реализованными ранее в п.1.8
- [mdb.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/2-advanced/mdb.erl)
Basic functions were improved to work with JSON spec. Function mjson_to_md was added. Tests were added. 

#### 2.3 Lambda-вычисления

- task1 
```erlang
lmd() ->
	fun Bimap(Op, [H1|L1], [H2|L2]) ->
			[Op(H1, H2)|Bimap(Op, L1, L2)];
		Bimap(_, [], _) ->
			[];
		Bimap(_, _, []) ->
			[]
	end.

Lmd = lmd().
Lmd(fun erlang:'+'/2, [1,2,3], [2,4,5]).
Lmd(fun (A,B) -> 2/(1/A + 1/B) end, [1,2,3], [2,4,5]).
```

- task2 
```erlang
S = [
	 {dimension, 5},
	 {dotsA, [{1, 2, 3, 4, 5}, {7, 8, 9, 10, 11}]},
	 {dotsB, [{0, 0, 0, 0, 0}, {-1, -2, -3, -4, -5}]}
	],

BiF = fun BiFold(Op, [H1|L1], [H2|L2], A) ->
		  BiFold(Op, L1, L2, A + Op(H1, H2));
	      BiFold(_, [], _, A) ->
		  A;
	      BiFold(_, _, [], A) ->
		  A
	  end,

Res = fun ([{dimension, N}, {dotsA, LA}, {dotsB, LB}]) ->
		  [[
		    case {length(tuple_to_list(L1)), length(tuple_to_list(L2))} of
			{N, N} -> math:sqrt(BiF(
					      fun (A, B) ->
						      math:pow(A-B,2) 
					      end, tuple_to_list(L1), tuple_to_list(L2), 0));
			_      -> {error, incorrect_dimension}
		    end
		    || L1 <- LA, 
		       L2 <- LB

		   ]] 
	  end,

Res(S).
```

#### 2.4 Библиотечные функции
- task1 
```erlang
bi_with_map() ->
    fun (Op, L1, L2) ->
	    L1Z = lists:zip(L1, lists:seq(1, length(L1))),
	    lists:map(fun ({E1, I1}) ->
			      Op(E1, lists:nth(I1,L2)) 
		      end, L1Z)
    end.
```

- task2 
in the example above BiF should be substituted to the following:
(and 0 excluded as BiWithFold takes only 3 arguments, not 4)
```erlang
BiWithFold = fun (Op, L1, L2) ->
			 L1Z = lists:zip(L1, lists:seq(1, length(L1))),
			 lists:foldl(fun ({E1, I1}, Sum) ->
					     Sum + Op(E1, lists:nth(I1,L2)) 
				     end, 0, L1Z)
		 end
```

- additional
```erlang
filtermap(F, L) ->
    lists:reverse(lists:foldl(fun(E, A) ->
			case F(E) of
			    true ->
				[E|A];
			    false ->
				A;
			    {true, V} ->
				[V|A]
			end
		end, [], L)).
```

#### 2.5 Ленивые вычисления
- [mlazy.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/2-advanced/mlazy.erl)

- Right fold is imposible to implement with lambdas due to the reason that we are obliged to get to the end of a list in order to begin to calculate accumulating values. By doing so, we evaluate lambdas eliminating any laziness whatsoever. 
It is still possible to implement lazy_map or lazy_filter with lazy_foldl though with 2 drawbacks: these functions return only when they are eveluated to the bottom and they return reversed lists.
```erlang
lazy_map2(_, []) ->
    fun() ->
	    []
    end;
lazy_map2(F, L = [_|_]) ->
    lazy_foldl(fun (H, A) -> [F(H)|A] end, [], L).
```

- Lazy concatenation preserves laziness of both lists and evaluates elements when it needed exactly. So lazy concatenation does not traverse first list before prepending second one, that's the benefit.

- All other tasks are available here: [mlazy.erl](https://github.com/toxish666/erlang_repo/blob/master/erlang_course_tasks/2-advanced/mlazy.erl)

