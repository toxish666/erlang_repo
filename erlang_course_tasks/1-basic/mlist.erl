-module(mlist).
-export([create/1,
	 create_reverse/1,
	 print_seq/1,
	 print_seq_odd/1,
	 filter/2,
	 concatenate/1,
	 flatten/1,
	 dna_to_rna/1,
	 cut_rdna/2
]).


%% 1. Напишите функцию create/1, которая на вход принимает число N и возвращает список вида [1, 2,..., N -1, N].
create(N) when is_integer(N), N > 0 ->
    create(N, []).

create(0, A) ->
    A;
create(N, A) ->
    create(N-1, [N|A]).


%% 2. Напишите функцию, которая также принимает число N, но возвращает список вида [N, N-1, ..., 2, 1].
create_reverse(N) when is_integer(N), N > 0 ->
    create_reverse(N, []).

create_reverse(0, A) ->
    A;
create_reverse(N, A) ->
    create_reverse(N-1, A ++ [N]).



%% 3. Напишите функцию, которая распечатывает все числа от 1 до N.
print_seq(0) ->
    erlang:display(0);
print_seq(N) when is_integer(N), N > 0 ->
    erlang:display(N),
    print_seq(N-1).


%% 4. Напишите функцию, которая распечатывает все нечётные числа от 1 до N.
print_seq_odd(1) ->
     erlang:display(1);
print_seq_odd(N) when is_integer(N), N > 0, N band 1 == 1 ->
    print_seq_with_2_increment(N);
print_seq_odd(N) when is_integer(N), N > 0 ->
    print_seq_with_2_increment(N - 1).

print_seq_with_2_increment(1) ->
    erlang:display(1);
print_seq_with_2_increment(N) ->
    erlang:display(N),
    print_seq_with_2_increment(N-2).


%% 5. Напишите функцию, которая принимает на вход список целых чисел и одно целое число, а возвращает список всех элементов списка, которые меньше либо равны числу, переданному вторым аргументом. 
filter(LL = [H|L], V) when is_list(L), is_integer(H), is_integer(V) -> 
    [ E || E <- LL, E =< V ].


%% 6. Напишите функцию, которая распечатывает все нечётные числа от 1 до N.
reverse(L) -> reverse(L,[]).

reverse([],A) -> 
    A;
reverse([H|T],A) -> 
    reverse(T,[H|A]).


%% 7.Напишите функцию, которая преобразует список списков в один список, соединяя все списки-элементы.
concatenate(L) when is_list(L) ->
    concatenate(L, []).

concatenate([], A) ->
    reverse(A);
concatenate([H|T], A) when is_list(H) ->
    concatenate(T, add_to_acc(H,A));
concatenate([H|T], A) ->
    concatenate(T, [H|A]).

add_to_acc([], A) ->
    A;
add_to_acc([H|T], A) ->
    add_to_acc(T, [H|A]).


%% 8. Напишите функцию, которая по списку вложенных списков строит линейный список.
flatten(L) when is_list(L) ->
    flatten(L, []).

flatten([], A) ->
    reverse(A);
flatten(L = [H|_], A) when is_list(H) ->
    flatten(concatenate(L), A); 
flatten([H|T], A) ->
    flatten(T,[H|A]).


%% 9. Напишите функцию, которая принимает на вход последовательность нуклеотидов ДНК и выдает комплементарную ей цепочку нуклеотидов РНК. 
dna_to_rna_map() ->
    #{g => c, "G" => c,
      c => g, "C" => g,
      t => a, "T" => a,
      a => u, "A" => u}.

dna_to_rna(L) when is_list(L) ->
    dna_to_rna(L, [], dna_to_rna_map()).

dna_to_rna([], A, _) ->
    reverse(string:uppercase(A));
dna_to_rna([H|L], A, M) ->
    try maps:get(H, M) of
	Atom -> dna_to_rna(L,[erlang:atom_to_list(Atom)|A], M)
    catch 
	_:{badkey, _} -> error
    end. 


%% 10. Напишите функцию, которая из заданной цепочки РНК/ДНК вырезает заданную последовательность из трех нуклеотидов.
%% Modification: there can be more/less than 3 nucleotides. Input still is available as "ATTG" or as [a,t,t,g].
is_dna([]) ->
    true;
is_dna([H|L]) when (not is_atom(H)) 
		   andalso (H == hd("G") 
			    orelse (H == hd("C"))
			    orelse (H == hd("T"))
			    orelse (H == hd("A"))
			    ) ->
    is_dna(L);
is_dna([H|L]) when (is_atom(H))
		   andalso ((H == g)
			    orelse (H == c)
			    orelse (H == t)
			    orelse (H == a)
			    ) ->
    is_dna(L);
is_dna(_) ->
    false.

is_rna([]) ->
    true;
is_rna([H|L]) when (not is_atom(H)) 
		   andalso (H == hd("C") 
			    orelse (H == hd("G"))
			    orelse (H == hd("A"))
			    orelse (H == hd("U"))
			    ) ->
    is_rna(L);
is_rna([H|L]) when (is_atom(H))
		   andalso ((H == c)
			    orelse (H == g)
			    orelse (H == a)
			    orelse (H == u)
			    ) ->
    is_rna(L);
is_rna(_) ->
    false.

is_both_dna_or_rna(L, LC) ->
    case is_dna(L) of
	true ->
	    case is_dna(LC) of
		true ->
		    true;
		_ ->
		    false
	    end;
	 _ ->
	    case is_rna(L) of
		true ->
		    case is_rna(LC) of
			true ->
			    true;
			_ ->
			    false
		    end;
		_ ->
		    false
	    end
    end.

cut_rdna(L, LC) ->
    IsLegal = is_both_dna_or_rna(L, LC),
    if IsLegal == true ->
	    cut_rdna(L, LC, [], LC, []);
       true ->
	    'incorrect entered data!'
    end.

%% L - given list of rdna s
%% LC - given list of rdna s for matching
%% ACCL - resulting accumulator
%% 4th argument - same as LC, but it is dissected for pattern matching with given list(L)
%% AccInSeq - accumulator for the case when we are in matching loop   
cut_rdna([], _, ACCL, [], _) ->
    flatten(reverse(ACCL));
cut_rdna([], _, ACCL, _, AccInSeq) ->
    flatten(reverse([AccInSeq|ACCL]));
cut_rdna(L, _, ACCL, [], _) ->
    flatten([reverse(ACCL)|L]);
cut_rdna(L = [H|_], LC, ACCL, [HC|_], AccInSeq) when not (H == HC)
						     andalso not (AccInSeq == []) ->
    cut_rdna(L, LC, [reverse(AccInSeq)|ACCL], LC, []);
cut_rdna([H|T], LC, ACCL, [HC|_], _) when not (H == HC) ->
    cut_rdna(T, LC, [H|ACCL], LC, []);
cut_rdna([H|T], LC, ACCL, [HC|TC], AccInSeq) when H == HC ->
    cut_rdna(T, LC, ACCL, TC, [HC|AccInSeq]).
