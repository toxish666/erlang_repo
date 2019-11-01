-module(pdb).
%% add code:add_path("../2-advanced")
-export([new/0,
	 new/1,
	 destroy/1,
	 write/3,
	 delete/2,
	 read/2,
	 match/2,
	 append/3,
	 batch_delete/2,
	 batch_read/2
	]).


loop(Db) ->
    receive
	{Pid, FuncNameAtom, Args} ->
	    try apply(mdb, FuncNameAtom, Args ++ [Db]) of
		ok -> %% mdb:destroy(Db) call returns only ok
		    exit;
		{error, Reason, _} ->
		    Pid ! {self(), {error, Reason}};
		{error, Reason} ->
		    Pid ! {self(), {error, Reason}};
		{ok, Element} ->
		    Pid ! {self() , {ok, Element}};
		{Params, Tree} = NewState when is_list(Params), is_tuple(Tree)  ->
		    Pid ! {self(), {updated_db, NewState}},
		    loop(NewState);
	        AnswerList when is_list(AnswerList) ->
		    Pid ! {self(), {ok, AnswerList}}		
	    catch
		error:Error ->
		    Pid ! {error, undefined_function_in_mdb, Error}
	    end,
	    loop(Db)
    end.

new(P) ->
    spawn(fun() ->
		  loop(mdb:new(P))
	  end).

new() ->
    spawn(fun() ->
		  loop(mdb:new())
	  end).


run_on_database_alive(DbP, Lmd) ->
    case is_process_alive(DbP) of
	false ->
	    {error, database_down_error};
	true ->
	    Lmd()
    end.

destroy(DbP) ->
    run_on_database_alive(DbP, fun() ->
				       destroy_impl(DbP)
			       end).

write(K, E, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       write_impl(K, E, DbP)
			       end).

delete(K, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       delete_impl(K, DbP)
			       end).

read(K, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       read_impl(K, DbP)
			       end).

match(E, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       match_impl(E, DbP)
			       end).

append(K, V, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       append_impl(K, V, DbP)
			       end).

batch_delete(K, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       batch_delete_impl(K, DbP)
			       end).

batch_read(K, DbP) ->
    run_on_database_alive(DbP, fun() ->
				       batch_read_impl(K, DbP)
			       end).


destroy_impl(DbP) ->
    DbP ! {self(), destroy, []},
    recv_anything().
    
write_impl(K, E, DbP) ->
    DbP ! {self(), write, [K, E]},
    recv_anything().

delete_impl(K, DbP) ->
    DbP ! {self(), delete, [K]},
    recv_anything().

read_impl(K, DbP) ->
    DbP ! {self(), read, [K]},
    recv_anything().

match_impl(E, DbP) ->
    DbP ! {self(), match, [E]},
    recv_anything().

append_impl(K, E, DbP) ->
    DbP ! {self(), append, [K,E]},
    recv_anything().

batch_delete_impl(KL, DbP) ->
    DbP ! {self(), batch_delete, [KL]},
    recv_anything().

batch_read_impl(KL, DbP) ->
    DbP ! {self(), batch_read, [KL]},
    recv_anything().



recv_anything() ->
    receive
	Anything ->
	    Anything
    after 1000 ->
	    {error, timeout_error}
    end.
