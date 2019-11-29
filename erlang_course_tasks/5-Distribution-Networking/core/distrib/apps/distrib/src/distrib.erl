-module(distrib).

-export([
         load_tasks_list_internal/0,
	 initiate_loaded_work/0,
	 load_and_initiate/0
	]).


load_tasks_list_internal() ->
    distrib_serv:load_tasks_list_internal().


initiate_loaded_work() ->
    distrib_serv:initiate_loaded_work().


load_and_initiate() ->
     distrib_serv:load_and_initiate().
