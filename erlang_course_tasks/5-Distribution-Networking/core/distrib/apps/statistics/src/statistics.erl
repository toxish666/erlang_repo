-module(statistics).

-export([
	 started_task/1,
	 failed_task/1,
	 completed_task/1,
	 restarted_task/1,
	 statistics_info_locally/0,
	 statistics_info/0
	]).

started_task(From) ->
    statistics_manager:started_task({From}).


failed_task(From) ->
    statistics_manager:failed_task({From}).


completed_task(From) ->
   statistics_manager:completed_task({From}).


restarted_task(From) ->
   statistics_manager:restarted_task({From}).


statistics_info_locally() ->
    statistics_manager:statistics_info_locally().


statistics_info() ->
    statistics_manager:statistics_info().
