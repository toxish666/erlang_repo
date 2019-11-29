-module(statistics_manager).

-export([
	 start_link/0,
	 started_task/1,
	 failed_task/1,
	 completed_task/1,
	 restarted_task/1
	]).
-export([statistics_info_locally/0, statistics_info/0]).


start_link() ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, statistics_accumulator, []),
    register(?MODULE, Pid),
    {ok, Pid}.


started_task({From}) ->
    gen_event:notify(?MODULE, {started_task, From}).


failed_task({From}) ->
    gen_event:notify(?MODULE, {failed_task, From}).


completed_task({From}) ->
    gen_event:notify(?MODULE , {completed_task, From}).


restarted_task({From}) ->
    gen_event:notify(?MODULE, {restarted_task, From}).


statistics_info_locally() ->
    gen_event:call(?MODULE, statistics_accumulator, {statistics_info_locally}).


statistics_info() ->
    gen_event:call(?MODULE, statistics_accumulator, {statistics_info}).
