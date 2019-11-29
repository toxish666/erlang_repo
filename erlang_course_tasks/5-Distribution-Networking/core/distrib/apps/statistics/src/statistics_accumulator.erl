-module(statistics_accumulator).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
         terminate/2]).

-record(state, {
		started = 0,
		failed = 0,
		completed = 0,
		restarted = 0
	       }).

-define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).


init([]) ->
    rd:add_local_resource(?MODULE, self()),
    rd:add_target_resource_type(?MODULE),
    rd:trade_resources(),
    {ok, #state{}}.


handle_event({started_task, _From}, #state{started = N} = State) ->
    {ok, State#state{started = N + 1}};
handle_event({failed_task, _From}, #state{failed = N} = State) ->
    {ok, State#state{failed = N + 1}};
handle_event({completed_task, _From}, #state{completed = N} = State) ->
    {ok, State#state{completed = N + 1}};
handle_event({restarted_task, _From}, #state{restarted = N} = State) ->
    {ok, State#state{restarted = N + 1}}.


handle_call({statistics_info_locally}, State) ->
    {ok, ?record_to_tuplelist(state, State), State};
%% collecting statistics asynchronously
handle_call({statistics_info}, State) ->
    {ok, Accumulators1} = rd:fetch_resources(?MODULE),
    Accumulators = lists:filter(fun(X) -> X =/= self() end, Accumulators1),
    PromisesList = lists:map(fun(A) -> 
				     rpc:async_call(node(A), statistics_manager,
						    statistics_info_locally, [])
			     end, Accumulators),
    ResListRemote = lists:map(fun(P) -> rpc:yield(P) end, PromisesList),
    ResLocal = ?record_to_tuplelist(state, State),
    ResList = [ResLocal| ResListRemote],
    EmptyState = #state{},
    Folded = lists:foldl(
	       fun(
		 [{_,S},{_,F},{_,C},{_,R}],
		 [{MsgS,SA},{MsgF,FA},{MsgC,CA},{MsgR,RA}]
		) ->
		       [{MsgS, SA+S},{MsgF, FA+F},{MsgC, CA+C},{MsgR, RA+R}]
	       end,
	       ?record_to_tuplelist(state, EmptyState),
	       ResList
	      ),
    {ok, Folded, State}.


handle_info(_, State) ->
    {ok, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_Reason, _State) ->
    ok.
