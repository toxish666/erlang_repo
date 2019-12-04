%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @doc This module wrapping mdb module in erlang_course_tasks/2-advanced/ folder with gen_server behaviour.
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/4-OTP.md#41-db-on-gen_server"> task itself </a> for more info.
%% 
%% it's necessary to have beam file of mdb module in codepath! 
%% code:add_path("../../../2-advanced").
%% plus
%% code:add_path("../../../1-basic").
%%

-module(mdb_server).

-behaviour(gen_server).

%%----------------------------------------------------------------------------
%% BEHAVIOUR EXPORTS
%%----------------------------------------------------------------------------
-export([
	 init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 new/1,
         delete/1,
         delete/2,
         delete_all_objects/1,
         insert/3,
         find/2
	]).


%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
-spec new(Name :: atom()) -> ok |{error, Reason :: term()}.
new(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).


-spec delete(Name :: atom()) -> ok.
delete(Name) ->
    gen_server:stop(Name).


-spec delete(Name :: atom(), Key :: term()) -> ok.
delete(Name, Key) ->
    gen_server:cast(Name, {delete, Key}).


-spec delete_all_objects(Name :: atom()) -> ok.
delete_all_objects(Name) ->
    gen_server:cast(Name, delete_all_objects).


-spec insert(Name :: atom(), Key :: term(), Value :: term()) -> ok.
insert(Name, Key, Value) ->
    gen_server:cast(Name, {insert, Key, Value}).


-spec find(Name :: atom(), Key :: term()) -> {ok, Value :: term()} | not_found.
find(Name, Key) ->
    gen_server:call(Name, {find, Key}).

%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init(_) ->
    {ok, mdb:new()}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
handle_call({find, Key}, _From, Db) ->
    case mdb:read(Key, Db) of
	{error, not_found} = NotFound ->
	    {reply, NotFound, Db};
	{ok, _Element} = Found ->
	    {reply, Found, Db}
    end.


handle_cast({delete, Key}, Db) ->
    {noreply, mdb:delete(Key, Db)};
handle_cast(delete_all_objects, Db) ->
    {P, _Tree} = Db,
    {noreply, {P, {}}};
handle_cast({insert, Key, Element}, Db) ->
    case mdb:write(Key, Element, Db) of
	{error, append_deny, _} ->
	    {noreply, Db};
	NewDb ->
	    {noreply, NewDb}
    end.


handle_info(_, State) ->
    {noreply, State}.
