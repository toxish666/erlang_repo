-module(lockman).
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
	 start_link/1
	]).


%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
-record(state, {
	        server 
	       }).

%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
start_link(Server) ->
    gen_server:start_link(?MODULE, [Server], []).


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([Server]) ->
    {ok, #state{server = Server}}.

terminate(_Reason, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
handle_call(_, _From, State) ->
    {reply, {error, wrong_request}, State}.


handle_cast(_, State) ->
    {noreply, State}.


handle_info(
  {tcp, Socket, <<"acquire:", AtomInBinary/binary>> = _Str},
  State
 ) ->
    routine(Socket, AtomInBinary, acquire, State, call);
handle_info(
  {tcp, Socket, <<"wait:", AtomInBinary/binary>> = _Str},
  State
 ) ->
    routine(Socket, AtomInBinary, wait, State, call);
handle_info(
  {tcp, Socket, <<"fire:", AtomInBinary/binary>> = _Str},
  State
 ) ->
    routine(Socket, AtomInBinary, fire, State, cast);
handle_info(
  {tcp, Socket, <<"release:", AtomInBinary/binary>> = _Str},
  State
 ) ->   
    routine(Socket, AtomInBinary, release, State, cast);
handle_info(
  {tcp_closed, _Socket}, 
  _State
 ) ->
    {stop, normal, _State};
handle_info(_, State) ->
    {noreply, State}.
    

%%----------------------------------------------------------------------------
%% HELPER FUNCTIONS
%%----------------------------------------------------------------------------
send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.


routine(Socket, AtomInBinary, AtomRoutineT, #state{server = Server} = State, CastCall) ->
    AtomRoutine = if AtomRoutineT == release ->
			  {release, self()};
		     true ->
			  AtomRoutineT
		  end,
    AtomList = lists:takewhile(
		 fun(E) ->
			 E =/= 13 % 13 is codepoint of \r
		 end,
		 binary_to_list(AtomInBinary)
		),
    AtomKey = list_to_atom(AtomList),
    case CastCall of 
	cast ->
	    ServerAnswer = gen_server:cast(Server, {AtomRoutine, AtomKey});
	call ->
	    ServerAnswer = gen_server:call(Server, {AtomRoutine, AtomKey}, infinity)
    end,
    case {ServerAnswer, AtomRoutine} of 
	{ok, {_, _}} ->
	    send(Socket, "Trying to release key ~p", [AtomKey]);
	_ ->
	    send(Socket, "Succesfully ~p key ~p", [AtomRoutine, AtomKey])
    end,
    {noreply, State}.
