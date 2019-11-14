-module(bank_serv).

-behaviour(gen_server).

-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
         code_change/3, 
	 terminate/2
	]).

-export([
	 start_link/1,
	 start_atm/1,
	 stop_atm/1
	]).


-define(BANK_SUPERSUPERVISOR, bank_otp_supersup).
-define(ATM_SUPERVISOR, atm_sup).
-define(ATM_NODE, atm_node).
%% Dynamically started atm_sup
-define(SPEC_ATMSUP,
        #{
	  id => ?ATM_SUPERVISOR,
	  start => {?ATM_SUPERVISOR, start_link, [self()]},
	  restart => permanent,
	  shutdown => 10000,
	  type => supervisor,
	  modules => [?ATM_SUPERVISOR]
	}
       ).

-define(SPEC_ATMNODE(Name),
	#{
	  id => Name,
	  start => {?ATM_NODE, start_link, [Name]},
	  restart => permanent,
	  shutdown => 3000,
	  type => worker,
	  modules => [?ATM_NODE]
	 }
       ).


-record (state, {
		 card_states,
		 atps = []
		}).


start_atm(Name) ->
    supervisor:start_child(?ATM_SUPERVISOR, ?SPEC_ATMNODE(Name)).

stop_atm(Name) ->
    supervisor:terminate_child(?ATM_SUPERVISOR, Name),
    supervisor:delete_child(?ATM_SUPERVISOR, Name).

start_link(Sup) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Sup, []).


init(Sup) ->
    {ok, StorageFileName} = application:get_env(storage_file_name),
    CardStates = read_card_states_from_file(StorageFileName),
    self() ! {start_atp_supervisor, CardStates, Sup},
    {ok, #state{card_states = CardStates}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call({run, Args}, _From, State) ->
    {reply, noalloc, State}.

handle_cast({run, Args}, State) ->
    {noreply, State}.


handle_info({start_atp_supervisor, CardStates, Sup}, State) ->
    {ok, _} = supervisor:start_child(Sup, ?SPEC_ATMSUP),
    {noreply, State#state{card_states = CardStates}};
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.


% expected that each line of text in file contains info about 1 card
%% card_no pin balance (example of 1 line: 1 1234 500)
read_card_states_from_file(StorageFileName) ->
    {ok, IoDev} = file:open(StorageFileName, [read]),
    CardStates = parse_file(IoDev),
    file:close(IoDev),
    CardStates.

parse_file(IoDev) ->
    case file:read_line(IoDev) of
	eof ->
	    [];
	{ok, CardInfo} ->
	    LexemesList = string:lexemes(string:trim(CardInfo), "x e" ++ [[$\r,$\n]]),
	    IntList = lists:map(fun (S) -> 
					{Num, _} = string:to_integer(S), 
					Num 
				end, LexemesList),
	    [list_to_tuple(IntList)] ++ parse_file(IoDev)
    end.
