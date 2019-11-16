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
	 stop_atm/1,
	 insert_card/2,
	 push_button/2
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
%supervisor:start_child(atm_sup, {fiva, {atm_node, start_link, [fiva, self()]}, permanent, 3000, worker, [atm_node]}).
-define(SPEC_ATMNODE(Name),
	#{
	  id => Name,
	  start => {?ATM_NODE, start_link, [Name, self()]},
	  restart => permanent,
	  shutdown => 3000,
	  type => worker,
	  modules => [?ATM_NODE]
	 }
       ).


-record(card_state, {
		     card_no,
		     pin,
		     balance,
		     %% offset from beggining in the store file
		     offset
		    }).

-record (state, {
		 atm_sup,
		 card_states = [],
		 atms = []
		}).


start_atm(Name) ->
    gen_server:cast(?MODULE, {start_atm, Name}).

stop_atm(Name) ->
    gen_server:cast(?MODULE, {stop_atm, Name}).

%% dupicating functions call from atm_node
%% it making call on atm remotely
insert_card(Name, CardNo) ->
    atm_node:insert_card(Name, CardNo).

push_button(Name, Button) ->
    atm_node:push_button(Name, Button).

start_link(Sup) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Sup, []).


init(Sup) ->
    {ok, StorageFileName} = application:get_env(storage_file_name),
    CardStates = read_card_states_from_file(StorageFileName),
    CardStatesInRecord = card_info_list_into_record(CardStates),
    self() ! {start_atp_supervisor, CardStatesInRecord, Sup},
    {ok, #state{card_states = CardStatesInRecord}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% basically requests from atp terminals
handle_call(
  {is_card_number_valid, CardNo}, 
  _From, 
  #state{card_states = CardStates} = State
 ) ->
    {reply, is_card_number_valid(CardNo, CardStates), State}; 
%% 1.update file storage; 2.update record in state; 3.return changed card
handle_call(
   {update_balance, #card_state{card_no = CardNo} = CurrentCard, NewBalance},
  _From,
  #state{card_states = CardStates} = State
) ->
    CardStateChanged = update_storage_balance(NewBalance, CurrentCard),
    {value, _, ElseCardStates} = lists:keytake(CardNo, #card_state.card_no, CardStates),
    NewState = State#state{card_states = [CardStateChanged | ElseCardStates]},
    {reply, {ok, CardStateChanged}, NewState}.


handle_cast(
  {start_atm, Name}, 
  #state{atm_sup = AtmSup, atms = Atms} = State
 ) ->
    StartChildRes = supervisor:start_child(AtmSup, [Name]),
    case StartChildRes of 
	{ok, AtmPid} ->
	    {noreply, State#state{atms = [{Name, AtmPid} | Atms]}};
	_ ->
	    {noreply, State}
    end;
handle_cast(
  {stop_atm, Name}, 
  #state{atm_sup = AtmSup, atms = Atms} = State
 ) ->
    case proplists:get_value(Name, Atms) of 
	Pid when is_pid(Pid) ->
	    supervisor:terminate_child(AtmSup, Pid),
	    {noreply, State#state{atms = proplists:delete(Name, Atms)}};
	_ ->
	    {noreply, State}
    end.


handle_info({start_atp_supervisor, CardStates, Sup}, State) ->
    case supervisor:start_child(Sup, ?SPEC_ATMSUP) of
	{ok, AtmSup} ->
	    {noreply, State#state{card_states = CardStates, atm_sup = AtmSup}};
	{error, {already_started, AtmSup}} ->
	    {noreply, State#state{card_states = CardStates, atm_sup = AtmSup}}
    end;
handle_info(Msg, State) ->
    io:format("Unknown msg: ~p~n", [Msg]),
    {noreply, State}.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
%% expected that each line of text in file contains info about 1 card
%% card_no pin balance (example of 1 line: 1 1234 500)
read_card_states_from_file(StorageFileName) ->
    {ok, IoDev} = file:open(StorageFileName, [read]),
    CardStates = parse_file(IoDev),
    file:close(IoDev),
    CardStates.


%% when updating the config file, inserted line might be shorter than 
%% the old one, so at the end of updated line will be placed \d character
parse_file(IoDev) ->
    {ok, PosInFile} = file:position(IoDev, cur),
    case file:read_line(IoDev) of
	eof ->
	    [];
	{ok, CardInfo} ->
	    LexemesList = string:lexemes(string:trim(CardInfo), "x e" ++ [[$\r,$\n]] ++ "\\d"),
	    IntList = lists:map(fun (S) -> 
					{Num, _} = string:to_integer(S), 
					Num 
				end, LexemesList),
	    {IntListToRecord, _} = lists:split(3, IntList),
	    [list_to_tuple(IntListToRecord ++ [PosInFile])] ++ parse_file(IoDev)
    end.


%% from card infos to card records
card_info_list_into_record(CardInfoList) ->
    lists:foldr(fun({CardNo, Pin, Balance, Offset}, Acc) ->
			[#card_state{card_no = CardNo, 
				     pin = Pin,
				     balance = Balance,
				     offset = Offset
				    }
			 | Acc] 
		end, [], CardInfoList).


%% check if cardno in a given card record list
is_card_number_valid(CardNo, CardStates) ->
    case lists:keyfind(CardNo, #card_state.card_no, CardStates) of
	false ->
	    invalid_card;
	#card_state{card_no = CardNo, pin = ExpectedPin, balance = CurrentBalance, offset = Offset}  ->
	    {ok, {CardNo, ExpectedPin, CurrentBalance, Offset}}
    end.


%% updating the file storage
update_storage_balance(NewBalance, #card_state{card_no = CardNo, pin = Pin, offset = Offset} = State) ->
    {ok, StorageFileName} = application:get_env(storage_file_name),
    {ok, IoDev} = file:open(StorageFileName, [read, write]),
    LineToInsert = <<
		     (integer_to_binary(CardNo))/binary,
		     " ",
		     (integer_to_binary(Pin))/binary,
		     " ",
		     (integer_to_binary(NewBalance))/binary,
		     "\\d"
		   >>,
    ok = file:pwrite(IoDev, {bof, Offset}, LineToInsert),
    file:close(IoDev),
    State#card_state{balance = NewBalance}.
    
    
    
		     
		     
