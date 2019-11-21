-module(atm_serv).

-behaviour(gen_server).

-export([start_link/1]).
-export([
	 init/1, 
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-include("atm_net_strucs.hrl").

-record(state, {
		capacity,
		refs = [],
		socket,
		card_states = []
	       }). 


start_link(Socket) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Socket, []).


init(Socket) ->
    {ok, StorageFileName} = application:get_env(storage_file_name),
    CardStates = read_card_states_from_file(StorageFileName),
    CardStatesInRecord = card_info_list_into_record(CardStates),
    {ok, AtmCapacity} = application:get_env(capacity),
    gen_server:cast(self(), accept),
    {ok, #state{capacity = AtmCapacity, socket=Socket, card_states = CardStatesInRecord}}.


handle_call(
  {is_card_number_valid, CardNo, Pid}, 
  _From, 
  #state{card_states = CardStates} = State
 ) ->
    ReplyMsg = is_card_number_valid(CardNo, CardStates),
    case ReplyMsg of
	{
	 ok, 
	 #card_state{
	    card_no = CardNo,
	    pin = Pin,
	    balance = Balance, 
	    offset = Offset
	   } = Rec,
	 NewCardStates
	} ->
	    {reply, {ok,Rec}, State#state{
			   card_states = 
			       [#card_state{
				   card_no = CardNo,
				   pin = Pin,
				   balance = Balance, 
				   offset = Offset,
				   busy = Pid % just to mark busy assigned to pid
				  } 
				| NewCardStates]
			  }
	    };
	Atom ->
	    {reply, Atom, State}
    end;
handle_call(
  {update_balance, #card_state{card_no = CardNo} = CurrentCard, NewBalance},
  _From,
  #state{card_states = CardStates} = State
 ) ->
    CardStateChanged = update_storage_balance(NewBalance, CurrentCard),
    {value, _, ElseCardStates} = lists:keytake(CardNo, #card_state.card_no, CardStates),
    NewState = State#state{card_states = [CardStateChanged | ElseCardStates]},
    {reply, {ok, CardStateChanged}, NewState};
handle_call(
  {release_card, Pid}, 
  _From, 
  #state{card_states = CardStates} = State
 ) ->
    NewCardStates = release_busy(Pid, CardStates),
    {reply, ok, State#state{card_states = NewCardStates}}.
	   

handle_cast(accept, #state{socket = ListenSocket} = State) ->
    {_Pid, _Ref} = spawn_monitor(
		   fun () ->
			   {ok, AcceptedSocket} = gen_tcp:accept(ListenSocket),
			   gen_tcp:controlling_process(AcceptedSocket, whereis(?MODULE)),
			   gen_server:cast(?MODULE, {accepted_sock, AcceptedSocket})
		   end
		  ),
    {noreply, State};
handle_cast({accepted_sock, AcceptedSocket}, #state{capacity = Cap, refs = Refs} = State) ->
    case length(Refs) of
	L when L == Cap ->
	    send(AcceptedSocket, "Server capacity is full", []),
	    gen_tcp:close(AcceptedSocket),
	    gen_server:cast(self(), accept),
	    {noreply, State};
	_ ->
	    {ok, Pid} = atm_state:start_link(AcceptedSocket, self()), 
	    Ref = monitor(process, Pid),
	    gen_tcp:controlling_process(AcceptedSocket, Pid),
	    N = [{AcceptedSocket, Pid, Ref} | Refs],
	    gen_server:cast(self(), accept),
	    {noreply, State#state{refs = N}}
    end.


handle_info({'DOWN', Ref, process, Pid, _Reason}, S = #state{refs = Refs, card_states = CardStates}) ->
    demonitor(Ref),
    case lists:keytake(Ref, 3, Refs) of
	false ->
	    {noreply, S};
	{value, _, NewRefs} ->
	    NewCardStates = release_busy(Pid, CardStates),
	    {noreply, S#state{refs = NewRefs, card_states = NewCardStates}}
    end;
handle_info(_Event, State) ->
    {noreply, State}.    


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 

terminate(_Reason, _State) ->
    ok.


send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
%% expected that each line of text in file contains info about 1 card
%% card_no pin balance (example of 1 line: 1111 1234 500)
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
				     offset = Offset,
				     busy = false
				    }
			 | Acc] 
		end, [], CardInfoList).


%% check if cardno in a given card record list
is_card_number_valid(CardNo, CardStates) ->
    case lists:keytake(CardNo, #card_state.card_no, CardStates) of
	false ->
	    invalid_card;
	{value, #card_state{card_no = CardNo, busy = false} = Rec, NewCardStates}  ->
	    {ok, Rec, NewCardStates};
	_ ->
	    busy_card
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
		     "\\d",
		     "\n"
		   >>,
    ok = file:pwrite(IoDev, {bof, Offset}, LineToInsert),
    file:close(IoDev),
    State#card_state{balance = NewBalance}.



%% release busy and assign it to false
release_busy(Pid, CardStates) ->
    case lists:keytake(Pid, #card_state.busy, CardStates) of
	{value, Rec, ElseCardStates} ->
	    NewCardStates = [Rec#card_state{busy = false}|ElseCardStates ];
	_ ->
	    NewCardStates = CardStates
    end,
    NewCardStates.
