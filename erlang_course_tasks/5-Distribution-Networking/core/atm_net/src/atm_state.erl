-module(atm_state).

-behaviour(gen_statem).

%%----------------------------------------------------------------------------
%% MACROS
%%----------------------------------------------------------------------------
-define(NAME, single_atm).

-define(TIMEOUTINSEC, 10000).
-define(TIMEOUT, {state_timeout, ?TIMEOUTINSEC, timeout_error}).

-define(MAXATTEMPTS, 3).

%%----------------------------------------------------------------------------
%% GEN_STATEM CALLBACKS
%%----------------------------------------------------------------------------
-export([
	 init/1,
	 terminate/3,
	 code_change/3,
	 callback_mode/0
	]).

%%----------------------------------------------------------------------------
%% STATES
%%----------------------------------------------------------------------------
-export([
	 handle_common/3,
	 waiting_card/3,
	 waiting_pin/3,
	 choose_deposit_or_withdraw/3,
	 withdraw/3,
	 deposit/3
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 start_link/1,
	 insert_card/1,
	 push_button/1,
	 stop_atm/0
	]).

%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
-record(card_state, {
		     card_no,
		     pin,
		     balance
		    }).

-record(state, {
		card_states = [],
		current_card = #card_state{},
		current_input = [],
		attempts = 0
	       }
       ).

%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
-spec start_link([{CardNo :: integer(), Pin :: list(integer()), Balance :: integer()}])
		-> {ok, pid()} | {error, any()}.
start_link(CardInfoList) ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, CardInfoList, []).


-spec insert_card(CardNo :: integer()) -> ok  | {error, Reason :: term()}.
insert_card(CardNo) ->
    gen_statem:call(?NAME, {insert_card, CardNo}).


-spec push_button(Button ::  enter | cancel | withdraw | deposit |
			     0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9) -> 
			 continue | {ok, Result :: term()} | {error, Reason :: term()}.
push_button(Button) ->
    gen_statem:call(?NAME, {push_button, Button}).


-spec stop_atm() -> ok.
stop_atm() ->
    gen_statem:stop(?NAME).

%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init(CardInfoList) ->
    {
     ok, 
     waiting_card, 
     #state{
	card_states = card_info_list_into_record(CardInfoList)
       }
    }.


terminate(_, _, _) ->
    ok.


code_change(_, State, _) ->
    {ok, State}.


callback_mode() ->
    state_functions.

%%----------------------------------------------------------------------------
%% STATES
%%----------------------------------------------------------------------------
waiting_card(
  {call, From}, 
  {insert_card, CardNo}, 
  #state{ card_states = CardStates } = State
 ) ->
    case is_card_number_valid(CardStates, CardNo) of
	%% no cardNo in cardStates were found
	invalid_card ->
	    {
	     keep_state, 
	     State, 
	     [{reply, From, {error, 'incorrect card number was given'}}]
	    };
	{ok, {CardNo, ExpectedPin, CurrentBalance}}  ->
	    {
	     next_state, 
	     waiting_pin, 
	     State#state{current_card = 
			     #card_state{
				card_no = CardNo, 
				pin = ExpectedPin, 
				balance = CurrentBalance
			       }
			},
	     [
	      {reply, From, {ok, waiting_for_pin}},
	      ?TIMEOUT
	     ]
	    }
    end;
waiting_card(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
waiting_pin(
  {call, From}, 
  {push_button, enter}, 
  #state{
     current_card = #card_state{pin = ExpectedPin},
     current_input = CurrentInput,
     attempts = Attempts
    } = State
 ) when Attempts =< ?MAXATTEMPTS ->
    EnteredPin = input_to_number(CurrentInput),
    case EnteredPin == ExpectedPin of
	true ->
	    {
	     next_state, choose_deposit_or_withdraw, State#state{current_input = []},
	     [{reply, From, {ok, valid_pin}}]
	    };
	false ->
	    %%----attempts are exhausted
	    if Attempts == ?MAXATTEMPTS - 1 ->
		    {
		     next_state,
		     waiting_card,
		     reset_current_state(State),
		     [{reply, From, {error, 'you exceeded the number of attempts'}}]
		    };
	       %% next attempt
	       true ->
		    {
		     keep_state, 
		     State#state{current_input = [], attempts = Attempts + 1}, 
		     [
		      {reply, From, {error, 'incorrect pin, try again'}},
		      ?TIMEOUT
		     ]
		    }
	    end
    end;
%% number buttons pressed
waiting_pin({call, From}, {push_button, Number}, #state{current_input = CurrentInput} = State) 
  when Number >= 0 andalso Number =< 9 ->
    {
     keep_state,
     State#state{current_input = CurrentInput ++ [Number]},
     [
      {reply, From, continue},
      ?TIMEOUT
     ]
    };
%% timeout exception
waiting_pin(state_timeout, _Reason, State) ->
    io:format("state_timeout in pin state ~n"),
    {
     next_state, 
     waiting_card, 
     reset_current_state(State)
    };
waiting_pin(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% choose next action after correct pin was entered
choose_deposit_or_withdraw(
  {call, From},
  {push_button, withdraw},
  State
 ) ->
    {next_state, withdraw, State, [{reply, From, {ok, withdraw}}]};
choose_deposit_or_withdraw(
  {call, From},
  {push_button, deposit},
  State
 ) ->
    {next_state, deposit, State, [{reply, From, {ok, deposit}}]};
choose_deposit_or_withdraw(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
withdraw(
  {call, From},
  {push_button, enter},  
  #state{
     current_card = CurrentCard,
     current_input = CurrentInput
    } = State
 ) ->
    SumToWithdraw = input_to_number(CurrentInput),
    #card_state{balance = CurrentBalance} = CurrentCard,
    %% check if balance is more than sum to withdraw
    case CurrentBalance > SumToWithdraw of 
	true ->
	    %% update status in stored data and in current data
	    NewBalance = CurrentBalance - SumToWithdraw,
	    NewState = updateBalance(NewBalance, State),
	    NewStateWithNullInput = NewState#state{current_input = []},
	    MessageToSend = {
			     ok, 
			     list_to_atom(
			       lists:flatten(
				 io_lib:format("you received ~p money units and current balance is ~p now", 
					       [SumToWithdraw, NewBalance])))
			    },
	    {next_state, choose_deposit_or_withdraw, NewStateWithNullInput,
	     [{reply, From, MessageToSend}]
	    };
	false ->
	    {next_state, choose_deposit_or_withdraw, State,
	     [{reply, From, {error, 'withdraw limit was exceeded'}}]}
    end;
%% number button pressed
withdraw(
  {call, From},
  {push_button, Number},
  #state{
     current_input = CurrentInput
    } = State
 ) when Number >= 0 andalso Number =< 9 ->
    enter_next_button(From, State, CurrentInput, Number);
%% wrong state
withdraw(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
deposit(
  {call, From},
  {push_button, enter},  
  #state{
     current_card = CurrentCard,
     current_input = CurrentInput
    } = State
 ) -> 
    SumToWithdraw = input_to_number(CurrentInput),
    #card_state{balance = CurrentBalance} = CurrentCard,
    NewBalance = CurrentBalance + SumToWithdraw,
    NewState = updateBalance(NewBalance, State),
    NewStateWithNullInput = NewState#state{current_input = []},
    MessageToSend = {
		     ok, 
		     list_to_atom(
		       lists:flatten(
			 io_lib:format("your balance is ~p now", [NewBalance])))
		    },
    {next_state, choose_deposit_or_withdraw, NewStateWithNullInput,
     [{reply, From, MessageToSend}]
    };
%% number button pressed
deposit(
  {call, From},
  {push_button, Number},
  #state{
     current_input = CurrentInput
    } = State
 ) when Number >= 0 andalso Number =< 9 ->
    enter_next_button(From, State, CurrentInput, Number);
%% wrong state
deposit(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% for cancel events, handle_common for All State Events (cancel button presssed)
%% or if junk input was intered
handle_common(
  {call, From},
  {push_button, cancel},
  State
 ) ->
    {
     next_state,
     waiting_card,
     reset_current_state(State),
     [{reply, From, {ok, 'you have your card back'}}]
    };
handle_common({call, From}, _IncorrectAction, State) ->
    {keep_state, State, [{reply, From, {error, 'invalid input'}}]};
handle_common(info, {tcp, _Socket, Str}, State) ->
    case Str of 
	<<"insert card ", CardNum/binary>> ->
	    io:format("intresting ~p", [CardNum]),
	    {keep_state, State};
	<<Any/bitstring>> ->
	    io:format("GOT ~p~n", [Any]),
	    {keep_state, State}
    end.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
%% from card infos to card records
card_info_list_into_record(CardInfoList) ->
    lists:foldr(fun({CardNo, Pin, Balance}, Acc) ->
			[#card_state{card_no = CardNo, 
				     pin = Pin,
				     balance = Balance
				    }
			 | Acc] 
		end, [], CardInfoList).


%% check if cardno in a given card record list
is_card_number_valid(CardStates, CardNo) ->
    case lists:keyfind(CardNo, #card_state.card_no, CardStates) of
	false ->
	    invalid_card;
	#card_state{card_no = CardNo, pin = ExpectedPin, balance = CurrentBalance}  ->
	    {ok, {CardNo, ExpectedPin, CurrentBalance}}
    end.


input_to_number(Input) ->
    lists:foldl(fun(Number, Acc) ->
			Acc * 10 + Number
		end, 0, Input).


reset_current_state(State) ->
    #state{card_states = CardStates} = State,
    #state{card_states = CardStates}.


enter_next_button(From, State, CurrentInput, Number) ->
    {
     keep_state,
     State#state{current_input = CurrentInput ++ [Number]},
     [{reply, From, continue}]
    }.


updateBalance(
  NewBalance, 
  #state{
     card_states = CardStates, 
     current_card = #card_state{card_no = CurrentCardNo}
    } = State
 ) ->
    {value, ChosenCard, ElseCardStates} = lists:keytake(CurrentCardNo, #card_state.card_no, CardStates),
    CardChangedBalance = ChosenCard#card_state{balance = NewBalance},
    State#state{
      card_states = [CardChangedBalance | ElseCardStates],
      current_card = CardChangedBalance
     }.


%% ----------------------------------------------------------------
%% TEST SUITE
%% ----------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_test_() -> 
    {
     setup, 
     fun start1/0, 
     fun stop1/1, 
     fun (SetupData) ->
	     {
	      inorder,
	      atmworkflow(SetupData)
	     }
     end
    }.

start1() ->
    States = [
	      {1, 7777, 500}, 
	      {2, 1222, 12445},
	      {3, 8832, 0}
	     ],
    atm_gen:start_link(States).


stop1(_SetupData) ->
    atm_gen:stop_atm().


atmworkflow(_SetupData) ->
    ErrorRes1 = atm_gen:push_button(5),
    CardNotFoundRes = atm_gen:insert_card(12),
    GoToWaitingState = atm_gen:insert_card(1),
    IncorrectActionInWaitingState = atm_gen:push_button(a),
    %% entering pin 3 times incorrect
    InputNumber1 = atm_gen:push_button(1),
    InputEnter1 = atm_gen:push_button(enter),
    atm_gen:push_button(1),
    atm_gen:push_button(enter),
    IncorrectPinResult = atm_gen:push_button(enter),
    %% we returned to waiting_state, so get back to waiting_pin state and input correct pin
    CorrectPinResult = insert_card_and_input_pin(),
    %% we can get card back on any step by pressing cancel
    CorrectCancelResult1 = atm_gen:push_button(cancel),
    insert_card_and_input_pin(),
    %% withdraw branch
    CorrectWithdrawInput = atm_gen:push_button(withdraw),
    atm_gen:push_button(1),
    atm_gen:push_button(0),
    CorrectWithdrawResult = atm_gen:push_button(enter),
    %% deposit branch
    CorrectDepositInput = atm_gen:push_button(deposit),
    atm_gen:push_button(1),
    atm_gen:push_button(0),
    atm_gen:push_button(0),
    CorrectDepositResult = atm_gen:push_button(enter),
    %% get card back
    CorrectCancelResult2 = atm_gen:push_button(cancel),
    [
     ?_assertEqual(ErrorRes1, {error,'invalid input'}),
     ?_assertEqual(CardNotFoundRes, {error,'incorrect card number was given'}),
     ?_assertEqual(GoToWaitingState, {ok,waiting_for_pin}),
     ?_assertEqual(IncorrectActionInWaitingState, {error,'invalid input'}),
     ?_assertEqual(InputNumber1, continue),
     ?_assertEqual(InputEnter1, {error,'incorrect pin, try again'}),
     ?_assertEqual(IncorrectPinResult, {error,'you exceeded the number of attempts'}),
     ?_assertEqual(CorrectPinResult, {ok,valid_pin}),
     ?_assertEqual(CorrectCancelResult1, {ok,'you have your card back'}),
     ?_assertEqual(CorrectWithdrawInput, {ok,withdraw}),
     ?_assertEqual(CorrectWithdrawResult,
		   {ok,'you received 10 money units and current balance is 490 now'}),
     ?_assertEqual(CorrectDepositInput, {ok,deposit}),
     ?_assertEqual(CorrectDepositResult, {ok,'your balance is 590 now'}),
     ?_assertEqual(CorrectCancelResult2, {ok,'you have your card back'})
    ].


insert_card_and_input_pin() ->	
    atm_gen:insert_card(1),
    atm_gen:push_button(7),
    atm_gen:push_button(7),
    atm_gen:push_button(7),
    atm_gen:push_button(7),
    atm_gen:push_button(enter).


-endif.
