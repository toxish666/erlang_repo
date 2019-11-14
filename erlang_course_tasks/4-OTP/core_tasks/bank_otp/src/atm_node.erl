%%
%%
%% @author toxish666 [https://github.com/toxish666]
%% @reference See <a href="https://github.com/bitgorbovsky/erlang-course-tasks/blob/master/tasks/4-OTP.md#42-%D0%B1%D0%B0%D0%BD%D0%BA%D0%BE%D0%BC%D0%B0%D1%82-gen_statem"> task itself </a> for more info.
%% 
%%

-module(atm_node).

-behaviour(gen_statem).

-export([
	 init/1,
	 terminate/3,
	 code_change/3,
	 callback_mode/0
	]).

-export([
	 handle_common/3,
	 waiting_card/3,
	 waiting_pin/3,
	 choose_deposit_or_withdraw/3,
	 withdraw/3,
	 deposit/3
	]).

-export([
	 start_link/2
	]).


-record(card_state, {
		     card_no,
		     pin,
		     balance
		    }).

-record(state, {
		bank_server_pid,
		current_card = #card_state{},
		current_input = [],
		attempts = 0
	       }
       ).


-define(TIMEOUTINSEC, 10000).
-define(TIMEOUT, {state_timeout, ?TIMEOUTINSEC, timeout_error}).

-define(MAXATTEMPTS, 3).


start_link(Name, BankServerPid) ->
    gen_statem:start_link({local, Name}, ?MODULE, [], []).

%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init(BankServerPid) ->
    {
     ok, 
     waiting_card, 
     #state{bank_server_pid = BankServerPid}
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
    {keep_state, State, [{reply, From, {error, 'invalid input'}}]}.


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
