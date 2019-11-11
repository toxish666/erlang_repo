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

-module(atm_gen).

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
	 waiting_card/3,
	 waiting_pin/3
	]).

%%----------------------------------------------------------------------------
%% PUBLIC API EXPORTS
%%----------------------------------------------------------------------------
-export([
	 start_link/1,
	 insert_card/1,
	 push_button/1,
	 terminate_atm/1
	]).

%%----------------------------------------------------------------------------
%% STRUCTURES
%%----------------------------------------------------------------------------
-record(card_state, {
		     card_no,
		     pin,
		     balance
		    }).

-record(pin_state, {
		    expected_pin,
		    attempts = 0,
		    current_input = []
		   }).

-record(state, {
	        card_states = [],
		current_card,
	        pin_state = #pin_state{}
	       }).



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


-spec push_button(Button ::  enter | cancel | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') -> 
			 continue | {ok, Result :: term()} | {error, Reason :: term()}.
push_button(Button) ->
    gen_statem:call(?NAME, {push_button, Button}).


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init(CardInfoList) ->
    {
     ok, 
     waiting_mode, 
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
  #state{
     card_states = CardStates,
     pin_state = PinState
    } = State
 ) ->
    case is_card_number_valid(CardStates, CardNo) of
	%% no cardNo in cardStates were found
	invalid_card ->
	    {
	     keep_state, State, 
	     [{reply, From, {error, 'incorrect card number was given'}}]
	    };
	{ok, {CardNo, ExpectedPin}}  ->
	    {
	     next_state, 
	     waiting_pin, 
	     State#state{
	       current_card = CardNo,
	       pin_state = 
		   PinState#pin_state{expected_pin = ExpectedPin}
	      },
	     [
	      {reply, From, {ok, waiting_for_pin}},
	      ?TIMEOUT
	     ]
	    }
    end;
waiting_card({call, From}, _IncorrectContent, State) ->
    {
     keep_state, 
     State, 
     [{reply, From, {error, 'invalid action'}}]
    }.


%% enter button block
waiting_pin(
  {call, From}, 
  {push_button, enter}, 
  #state{
     pin_state = #pin_state
     {
       expected_pin = Pid,
       current_input = Pid
     } = PinState
    } = State
 ) ->
    {
     next_state,
     withdraw_input,
     State#state{pin_state = PinState#pin_state{current_input = []}},
     [{reply, From, {ok, valid_pin}}]
    };

waiting_pin(
  {call, From}, 
  {push_button, enter}, 
  #state{
     pin_state = #pin_state
     {
       expected_pin = Pid,
       current_input = WrongPid,
       attempts = Attempts
     } = PinState
    } = State
 ) when Attempts =< ?MAXATTEMPTS andalso Pid =/= WrongPid ->
    {
     keep_state,
     State#state{pin_state = PinState#pin_state{current_input = [], attempts = Attempts + 1}},
     [
      {reply, From, {error, 'incorrect pin, try again'}},
      ?TIMEOUT
     ]
    };

waiting_pin(
  {call, From}, 
  {push_button, enter}, 
  #state{
     card_states = CardStates,
     pin_state = #pin_state
     {
       expected_pin = Pid,
       current_input = WrongPid,
       attempts = Attempts
     }
    }
 ) when Attempts > ?MAXATTEMPTS andalso Pid =/= WrongPid ->
    {
     next_state,
     waiting_card,
     #state{card_states = CardStates},
     [{reply, From, {error, 'you exceeded the number of attempts'}}]
    };

%% cancel button block
waiting_pin(
  {call, From}, 
  {push_button, cancel}, 
  #state{
     card_states = CardStates
    }
 ) ->
    {
     next_state,
     waiting_card,
     #state{card_states = CardStates},
     [{reply, From, {error, 'you have your card back'}}]
    };

%% number buttons block
waiting_pin(
  {call, From}, 
  {push_button, Number},    
  #state{
     pin_state = #pin_state
     {
       current_input = CurrentInput
     } = PinState
    } = State
 ) when Number >= 0 andalso Number =< 9 ->
    {
     keep_state,
     State#state{pin_state = PinState#pin_state{current_input = CurrentInput ++ [Number]}},
     [{reply, From, 'enter next button'}],
     ?TIMEOUT
    }.








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
			 | Acc] end, [], CardInfoList).


%% check if cardno in a given card record list
is_card_number_valid(CardStates, CardNo) ->
    case lists:keyfind(CardNo, #card_state.card_no, CardStates) of
	false ->
	    invalid_card;
	#card_state{card_no = CardNo, pin = ExpectedPin}  ->
	    {ok, {CardNo, ExpectedPin}}
    end.





