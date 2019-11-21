-module(atm_state).

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

-export([start_link/2]).

-include("atm_net_strucs.hrl").

-define(TIMEOUTINSEC, 10000).
-define(TIMEOUT, {state_timeout, ?TIMEOUTINSEC, timeout_error}).

-define(MAXATTEMPTS, 3).

-record(state, {
		server,
		socket,
		current_card = #card_state{},
		current_input = [],
		attempts = 0
	       }
       ).

%%----------------------------------------------------------------------------
%% PUBLIC API
%%----------------------------------------------------------------------------
start_link(Socket, Server) ->
    gen_statem:start_link(?MODULE, [Socket, Server], []).


%%----------------------------------------------------------------------------
%% LIFECYCLE
%%----------------------------------------------------------------------------
init([Socket, Server]) ->
    {
     ok, 
     waiting_card, 
     #state{
	server = Server,
	socket = Socket
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
  info,
  {tcp, Socket, <<"insert card ", CardNumber:32, _/binary>> = _Str},
  #state{server = Server } = State
 ) ->
    CardNumberBinary = <<CardNumber:32>>,
    CardNo = list_to_integer(binary_to_list(CardNumberBinary)),
    GenServAnswer = gen_server:call(Server, {is_card_number_valid, CardNo, self()}),
    case GenServAnswer of
	{ok, NewCurrentCard}  ->
	    send(Socket, "Waiting 10 sec for pin enterance", []),
	    {
	     next_state, 
	     waiting_pin, 
	     State#state{current_card = NewCurrentCard},
	     [?TIMEOUT]
	    };
	busy_card ->
	    send(Socket, "Error: the card is in use", []),
	    {keep_state, State};
	invalid_card ->
	    send(Socket, "Error: invalid card", []),
	    {keep_state, State}
    end;
waiting_card(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
waiting_pin(
  info,
  {tcp, Socket, <<"push button enter", _/binary>> = _Str},
  #state{
     current_card = #card_state{pin = ExpectedPin},
     current_input = CurrentInput,
     attempts = Attempts
    } = State
 ) when Attempts =< ?MAXATTEMPTS ->
    EnteredPin = input_to_number(CurrentInput),
    case EnteredPin == ExpectedPin of
	true ->
	    send(Socket, "Valid pin, choose withdraw or deposit", []),
	    {next_state, choose_deposit_or_withdraw, State#state{current_input = []}};
	false ->
	    %%----attempts are exhausted
	    if Attempts == ?MAXATTEMPTS - 1 ->
		    send(Socket, "Error: you exceeded the number of attempts", []),
		    {
		     next_state,
		     waiting_card,
		     reset_current_state(State)
		    };
	       %% next attempt
	       true ->
		    send(Socket, "Incorrect pin, try again", []),
		    {
		     keep_state, 
		     State#state{current_input = [], attempts = Attempts + 1}, 
		     [?TIMEOUT]
		    }
	    end
    end;
%% number buttons pressed
waiting_pin(
  info,
  {tcp, Socket, <<"push button ", Button:8 , _/binary>> = _Str},
  #state{current_input = CurrentInput} = State
 ) ->
    ButtonBinary = <<Button:8>>,
    ButtonInt = list_to_integer(binary_to_list(ButtonBinary)),
    if ButtonInt >= 0 andalso ButtonInt =<9 ->
	    send(Socket, "Continue input ", []),
	    {
	     keep_state, 
	     State#state{current_input = CurrentInput ++ [ButtonInt]},
	     [?TIMEOUT]
	    };
       true ->
	    send(Socket, "Incorrect input!", []),
	    {
	     keep_state,
	     State,
	     [?TIMEOUT]
	    }
    end;
%% timeout exception
waiting_pin(state_timeout, _Reason, #state{socket = Socket, server = Server} = State) ->
    ok = gen_server:call(Server, {release_card, self()}),
    send(Socket, "State timeout in waiting for pin state", []),
    {
     next_state, 
     waiting_card, 
     reset_current_state(State)
    };
waiting_pin(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% choose next action after correct pin was entered
choose_deposit_or_withdraw(
  info,
  {tcp, Socket, <<"withdraw" , _/binary>> = _Str},
  State
 ) ->
    send(Socket, "Withdraw mode enabled", []),
    {next_state, withdraw, State};
choose_deposit_or_withdraw(
  info,
  {tcp, Socket, <<"deposit" , _/binary>> = _Str},
  State
 ) ->
    send(Socket, "Deposit mode enabled", []),
    {next_state, deposit, State};
choose_deposit_or_withdraw(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
withdraw(
  info,
  {tcp, Socket, <<"push button enter" , _/binary>> = _Str},
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
	    NewState = State#state{current_card = updateBalance(NewBalance, State)},
	    NewStateWithNullInput = NewState#state{current_input = []},
	    MessageToSend = {
			     ok, 
			     list_to_atom(
			       lists:flatten(
				 io_lib:format("you received ~p money units and current balance is ~p now", 
					       [SumToWithdraw, NewBalance])))
			    },
	    send(Socket, "A ~p", [MessageToSend]),
	    {next_state, choose_deposit_or_withdraw, NewStateWithNullInput};
	false ->
	    send(Socket, "Error: withdraw limit was exceeded", []),
	    {next_state, choose_deposit_or_withdraw, State}
    end;
%% number button pressed
withdraw(
  info,
  {tcp, Socket, <<"push button ", Button:8 , _/binary>> = _Str},
  #state{
     current_input = CurrentInput
    } = State
 ) ->
    ButtonBinary = <<Button:8>>,
    ButtonInt = list_to_integer(binary_to_list(ButtonBinary)),
    if ButtonInt >= 0 andalso ButtonInt =<9 ->
	    enter_next_button(Socket, State, CurrentInput, ButtonInt);
       true ->
	    send(Socket, "Incorrect input!", []),
	    {keep_state, State}    
    end;
%% wrong state
withdraw(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


%% enter button pressed
deposit(
  info,
  {tcp, Socket, <<"push button enter" , _/binary>> = _Str}, 
  #state{
     current_card = CurrentCard,
     current_input = CurrentInput
    } = State
 ) -> 
    SumToWithdraw = input_to_number(CurrentInput),
    #card_state{balance = CurrentBalance} = CurrentCard,
    NewBalance = CurrentBalance + SumToWithdraw,
    NewState = State#state{current_card = updateBalance(NewBalance, State)},   
    NewStateWithNullInput = NewState#state{current_input = []},
    MessageToSend = {
		     ok, 
		     list_to_atom(
		       lists:flatten(
			 io_lib:format("your balance is ~p now", [NewBalance])))
		    },
    send(Socket, "A ~p", [MessageToSend]),
    {next_state, choose_deposit_or_withdraw, NewStateWithNullInput};
%% number button pressed
deposit(
  info,
  {tcp, Socket, <<"push button ", Button:8 , _/binary>> = _Str},
  #state{
     current_input = CurrentInput
    } = State
 ) ->
    ButtonBinary = <<Button:8>>,
    ButtonInt = list_to_integer(binary_to_list(ButtonBinary)),
    if ButtonInt >= 0 andalso ButtonInt =<9 ->
	    enter_next_button(Socket, State, CurrentInput, ButtonInt);
       true ->
	    send(Socket, "Incorrect input!", []),
	    {keep_state, State}    
    end;
%% wrong state
deposit(EventType, IncorrectAction, State) ->
    handle_common(EventType, IncorrectAction, State).


handle_common(
  info,
  {tcp, Socket, <<"push button cancel", _/binary>> = _Str},
  State
 ) ->
    send(Socket, "You have your card back", []),
    {
     next_state,
     waiting_card,
     reset_current_state(State)
    };
handle_common(
  info,
  {tcp, _Socket, <<"stop", _/binary>> = _Str},
  _State
) ->
    io:format("Stopping atm client ~n"),
    {stop, normal}; %% stop
handle_common(info, {tcp, Socket, _Str}, State) ->
    send(Socket, "Error: incorrect input", []),
    {keep_state, State};
handle_common(info, {tcp_closed, _Socket}, _State) ->
    io:format("Dead tcp client ~n"),
    {stop, normal}.


%%----------------------------------------------------------------------------
%% HANDLERS
%%----------------------------------------------------------------------------
input_to_number(Input) ->
    lists:foldl(fun(Number, Acc) ->
			Acc * 10 + Number
		end, 0, Input).


reset_current_state(State) ->
    State#state{current_card = #card_state{}, current_input = []}.


enter_next_button(Socket, State, CurrentInput, Number) ->
    send(Socket, "Continue input ", []),
    {
     keep_state,
     State#state{current_input = CurrentInput ++ [Number]}
    }.


updateBalance(
  NewBalance,   
  #state{
     current_card = CurrentCard,
     server = BankServerPid
    }) ->
    {ok, UpdatedCard} = gen_server:call(BankServerPid, {update_balance, CurrentCard, NewBalance}),
    UpdatedCard.


send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
    ok = inet:setopts(Socket, [{active, once}]),
    ok.
