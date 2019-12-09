-module(encryption_server).
-behaviour(gen_server).

%% behaviour
-export([
	 init/1,
	 terminate/2,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2
	]).

%% public api
-export([
	 start_link/1,
	 decrypt_message/3
	 %encrypt_message/3
	]).

%% public apis
start_link(MDhtServerPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, MDhtServerPid, []).


decrypt_message(Ciphered, Nonce, SenderPublicKey) ->
    gen_server:call(?MODULE, {decrypt, {Ciphered, Nonce, SenderPublicKey}}).


%% Gen server callbacks
init(MDhtServerPid) ->
    {ok, MDhtServerPid}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({decrypt, {Ciphered, Nonce, SenderPublicKey}}, _From, MDhtServerPid) ->
    %% get our own pk
    {ok, OwnSK} = gen_server:call(MDhtServerPid, {get_sk}),
    DecryptedBin = mdht_encryption:decrypt_packet(Ciphered, Nonce, SenderPublicKey, OwnSK),
    case DecryptedBin of 
	-1 ->
	    {reply, {error, decryption_error}, MDhtServerPid};
	Correct when is_binary(Correct) ->
	    {reply, {ok, Correct}, MDhtServerPid}
    end.

handle_cast(Msg, State) ->
    {noreply, State}.
    
handle_info(Msg, State) ->
    {noreply, State}.
