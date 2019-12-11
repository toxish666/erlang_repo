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
	 decrypt_message/3,
	 encrypt_message/3,
	 get_pk/0,
	 get_nonce/0,
	 get_tag/0,
	 get_request_id/0
	]).

%% public apis
start_link(MDhtServerPid) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, MDhtServerPid, []).


decrypt_message(Ciphered, Nonce, SenderPublicKey) ->
    gen_server:call(?MODULE, {decrypt, {Ciphered, Nonce, SenderPublicKey}}).

encrypt_message(PlayinText, Nonce, ReceiverPublicKey) ->
    gen_server:call(?MODULE, {encrypt, {PlayinText, Nonce, ReceiverPublicKey}}).

get_pk() ->
    gen_server:call(?MODULE, {get_pk}).

get_nonce() ->
    gen_server:call(?MODULE, {get_nonce}).

get_tag() ->
    gen_server:call(?MODULE, {get_tag}).

get_request_id() ->
    gen_server:call(?MODULE, {get_request_id}).

%% Gen server callbacks
init(MDhtServerPid) ->
    self() ! {assign_to_server},
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
    end;
handle_call({encrypt, {PlainText, Nonce, ReceiverPublicKey}}, _From, MDhtServerPid) ->
    %% get our own pk
    {ok, OwnSK} = gen_server:call(MDhtServerPid, {get_sk}),
    EcryptedBin = mdht_encryption:encrypt_packet(PlainText, Nonce, ReceiverPublicKey, OwnSK),
    case EcryptedBin of 
	-1 ->
	    {reply, {error, decryption_error}, MDhtServerPid};
	Correct when is_binary(Correct) ->
	    {reply, {ok, EcryptedBin}, MDhtServerPid}
    end;
handle_call({get_pk}, _From, MDhtServerPid) ->
    {ok, PK} = gen_server:call(MDhtServerPid, {get_pk}),
    {reply, {ok, PK}, MDhtServerPid};
handle_call({get_nonce}, _From, MDhtServerPid) ->
    Nonce = mdht_encryption:generate_nonce(),
    {reply, {ok, Nonce}, MDhtServerPid};
handle_call({get_tag}, _From, MDhtServerPid) ->
    Tag = mdht_encryption:generate_tag(),
    {reply, {ok, Tag}, MDhtServerPid};
handle_call({get_request_id}, _From, MDhtServerPid) ->
    RequestId = mdht_encryption:generate_request_id(),
    {reply, {ok, RequestId}, MDhtServerPid}.


handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info({assign_to_server}, MDhtServerPid) ->
    {PK, SK} = mdht_encryption:generate_crypto_pair(),
    gen_server:cast(MDhtServerPid, {assigned_keys, PK, SK}),
    {noreply, MDhtServerPid}.
