%% @doc Encrypt packet with the public key of the receiver
%% and the secret key of the sender.
%% The receiver decrypts the packets using the receiver's secret key
%% and the sender's public key.

-module(mdht_encryption).
-include("consts.hrl").

-export([
	 generate_nonce/0,
	 generate_crypto_pair/0,
	 generate_combined_key/2,
	 encrypt_packet/4,
	 encrypt_packet/3,
	 decrypt_packet/4,
	 decrypt_packet/3
	]).


%% @doc Generate public key and secret key pair
generate_crypto_pair() ->
    libsodium_crypto_box_curve25519xsalsa20poly1305:keypair().


%% @doc Genereate random nonce.
generate_nonce() ->
    libsodium_randombytes:randombytes(?NONCEBYTES).


%% @doc Generate random tag.
generate_tag() ->
    libsodium_randombytes:randombytes(?TAGBYTES).


%% @doc Get combined key out of public key and secret key
generate_combined_key(PK, SK) ->
    libsodium_crypto_box_curve25519xsalsa20poly1305:beforenm(PK, SK).
    

%% @doc Encrypt binary with a combined key(public key of the receiver and
%% the secret key of the sender) and a nonce 
encrypt_packet(Binary, Nonce, PublicKey, SecretKey) ->
    CombKey = generate_combined_key(PublicKey, SecretKey),
    encrypt_packet(Binary, Nonce, CombKey).

encrypt_packet(Binary, Nonce, CombinedKey) ->
    libsodium_crypto_box:easy_afternm(Binary, Nonce, CombinedKey). 


%% @doc Decrypt ciphered text 
decrypt_packet(Binary, Nonce, PublicKey, SecretKey) ->
    CombKey = generate_combined_key(PublicKey, SecretKey),
    decrypt_packet(Binary, Nonce, CombKey).

decrypt_packet(Binary, Nonce, CombinedKey) ->
    libsodium_crypto_box:open_easy_afternm(Binary, Nonce, CombinedKey). 


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

generate_combined_key_test() ->
    {PK1, SK1} = mdht_encryption:generate_crypto_pair(),
    {PK2, SK2} = mdht_encryption:generate_crypto_pair(),
    ?assertEqual(
       generate_combined_key(PK1, SK2),
       generate_combined_key(PK2, SK1)
      ).

encrypt_decrypt_test() ->
    Message = <<$f,$i,$v,$a>>,
    {PK1, SK1} = mdht_encryption:generate_crypto_pair(),
    {PK2, SK2} = mdht_encryption:generate_crypto_pair(),
    Nonce = mdht_encryption:generate_nonce(),
    CipheredText = mdht_encryption:encrypt_packet(Message, Nonce, PK2, SK1),
    PlainText = mdht_encryption:decrypt_packet(CipheredText, Nonce, PK1, SK2),
    ?assertEqual(Message, PlainText).

-endif.
