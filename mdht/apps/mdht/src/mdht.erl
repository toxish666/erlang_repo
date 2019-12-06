-module(mdht).

%% encrypting key types
-type public_key() :: binary().
-type private_key() :: binary().
-type nonce() :: binary().

%% ordering interface type
-type ordering() :: less | greater | equal.

%% type of time throughout whole project
-type instant() :: integer().

%% some utility types
-type option(A) :: A | none.

-export_type([
	      public_key/0,
	      private_key/0,
	      nonce/0,
	      ordering/0,
	      instant/0,
	      option/1
	     ]).

%libsodium_crypto_box_curve25519xsalsa20poly1305:keypair(). 
