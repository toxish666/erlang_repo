-module(mdht).

%% encrypting key types
-type publickey() :: binary().
-type privatekey() :: binary().
-type nonce() :: binary().

%% ordering interface type
-type ordering() :: less | greater | equal.

%% type of time throughout whole project
-type instant() :: integer().

%% some utility types
-type option(A) :: A | none.

-export_type([
	      publickey/0,
	      privatekey/0,
	      nonce/0,
	      ordering/0
	     ]).
