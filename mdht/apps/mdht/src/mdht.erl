-module(mdht).
-behaviour(application).

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
-type either(A,B) :: {some, A} | {error, B}.

-export_type([
	      public_key/0,
	      private_key/0,
	      nonce/0,
	      ordering/0,
	      instant/0,
	      option/1,
	      either/2
	     ]).

%% behaviour
-export([start/2, stop/1]).

start(_Type, _Args) ->
	mdht_sup:start_link().

stop(_State) ->
	ok.
