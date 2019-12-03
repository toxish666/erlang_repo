-module(restdb_handler).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).

%% Custom callbacks.
-export([read_db/2]).


init(Req, Opts) ->
    io:format("GOT OPTS ~p~n", [Opts]),
    {cowboy_rest, Req, Opts}.


allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.


content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, read_db}
	], Req, State}.


content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, insert_items}],
		Req, State}.


resource_exists(Req, _State) ->
    case cowboy_req:binding(paste_id, Req) of
	undefined ->
	    {false, Req, undefined};
	GetKey ->
	    {true, Req, GetKey}
    end.



read_db(Req, Key) ->
    Qs = cowboy_req:qs(Req),
    io:format("OH MYY ~p~n", [Req]),
    {<<"ARARARARRA">>, Req, Key}.


insert_items(Req, State) ->
    PasteID = new_paste_id(),
    {ok, [{<<"write">>, Paste}], Req2} = cowboy_req:read_urlencoded_body(Req),
    mdb:write(Paste),
    case cowboy_req:method(Req2) of
		<<"POST">> ->
			{{true, <<$/, PasteID/binary>>}, Req2, State};
		_ ->
			{true, Req2, State}
	end.


new_paste_id() ->
	Initial = rand:uniform(62) - 1,
	new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
	Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
	<< <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_paste_id(Bin, Rem) ->
	Next = rand:uniform(62) - 1,
	new_paste_id(<<Bin/binary, Next>>, Rem - 1).
