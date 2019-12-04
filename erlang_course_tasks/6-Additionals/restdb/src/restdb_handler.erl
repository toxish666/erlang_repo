-module(restdb_handler).

%curl -d "param1=value1&param2=value2" -H "Content-Type: application/x-www-form-urlencoded" -X POST http://localhost:3000/data


%% Standard callbacks.
-export([
	 init/2,
	 allowed_methods/2,
	 content_types_provided/2,
	 content_types_accepted/2,
	 resource_exists/2,
	 delete_resource/2
	]).

%% Custom callbacks.
-export([
	 read_db/2,
	 insert_items/2
	]).


-define(MDB_NAME, mdb_server).

%%----------------------------CALLBACKS---------------------------

init(Req, Opts) ->
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


delete_resource(Req, KeyB) ->
    Key = binary_to_atom(KeyB, unicode),
    ok = mdb_server:delete(?MDB_NAME, Key),
    {<<"Attempt to delete key">>, Req, KeyB}.


read_db(Req, KeyB) ->
    Key = binary_to_atom(KeyB, unicode),
    FoundedRes = mdb_server:find(?MDB_NAME, Key),
    case FoundedRes of
	{error, _} = Tup when is_tuple(Tup) ->
	    ResInterleaved = interleave_atoms(tuple_to_list(Tup), ' '),
	    Res = lists:map(fun(A) -> atom_to_list(A) end, ResInterleaved),
	    ResBin = list_to_binary(Res),
	    {<<"Not found with: ", ResBin/binary>>, Req, KeyB};
	{ok, StrRes} ->
	    StrResB = list_to_binary(StrRes),
	    {<<"Founded result: ", StrResB/binary>>, Req, KeyB}
    end.


%% insert in format: atom -> "string"
insert_items(Req, State) ->
    PasteID = new_paste_id(),
    {ok, ReqBody, Req2} = cowboy_req:read_urlencoded_body(Req),
    lists:foreach(fun({KeyB, ValueB}) ->
			  Key = binary_to_atom(KeyB, unicode),
			  Value = binary_to_list(ValueB),
			  mdb_server:insert(?MDB_NAME, Key, Value)
		  end, ReqBody
		 ),
    case cowboy_req:method(Req2) of
		<<"POST">> ->
			{{true, <<$/, PasteID/binary>>}, Req2, State};
		_ ->
			{true, Req2, State}
	end.

%%--------------------------PRIVATE---------------------------------

new_paste_id() ->
	Initial = rand:uniform(62) - 1,
	new_paste_id(<<Initial>>, 7).
new_paste_id(Bin, 0) ->
	Chars = <<"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890">>,
	<< <<(binary_part(Chars, B, 1))/binary>> || <<B>> <= Bin >>;
new_paste_id(Bin, Rem) ->
	Next = rand:uniform(62) - 1,
	new_paste_id(<<Bin/binary, Next>>, Rem - 1).


interleave_atoms(List, Atom) ->
    interleave_atoms(List, Atom, []).

interleave_atoms([], _, Acc) ->
    lists:reverse(Acc);
interleave_atoms([H|L], Atom, Acc) ->
    interleave_atoms(L, Atom, [Atom, H|Acc]).
