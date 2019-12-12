-module(find_manager).

-export([start_link/2]).
-export([nodes_response/3]).


start_link({KeyToFind, Ref}, From) ->
    {ok, Pid} = gen_event:start_link(),
    gen_event:add_handler(Pid, find_progress, [KeyToFind, From, Ref]),
    {Pid, Ref}.


nodes_response(Pid, SenderPublicKey, PackedNodes) ->
    gen_event:notify(Pid, {nodes_response, SenderPublicKey, PackedNodes}).
