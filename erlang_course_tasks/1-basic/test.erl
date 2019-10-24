-module(test).

-export([mapf/2]).

mapf(List, F) ->
    lists:foldr(fun(E, Ac) -> [F(E)|Ac] end, [], List).
