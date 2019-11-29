-module(statistics_app).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    case statistics_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Other ->
            {error, Other}
    end.


stop(_State) ->
    ok.
