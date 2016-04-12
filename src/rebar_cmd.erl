-module(rebar_cmd).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar_cmd_prv:init(State),
    {ok, State1}.
