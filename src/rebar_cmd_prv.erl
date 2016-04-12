-module(rebar_cmd_prv).

-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, rebar_cmd).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 cmd COMMAND"},
            {opts, []},
            {short_desc, "A rebar plugin for running custom commands"},
            {desc, "A rebar plugin for runnins custom commands."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Config = rebar_state:get(State, cmd, []),
  {Opts, _} = rebar_state:command_parsed_args(State),
  case lists:keyfind("test", 1, Config) of
    {_, Command} ->
      io:format("~p~n", [ Opts ]),
      io:format("~p~n", [ Command ]),
      {ok, State};
    false ->
      io:format("Did not find command"),
      {error, {?MODULE, no_command}}
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
