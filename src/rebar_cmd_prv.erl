-module(rebar_cmd_prv).

-export([
         init/1,
         do/1,
         format_error/1
        ]).

-define(PROVIDER, cmd).
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
            {desc, "A rebar plugin for running custom commands."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  Config = rebar_state:get(State, commands, []),
  {ArgsPropList, _} = rebar_state:command_parsed_args(State),
  {task, CmdName} = proplists:lookup(task, ArgsPropList),
  case lists:keyfind(list_to_atom(CmdName), 1, Config) of
    {_, Command} ->
      rebar_api:debug("Running ~p with command ~p.~n", [[CmdName], [Command]]),
      case run_shell(Command) of
        {ok, _} -> {ok, State};
        Error -> {error, {?MODULE, Error}}
      end;
    false ->
      {error, {?MODULE, no_command}}
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
run_shell(Command) ->
  run_shell(Command, 15000).

run_shell(Command, Timeout) ->
  Port = erlang:open_port({spawn, Command}, [exit_status]),
  shell_loop(Port, [], Timeout).

shell_loop(Port, Data, Timeout) ->
  receive
    {Port, {data, MoreData}} ->
      rebar_api:console(MoreData, []),
      shell_loop(Port, MoreData++Data, Timeout);
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, Error}} -> throw({sh_fail, Error})
  after Timeout ->
    throw(Timeout)
  end.

