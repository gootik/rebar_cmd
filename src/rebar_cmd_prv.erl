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
      case run_shell({CmdName, Command}) of
        {ok, _} -> {ok, State};
        {error, Error} -> {error, Error}
      end;
    false ->
      {error, "Command " ++ CmdName ++ " is unknown. Check your configuration."}
  end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================
run_shell({CmdName, Command}) ->
  run_shell({CmdName, Command}, 15000).

run_shell({CmdName, Command}, Timeout) ->
  Port = erlang:open_port({spawn, Command}, [exit_status]),
  shell_loop(Port, [], Timeout, {CmdName, Command}).

shell_loop(Port, Data, Timeout, {CmdName, Command}) ->
  receive
    {Port, {data, MoreData}} ->
      io:format(MoreData),
      shell_loop(Port, MoreData ++ Data, Timeout, {CmdName, Command});
    {Port, {exit_status, 0}} -> {ok, Data};
    {Port, {exit_status, Error}} ->
        Cmd = lists:flatten(Command),
        {error, "Command " ++ CmdName ++ " (" ++ Cmd ++ ") failed with exit status "
                ++ integer_to_list(Error) ++ "."}
  after Timeout ->
        Cmd = lists:flatten(Command),
        {error, "Command " ++ CmdName ++ " (" ++ Cmd ++ ") timed out after "
                ++ integer_to_list(Timeout) ++ " ms."}
  end.

