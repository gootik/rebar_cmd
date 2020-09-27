-module(rebar_cmd_prv).

-export([
    init/1,
    do/1,
    format_error/1
]).

-ignore_xref([
    do/1,
    format_error/1
]).

-define(PROVIDER, cmd).
-define(DEPS, [app_discovery]).
-define(DEFAULT_OPT_TIMEOUT, 15000).
-define(DEFAULT_OPT_VERBOSE, false).

-ifdef(TEST).
-export([
    do_internal/2,
    find_command_in/2
]).

-ignore_xref([
    do_internal/2,
    find_command_in/2
]).
-endif.

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
    CmdName = proplists:get_value(task, ArgsPropList, undefined),
    do_internal({CmdName, find_command_in(CmdName, Config)}, State).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

do_internal({_CmdName, CmdDef}, _State) when
    is_tuple(CmdDef) andalso
        (erlang:tuple_size(CmdDef) < 2 orelse erlang:tuple_size(CmdDef) > 3)
->
    {error, "Invalid configuration: expected [{CmdName, Cmd, Opts}]."};
do_internal({_CmdName, CmdDef}, State) when is_tuple(CmdDef) ->
    {CmdName, Cmd, Opts} = normalize(CmdDef),
    handle_cmd(exec(CmdName, Cmd, Opts), Opts, State);
do_internal({undefined = _CmdName, false = _CommandFound}, _State) ->
    {error, "No <command> (as in 'cmd <command>') given as input."};
do_internal({CmdName, false = _CommandFound}, _State) ->
    {error, "Command " ++ CmdName ++ " (as atom) is unknown. Check your configuration."}.

find_command_in(undefined = _CmdName, _Config) ->
    false;
find_command_in(CmdName, Config) ->
    lists:keyfind(list_to_atom(CmdName), 1, Config).

normalize({CmdName, Cmd}) ->
    normalize({CmdName, Cmd, []});
normalize({CmdName, Cmd, Opts}) ->
    {atom_to_list(CmdName), Cmd, Opts}.

exec(CmdName, Cmd, Opts) ->
    Pid = self(),
    Ref = erlang:make_ref(),
    Timeout = get_opt(timeout, Opts, CmdName),
    {MonitorPid, MonitorRef} = erlang:spawn_monitor(
        fun() ->
            not is_list(Cmd) andalso exit("call not expressed as a string"),
            rebar_api:debug("Command ~s executing ~s with options ~p.", [CmdName, Cmd, Opts]),
            Pid ! {Ref, os:cmd(Cmd)}
        end
    ),
    receive
        {Ref, Result} ->
            {ok, {CmdName, Result}};
        {'DOWN', MonitorRef, _Type, _Object, Info} ->
            rebar_api:error("Command ~s died unexpectedly with ~p.", [CmdName, Info]),
            {ok, {CmdName, died}}
    after Timeout ->
        exit(MonitorPid, kill),
        TimeOutS = integer_to_list(Timeout),
        {error, "Command " ++ CmdName ++ " (" ++ Cmd ++ ") timed out after " ++ TimeOutS ++ " ms."}
    end.

handle_cmd({error, Error}, _Opts, _State) ->
    {error, Error};
handle_cmd({ok, {_CmdName, died}}, _Opts, State) ->
    {ok, State};
handle_cmd({ok, {CmdName, Result}}, Opts, State) ->
    case get_opt(verbose, Opts, CmdName) of
        true ->
            rebar_api:warn("Command ~s resulted in: ~p", [CmdName, Result]);
        false ->
            rebar_api:debug("Command ~s resulted in: ~p", [CmdName, Result])
    end,
    {ok, State}.

get_opt(timeout, Opts, CmdName) ->
    Timeout = proplists:get_value(timeout, Opts, ?DEFAULT_OPT_TIMEOUT),
    case is_integer(Timeout) of
        false ->
            rebar_api:error(
                "Command ~s's timeout not expressed as an integer. "
                "Defaulting to ~p.",
                [CmdName, ?DEFAULT_OPT_TIMEOUT]
            ),
            ?DEFAULT_OPT_TIMEOUT;
        true ->
            Timeout
    end;
get_opt(verbose, Opts, _CmdName) ->
    proplists:get_value(verbose, Opts, ?DEFAULT_OPT_VERBOSE).
