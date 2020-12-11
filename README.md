# rebar_cmd [![Build Status](https://github.com/gootik/rebar_cmd/workflows/build/badge.svg)](https://github.com/gootik/rebar_cmd) [![Hex.pm](https://img.shields.io/hexpm/v/rebar_cmd.svg)](https://hex.pm/packages/rebar_cmd)

Run custom shell commands with `rebar3 cmd <command>`.

## Purpose

The goal of this plugin is to allow `rebar3` to run additional commands, so
that one can use it solely to manage everything in an Erlang project.

Whether it is bringing Docker containers up, tagging a new Git release or
deleting log files, you should be able to use a single build tool.

## How it works

This is a very simple and straightforward plugin. Simply describe your
command in `rebar.config` and execute (just like you would Linux aliases)
with `rebar3 cmd <command>`.

## Usage

Add the plugin to your `rebar.config`:

```erlang
    {plugins, [
        {rebar_cmd, "0.2.6"}
    ]}.

    {commands, [
        {docker_up, "docker-compose up -d"},
        {sync, "git fetch upstream && git merge upstream/master"}
    ]}.
```

The example above shows you how to describe a command to bring your
Docker containers up, as well as another one to sync a Git repository
with remote master.

Some options are available, as described below:

* `[{timeout, <Ms>}]` (defaults to `15000`)
* `[{verbose, <Verbose>}]` (defaults to `false`)

e.g. you could change the previous `docker_up` command to have it fail
after 5s with `{docker_up, "docker-compose up -d", [{timeout, 5000}]},`.
You could also get more info from the shell for the above command
`sync` with
`{sync, "git fetch upstream && git merge upstream/master", [{verbose, true}]}`

Check it out:

```bash
$ rebar3 cmd sync
===> Analyzing applications...
===> Compiling rebar_cmd
===> Command sync resulted in: "Already up to date."
```
