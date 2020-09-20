rebar_cmd
=====
Run custom shell commands as rebar3 tasks.

### Purpose

The goal of this plugin is to allow additional commands to rebar3 so that one can use it to manage everything in an Erlang project.

Wether it is bringing docker containers up, tagging a new release on git or deleting log files, we should be able to use the same build tool for all of that.

### How it works

This is a very simple and straight forward plugin. Simply tell rebar a list of commands to execute (just like linux aliases) and access them via `rebar3 cmd <your command>`


### Usage


Add the plugin to your rebar.config:
```erlang
    {plugins, [
      {rebar_cmd, "0.2.5"}
    ]}.

    {commands, [
        { docker_up, "docker-compose up -d" },
        { sync, "git fetch upstream && git merge upstream/master" }
    ]}.
```
In this case, we've added a command to bring our docker containers up, and another to sync the git repository with the remote master.

Then you just have to call your plugin directly in an existing application:

```
$ rebar3 cmd docker_up
===> Fetching rebar_cmd
===> Compiling rebar_cmd
<Command Output will be here>
```

