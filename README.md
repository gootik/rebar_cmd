rebar_cmd
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:
```Erlang
    {plugins, [
        { rebar_cmd, ".*", {git, "git@github.com:shezarkhani/rebar_cmd.git", {tag, "0.1.0"`}}}
    ]}.

    {commands, [
        { docker_up, "docker-compose up -d" }
    ]}.
```
Then just call your plugin directly in an existing application:


    $ rebar3 cmd docker_up
    ===> Fetching rebar_cmd
    ===> Compiling rebar_cmd
    <Plugin Output>
