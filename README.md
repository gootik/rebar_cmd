rebar_cmd
=====
Run custom shell commands as rebar3 tasks.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:
```Erlang
    {plugins, [
        { rebar_cmd, ".*", {git, "git@github.com:shezarkhani/rebar_cmd.git", {tag, "0.2.1"}}}
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
