rebar_cmd
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar_cmd, ".*", {git, "git@host:user/rebar_cmd.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar_cmd
    ===> Fetching rebar_cmd
    ===> Compiling rebar_cmd
    <Plugin Output>
