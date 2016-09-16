provee
======

`provee` is a library for writing system provisioning scripts in
[Racket](https://racket-lang.org).


Goals
=====

1. Limited Scope

    Unlike the Ansibles, Salts, Chefs and Puppets of the world, `provee` does
    not want to be everything. It merely wants to provide you with procedures
    and tools that allow you to write clear, concise scripts to ensure things,
    like, packages are installed, configuration files contain the correct
    contents and services are restarted when they need to be. Those scripts need
    to be run on the hosts (or in the containers) they're meant to configure.
    `provee` has no opinions on remote execution or orchestration. Due to the
    choice of Racket, however, pacakging, shipping and running scripts on remote
    hosts can be as simple as using `raco exe` and `raco dist` to build a
    standalone distribution of the script that can be copied to the host with
    `rsync` or `scp` and, finally, exectuted with `ssh` requiring nothing to be
    preinstalled on the target host. `provee` is designed to target modern Linux
    systems.

2. Logging

    `provee` aims to send as much information as possible through Racket's
    built-in logging infrastructure. This allows users of the library to setup
    their own log receivers and react to events produced by `provee` as they see
    fit. `provee` even uses its own logged events to implement features. The
    `ensure:together` macro is designed to run its body arguments and merge all
    the results produced by other `ensure:` procedures into a single meta result.
    This can be useful in a case where you have a set of results and you want to
    perform some function if any of them have changed after running. It
    accomplishes this by simply setting up a log receiver on the
    `'provee/results` topic in a separate thread while running its body
    arguments and capturing the result objects as they're published, eventually
    providing the meta result back to the main thread to return as the result of
    the expression.

3. Result diffs and metadata

    All successful results returned by `ensure:` procedures are meant to contain
    diffs of relevent values and useful metadata about the result. `provee`
    provides procedures for displaying diffs of various types of values. For
    example, when displaying a file diff, the output is reminiscent of that from
    the `diff` command and when displying the difference of two integers labeled
    as file modes, they are displayed in octal because that's the format most
    would expect.


Example
=======

An example project exists in the `example/` directory that shows what installing
and configuring [Dnsmasq](http://www.thekelleys.org.uk/dnsmasq/doc.html) on a
home server might look like.
