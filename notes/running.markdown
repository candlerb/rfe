Running and Compiling Erlang
============================

Interactive Erlang
------------------

The nearest equivalent to irb is erl, the erlang shell. Type `erl` to start
it. I also suggest you try `erl +Bc` which gives a more familiar behaviour to
Ctrl-C (it just interrupts the expression you're currently running)

When you want to exit, use one of the following:

    q().             -- clean shutdown, shortcut for init:stop().
    halt().          -- hard exit, short for erlang:halt().
    Ctrl-G q         -- shell quit
    Ctrl-C a         -- hard exit (if *not* started with +Bc)

There are a number of shell built-in commands of which `q().` is one.
Type `help().` for a list of them all.

Ctrl-G is a short of built-in job control. You can have multiple shell
sessions and switch between them. Ctrl-G followed by `h` gives you help.

You can enter expressions interactively, but you cannot define functions. 
The nearest you can do is to make an anonymous function (i.e. a lambda) and
bind it to a variable.

    1> 2 + 2.
    4
    2> Adder = fun(A,B) -> A + B end.
    #Fun<erl_eval.12.113037538>
    3> Adder(2,3).
    5

Once you have bound a variable you can't rebind it to a different value
unless you use the shell command  `f(Var).` to unbind Var, or `f().` to
forget all bindings.

    8> A = 1.
    1
    9> A = 2.
    ** exception error: no match of right hand side value 2
    10> f().
    ok
    11> A = 2.
    2

If you want to run some code repeatedly which binds a variable, a trick is
to put it into an anonymous function and call it. For example, the following
line will check if there are any messages in the shell process's mailbox:

    6> RX = fun() -> receive Y -> io:write(Y) after 0 -> io:write(nothing) end end.
    #Fun<erl_eval.20.67289768>
    7> RX().
    nothingok
    8> RX().
    nothingok

You can also define the function and call it without binding it to a variable.

    fun() -> receive Y -> io:write(Y) after 0 -> io:write(nothing) end end ().

Interactive with GUI toolbar
----------------------------

    erl -s toolbar

gives you an interactive shell plus a GUI toolbar (for monitoring processes,
starting the debugger, and viewing ETS/Mnesia tables)

[More info and examples](http://ns.erlang.org/documentation/doc-5.2/doc/getting_started/getting_started.html)

Compiling erlang
----------------

Erlang source files consist of *functions* within a *module*. Functions
which are 'exported' are available for calling from outside; others are
private.  Here the function fac/1 is exported (meaning the function with
name 'fac' which takes 1 argument)

    -module(factorial).      %% MUST MATCH FILENAME, factorial.erl
    -export([fac/1]).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).

You can compile this from the command line:

    erlc factorial.erl       #  OR: erlc +debug_info factorial.erl

Or from within the erl shell:

    1> c(factorial).         %% OR: c(factorial,debug_info).

This creates `factorial.beam`. Once compiled, it is loaded on demand when
referenced, so you can use it straight away from the the erl shell:

    2> factorial:fac(5).
    120

If you modify factorial.erl and recompile it from the shell, you'll get
the new module loaded. But if you recompile it externally (using erlc at
the command line), you'll need to force it to be reloaded using `l`.

    3> l(factorial).
    {module,factorial}

Starting erlang from the command line
-------------------------------------

When erlang starts you can get it to call a function within a module
(i.e. your application's entry point) instead of the shell. This function
can be called whatever you like, but if you don't specify a name erlang
will try to call start/0.

You can also pass arguments to the function, but if you do, these are passed
as a list of atoms (not strings or integers).  We also want to print the
result, so here is a modified version of the sample code:

    -module(factorial2).      %% MUST MATCH FILENAME, factorial2.erl
    -export([main/1]).

    main([Arg]) ->
        N = list_to_integer(atom_to_list(Arg)),
        F = fac(N),
        io:format("factorial ~w = ~w\n", [N,F]).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).

Now we can compile and run it like this:

    $ erlc factorial2.erl
    $ erl -noshell -s factorial2 main 5 -s init stop
    factorial 5 = 120

The -s flags mean that first factorial2:main is called with the given
argument; once that has completed, init:stop() is called which terminates
the erlang runtime.

Using -extra arguments
----------------------

Another way of passing arguments to a program is to use

    erl ... -extra MORE ARGS HERE

These values are returned by init:get_plain_arguments() as a list of strings.

ebin directory
--------------

Conventially, your compiled .beam files would go in a 'ebin' directory.
To arrange this, you need to tell the compiler to put the output there,
and add your ebin directory to the code search path at runtime.

    $ mkdir ebin
    $ erlc -o ebin factorial2.erl
    $ erl -pa ebin -noshell -s factorial2 main 5 -s init stop
    factorial 5 = 120

The -pa option can be repeated as many times as required. Here is a slightly
longer example from reia's `ire` startup script:

    erl +K true -pa ebin -pa ../ebin -noshell -noinput -s ire init -extra $*

There are many other options available when running erlang - e.g. detached
process, heartbeat monitoring, multiple nodes.  Have a look at the couchdb
script for a much bigger example.

compiling with a Rakefile
-------------------------

Here is a simple Rakefile which will compile erlang source from anywhere
under src/ and lump all the compiled modules into a flat ebin/ directory.

    directory 'ebin'
    task :default => 'ebin'

    def output_file(input_file, dir = 'ebin/')
      dir + File.basename(input_file,'.erl') + '.beam'
    end

    SRC_FILES = Dir['src/**/*.erl']
    SRC_FILES.each do |src|
      file output_file(src) => [src] do
        sh "erlc +debug_info -o ebin #{src}"
      end
      task :default => output_file(src)
    end

escript
-------

You can write erlang scripts which can be run directly without a compilation
step.  See `man escript` for details.  In this case the entry point must be
a function `main/1` and it takes an array of strings as its argument.

    #!/usr/bin/escript
    %% -*- erlang -*-
    %%! -smp enable -sname factorial -mnesia debug verbose
    main([String]) ->
        try
            N = list_to_integer(String),
            F = fac(N),
            io:format("factorial ~w = ~w\n", [N,F])
        catch
            _:_ ->
                usage()
        end;
    main(_) ->
        usage().

    usage() ->
        io:format("usage: factorial integer\n"),
        halt(1).

    fac(0) -> 1;
    fac(N) -> N * fac(N-1).

You can make this executable using `chmod +x` and then it runs. The startup
overhead is pretty substantial compared to ruby though.

escript with .beam
------------------

It is also possible to use escript to execute a compiled .beam file. Rename
the escript file as factorial3.erl and replace the initial comments with:

    -module(factorial3).      %% MUST MATCH FILENAME, factorial3.erl
    -export([main/1]).

Then compile it using `erlc factorial3.erl`.

Now create a header file "escript-header" like this:

    #!/usr/bin/escript
    %%! -smp enable -sname factorial -mnesia debug verbose

Finally, concatenate the two.

    cat escript-header factorial3.beam >factorial3
    chmod +x factorial3

This is now executable. Unfortunately, this doesn't reduce the startup
overhead by any noticeable margin for a small script.  But since the code is
compiled rather than interpreted, it should run faster.

For anything other than a single-file project, it is almost certainly be
better to have a small sh script which starts erl with the appropriate
flags.
