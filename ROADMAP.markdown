Basic Ruby-Flavoured Erlang
===========================

Stage 1: Line breaks, and reclaiming the semicolon
--------------------------------------------------

Syntax becomes line-oriented. Semicolon at end is optional, and allows
multiple items to be joined.

* Expressions separated by newline or semicolon

        1 + 2           -->     1 + 2,
        3 + 4                   3 + 4

        1 + 2; 3 + 4    -->     ditto

        1 +             -->     1 + 2
        2

* backslash-newline joins lines together

* This in turn means removing other uses of semicolon

* Guards: use 'and' and 'or' instead of ',' and ';'

* Block structure for try

        begin                   -->     try
          foo()                             foo()
        rescue error:badarith           catch
          "oops"                            error:badarith ->
        rescue => e                             "oops";
          "doh!"                            error:E ->
        ensure                                  "doh!"
          io:write("done")              after
        end                                 io:write("done")
                                        end

* Ditto for 'receive'

* 'case' with expression maps to pattern matching

        case X                  -->     case X of
        when {foo}                          {foo} ->
          io:write("is foo")                    io:write("is foo");
        when {bar, Y}                       {bar,Y} ->
          io:write(Y)                           io:write(Y);
        else                                _ ->
          io:write("doh!")                      io:write("doh!")
        end                             end

We can also implement case with rescues/ensure, which would map to
"try Expr of ..."

* case without expression maps to 'if', and just tests guard expressions

        case                    -->     if
        when is_integer(X)                  is_integer(X) ->
          "integer"                             "integer";
        when is_float(X)                    is_float(X) ->
          "float"                               "float";
        else                                true
          "unknown"                             "unknown"
        end                             end

(Can wrap in 'try' to add rescues/ensure)

* 'if' maps to nested 'case' statements, so that it can use expressions
  with side-effects

        if Foo < 3            -->     case Foo < 3 of
          "too low"                         true ->
        elsif Foo > 5                   "too low";
          "too high"                        _ ->
        else                                    case Foo > 5 of
          "just right"                              true ->
        end                                             "too high";
                                                    _ ->
                                                        "just right"
                                                end
                                        end

(perhaps this could be optimised, so if (expr) == 3 turns into
case (expr) of 3 -> ...)

* function definitions

    Start with 'def' and end with 'end'

        def foo(Bar) when Bar > 1       foo(Bar) when Bar > 1 ->
          Bar * 2                         Bar * 2.
        end

        def a({X})                -->   a({X}) ->
          X * X                             X * X,
        end                             a({X,Y}) ->
        def a({X,Y})                          X * Y.
          X * Y
        end

    If there are multiple definitions of foo/1 in the same source file with the
    same arity, combine them into clauses of foo/1.  (Do we enforce that they
    are adjacent?  In any case they need to be in the correct order for pattern
    matching)

* Top-level 'rescue'(s) and/or 'ensure' wrap the function in try/catch/after

        def a(X)                -->     a(X) ->
          1.0 / X                           try
        rescue error:badarith                   1.0 / X
          1.0                               catch
        end                                     error:badarith ->
                                                    1.0
                                          end.

* what syntax to use for anonymous functions with multiple clauses?

        ?                               -->     A = fun({X}) -> X * X;
                                                       ({X,Y}) -> X * Y end

(Remember that each clause can have a 'where' guard too)

Stage 2: sugar for blocks and self
----------------------------------

I observe that in the Erlang standard library:
* many functions take a fun as the first argument
* many functions take the main "thing" being operated on as the last argument

This suggests the following additional permitted syntax:

* block syntax for function calls

    If you provide a 'block' after a function call, it is prepended as the first
    argument in the call

        A = [1,2,3]                     -->     A = [1,2,3],
        lists:each(A) { |X| puts(X) }           lists:each(fun(X) -> puts(X) end, A)

    (Note: should block notation support multiple clauses?)

* use block syntax for funs too? See note above re. funs with multiple clauses

        A = fun { |X| X * X }         ? -->     A = fun(X) -> X * X end

        A = fun { |{X}| X * X         ? -->     A = fun({X}) -> X * X;
                  |{X,Y}| X * Y }                      ({X,Y}) -> X * Y end

* dot notation for function calls

    If you provide value.fn(args) then the value is passed as the last argument

        A = [1,2,3]                     -->     A = [1,2,3],
        A.lists:each() { |X| puts(X) }          lists:each(fun(X) -> puts(X) end, A)

        B = {x,y,z}                     -->     B = {x,y,z},
        B.element(2)                            element(2, B)

    (This is almost as convenient as B[1] in Reia!)

    We could also make the () optional for dot function calls with no arguments,
    giving `A.lists:each { |X puts(x) }`

    This gives a 'chained' syntax for nested function calls, e.g.

        A.lists:map { |X| X*2 }.lists:each { |X| puts(X) }

                                        -->     lists:each(fun(X) -> puts(X),
                                                   lists:map(fun(X) -> X*2, A))

    Note: does this conflict with record access syntax? e.g.

        Opts#server_opts.port

    Note: when converting erlang to rfe, we may have to decide whether to use
    the dot notation for all function calls, or just some, or none.

Stage 3: interactive rfe
------------------------

* module syntax

    Replace `-module(name)` with `module name ... end`. Probably don't allow
    multiple module definitions within the same source file though, as erlang
    assumes it can load module "foo" from file "foo.beam"

    Allow functions to be defined within the interactive shell, like Reia, and
    hence be able to update module definitions.  Each def a(..) would add
    (prepend?) a new pattern match for function a.  Need 'undef a' or 'undef
    a/2' to reset.

    Maybe all we need is to fetch the abstract_code chunk, update it,
    recompile and reload.

Wishlist (unprioritised)
========================

* tail conditionals

    io:write(...) if Debug

(This is useful as it needs no explicit 'else true' branch)

* inline rescue

        A = foo() rescue bar()  -->     A = try
                                            foo()
                                        catch
                                            error:_ ->
                                                bar()
                                        end.

(do we want to assign the error to something which can be used as "$!" ?
This might involve assigning vars like _err1, _err2 etc and remembering
which was the most recently bound, ugh)

* Implement rfe_pp, so we can convert erlang into RFE

* Implement rfec (erlc is written in C, but calls to erl_compile)

* At the moment we compile to the Erlang abstract form. This is good because
there's existing code to convert this back out to Erlang, but unfortunately
macros and includes have to be expanded by then.

    Ideally I would like to keep macros and includes as-is, so you can
    translate .hrl into .rfh and back again, but this may not work in all
    cases. Fortunately, some of the pathological ones are disallowed anyway:

        % This doesn't work
        foo(A, B
        -ifdef(extra_arg).
        ,C
        -endif.
        ) -> 0.
 
        % Nor does this
        -ifdef(asdf).
        junk
        -endif.

    Macros which are not valid Erlang syntactic forms _do_ work, but are
    not recommended:

        -define(Lbracket, [).
        -define(Rbracket, ]).

        foo() ->
          bar( ?Lbracket 1,2,3 ?Rbracket ).

    So perhaps a restricted solution would be acceptable, keeping the
    macro calls in the abstract form. Note that ?m(Y) might represent
    <expanded>(Y) or a parameterised macro call, but maybe that doesn't
    matter when translating erl<->rfe.

* I would also like to extend the abstract form to preserve comments
and position of newlines:

        A = 2 +  % adding's great   -->     A = 2 +  % adding's great
            3                                   3

* Optional value arguments

      def foo(A, B:=12, C:="hello", D) ->	foo(A, D) -> foo(A, 12, "hello", D).
        ...					foo(A, B, D) -> foo(A, B, "hello", D).
      end					foo(A, B, C, D) -> ... .

    (note that '=' is pattern match, so we need a different syntax
    for identifying defaults. Maybe real erlangers would use a record.)

Undecided
=========    

The consequences of the following changes are not fully thought through
and may never be implemented.

I like the simplicity of Erlang's convention: variables begin with
Uppercase, atoms begin with lower case (and atoms are also usable as
function names and module names).  Sticking with this would also make it
easier to switch between real erlang and rfe.

Reclaim the colon?
------------------

* Mark atoms with colon, like ruby symbols

        :foo            -->     foo
        :'foo bar       -->     'foo bar'
        :foo()          -->     foo()

* Use dot instead of colon for module function calls?

        :io.:write()    -->     io:write()

    This conflicts with the dot syntax already introduced. Is it sufficiently
    unambiguous to retain colon here? Consider:

        io:write
        :io:write
        io::write
        :io::write


Reclaim the upper-case letter, and allow bare function calls?
-------------------------------------------------------------

* Variables start with lower-case letters

* A bareword which matches a previously bound (or potentially bound)
  variable is a variable; otherwise it is a function call, and the arguments
  need not be in parentheses.

        a = 3           -->     A = 3,
        a                       A

        b               -->     b()
        b "foo","bar"   -->     b("foo","bar")

* This also applies to "expr . bareword" and "bareword . bareword"

        :io.write       -->     io:write()
        io.write        -->     io:write()

        a = :io         -->     a = io,
        a.write         -->     a:write()

    Maybe this 'poetry mode' is going to far?
    But if not, need a way to get the fun handle, e.g.
	&io.write	->	io:write
	io.write	->	io:write()

Reclaim the % and operators
---------------------------

* Comments start with '#' not '%'

* Have to decide what to do about record syntax :-(

* Use % instead of mod operator

* Use & | ^ << >> instead of bsl bsr band bor bxor bsl bsr
  (as infix operators; I don't think << and >> will clash with binaries)

* and/&& map to 'andalso'; or/|| map to 'orelse'
  (does || clash with list and binary comprehensions? Maybe we should just
  keep the word versions)

Miscellaneous
-------------

* Use ?x instead of $x  [just for rubyness]

* Add ? : ternary operator

* Replacement preprocessor. Should we use words starting with
  capital letters as macros?

* Replacement syntax for compiler directives

* It would more ruby-like to avoid the 'export' keyword, instead using
'public' and 'private' to set visibility

        private
          # default is private here
        private :a
        public 'a/2'  # ??

* Should we allow nested defs? Would just be sugar for fun

        def a(X)                ->      a(X) ->
          def sq(Y)                         Sq = fun(Y) ->
            Y*Y                                 Y*Y,
          end                               Sq(X) + 2*X.
          sq(X) + 2*X
        end

Even more blue sky ideas
========================

Patterns
--------

Be able to wrap entire patterns such as gen_server. For example:

    class Thing
      def initialize
        0
      end
      def initialize(n)
        n
      end
      def inc(state)
        {state, state+1}
      end
    end

might become three modules: a spawner, a parameterised API module, and a
gen_server implementation module.

    -module(thing).
    -export([create/0, create/1]).
    create() -> create_args([]).
    create(N) -> create_args([N]).
    create_args(Args) ->
        {ok, Pid} = gen_server:start_link(thing_impl, Args, []),
        thing_intf:new(Pid).

    -----

    -module(thing_intf, [Pid]).
    -export([inc/0, destroy/0]).
    inc() ->
        gen_server:call(Pid, {inc}).
    destroy() ->
        gen_server:call(Pid, stop).

    -----

    -module(thing_impl).
    -behavior(gen_server).
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
    init([]) -> {ok, 0};
    init([Count]) -> {ok, Count}.
    handle_call({inc}, _From, State) ->
        {reply, State, State+1}.
    handle_cast(_Msg, State) -> {noreply, State}.
    handle_info(_Info, State) -> {noreply, State}.
    terminate(_Reason, _State) -> ok.
    code_change(_OldVsn, State, _Extra) -> {ok, State}.

Hence you could use it like this:

    counter = Thing.new
    io.write counter.inc
    io.write counter.inc

(The only problem with starting processes with a pid like this is that they
won't be garbage-collected automatically, so it's probably still essential
to be able to register and unregister them; although careful use of linking
may help)

Promote binaries
----------------
 
I would like the double-quote "..." syntax to produce a binary rather than a
list of characters; perhaps have another syntax like %l{...} for a list of
characters, or use single quotes.

However this adds some awkwardness for basic operations like concatenation:

    c = <<a/binary, b/binary>>

Perhaps there should be a binary concatenation operator, e.g. +++ (ugh)
else it's `c = list_to_binary([a,b])`

Tuples and hash literals
------------------------

* Reia uses (1,2,3) for tuples instead of {1,2,3}. Should this be copied?
  Would it introduce too much ambiguity around function calls? Then we can
  re-use {..} syntax for dict literals (or proplists)

        {}			->	dict:new()
        {:foo=>1, :bar=>2}	->	dict:from_list([(foo,1),(bar,2)])

Miscellaneous
-------------

* Maybe: && and || map to case statements, so you can say `x = y || "default"`
  (whereas 'and' and 'or' only work on boolean values). But then what would
  we decide are false values? Atom :undefined perhaps?

* Occam uses '?' for receive, which is nicely compact and mirrors '!' for
  send. But then you need 'alt' to handle pattern matching and timeouts,
  which is pretty much the same as erlang's 'receive'.

