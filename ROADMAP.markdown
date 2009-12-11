Line breaks, and reclaiming the semicolon
-----------------------------------------

* Expressions separated by newline or semicolon

        1 + 2           -->     1 + 2,
        3 + 4                   3 + 4

        1 + 2; 3 + 4    -->     ditto

        1 +             -->     1 + 2
        2

* backslash-newline joins lines together

* This in turn means removing other uses of semicolon

* Guards: use 'and' and 'or' for ',' and ';'

* Fun clauses: see below for new syntax

* Block structure for try

        begin                   -->     try
          foo                               foo()
        rescue error:badarith           catch
          "oops"                            error:badarith ->
        rescue => e                             "oops";
          "doh!"                                    error:E ->
        ensure                                  "doh!"
          io.write "done"               after
        end                                 io:write("done")
                                        end

* Ditto for 'receive'

* 'case' with expression maps to pattern matching

        case x                  -->     case X of
        when {:foo}                         {foo} ->
          io.write "is foo"                     io.write("is foo");
        when {:bar, y}                      {bar,Y} ->
          io:write y                            io.write(Y);
        else                                _ ->
          io.write "doh!"                       io.write("doh!")
        end                             end

We can also implement case with rescues/ensure, which would map to
"try Expr of ..."

* case without expression maps to 'if', and just tests guard expressions

        case                    -->     if
        when is_integer(x)                  is_integer(X)
          "integer"                             "integer";
        when is_float(x)                    is_float(X);
          "float"                               "float";
        else                                true
          "unknown"                             "unknown"
        end                             end

(Can wrap in 'try' for rescues/ensure)

* 'if' maps to nested 'case' statements, so that it can use expressions
  with side-effects

        if foo < 3              -->     case foo() < 3 of
          "too low"                         true ->
        elsif foo > 5                   "too low";
          "too high"                        _ ->
        else                                    case foo() > 5 of
          "just right"                              true ->
        end                                             "too high";
                                                    _ ->
                                                        "just right"
                                                end
                                        end

* inline rescue

        a = foo rescue bar      -->     A = try
                                            foo()
                                        catch
                                            error:_ ->
                                                bar()
                                        end.

Function calls and lambdas
--------------------------

* 'Block' syntax {..} and do..end creates an extra first argument to
  a function call.

        a = [1,2,3]                     -->     A = [1,2,3],
        lists.each(a) { |x| puts x }            lists:each(fun(X) -> puts X end, A)

* New syntax for funs

        a = fun { |x| x * x }           -->     A = fun(X) -> X * X end

        a = fun { |{x}| x * x           -->     A = fun({X}) -> X * X;
                  |{x,y}| x * y }                      ({X,Y}) -> X * Y end

* 'lambda' as an alias for 'fun'

Function definitions
--------------------

* Start with 'def' and end with 'end'

        def a({x})                -->   a({X}) ->
          x * x                             X * X,
        end                             a({X,Y}) ->
        def a({x,y})                          X * Y.
          x * y
        end

* Top-level 'rescue'(s) and/or 'ensure' wrap the function in try/catch/after

        def a(x)                -->     a(X) ->
          1.0 / x                           try
        rescue error:badarith                   1.0 / X
          1.0                               catch
        end                                     error:badarith ->
                                                  1.0
                                          end.

(It might be good to allow these functions to be in an arbitrary order in
the source file, and to link branches of the same name+arity together
automatically at the end - but not essential.  In any case the user needs to
put the definitions in the correct order)

Similarly, it would be good to avoid the 'export' keyword, instead using
'public' and 'private' to set visibility

        private
          # default is private here
        private :a
        public 'a/2'

* Should we allow nested defs? Would just be sugar for lambda

        def a(x)                ->      a(X) ->
          def sq(y)                         Sq = fun(Y) ->
            y*y                         Y*Y,
          end                               Sq(X) + 2*X.
          sq(x) + 2*x
        end

Reclaim the colon
-----------------

* Mark atoms with colon, like ruby symbols

        :foo            -->     foo
        :'foo bar       -->     'foo bar'
        :foo()          -->     foo()

* Use dot instead of colon for module function calls

        :io.:write()    -->     io:write()

Reclaim the upper-case letter, and allow bare function calls
------------------------------------------------------------

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

Reclaim the % and operators
---------------------------

* Comments start with '#' not '%'

* Have to decide what to do about record syntax :-(

* Use % instead of mod operator

* Use & | ^ << >> instead of bsl bsr band bor bxor bsl bsr
  (as infix operators, I don't think << and >> will clash with binaries)

* and/&& map to 'andalso'; or/|| map to 'orelse'
  (does || clash with list and binary comprehensions? Maybe we should just
  keep the word versions)

Miscellaneous
-------------

* Use ?x instead of $x  [just for rubyness]

* Add ? : ternary operator

* Need a replacement preprocessor :-( Should we use words starting with
  capital letters as macros?

* Need a replacement syntax for compiler directives
    
Future/blue sky ideas
=====================

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

Dynamic method definitions
--------------------------

Would like to be able to do 'def' within the interactive shell, and hence
update module definitions. Each def a(..) would add a new pattern match for
function a. Need 'undef a' or 'undef a/2' to reset.

Maybe all we need is to fetch the abstract_code chunk, update it,
recompile and reload.

Promote binaries
----------------
 
I would like the "..." syntax to produce a binary rather than a list of
characters; perhaps have another syntax like %l{...} for a list of
characters.

However this adds some awkwardness for basic operations like concatenation:

   c = <<a/binary, b/binary>>

Perhaps there should be a binary concatenation operator, e.g. +++ (ugh)

Preserve comments
-----------------

Extend the abstract form to preserve comments, so that rfe2erl can keep them
as well.

    a = 2 +  # adding's great   -->     A = 2 +  % adding's great
        3                                   3

Miscellaneous
-------------

* Maybe: && and || map to case statements, so you can say 'x = y || "default"'
  (whereas 'and' and 'or' only work on boolean values). But then what would
  we decide are false values? Atom :undefined perhaps?

* Occam uses '?' for receive, which is nicely compact and mirrors '!' for
  send. But then you need 'alt' to handle pattern matching and timeouts,
  which is pretty much the same as erlang's 'receive'.

* rfe_pp module, and erl2rfe

* Reia uses (1,2,3) for tuples instead of {1,2,3}. Should this be copied?
  Would it introduce too much ambiguity around function calls? Can we re-use
  {..} syntax for something else, e.g. dict:from_list?

