erl_crash.dump
--------------

If your program dies and leaves an erl_crash.dump file lying around,
start a new erl shell, type

    webtool:start().

and then point your web browser at http://127.0.0.1:8888/

You can use this web interface to start the crashdump viewer, upload
the dump file, and then analyse the machine state in great detail. In
particular you can see all the processes and their stack state.

The crashdump file is actually plain text so you can grep it, if you
know what you're looking for.

See <http://www.erlang.org/doc/apps/erts/crash_dump.html>

Other notes
-----------
    c(adder,debug_info).    -- compile with debug info
    debugger:start().

    dbg:tracer().
    dbg:p(Pid|all,[c]).                      -- trace function calls
    dbg:p(Pid|all,[m,c,p,sos,timestamp]).    -- more useful info
    dbg:tpl(Module,[{'_'},[],[{return_trace}]}]).
    dbg:tpl(Module,[{'_'},[],[{exception_trace}]}]).

    erlang:trace(whereis(Regname),false,[arity]).
