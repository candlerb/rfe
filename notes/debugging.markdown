c(adder,debug_info).    -- compile with debug info
debugger:start().

dbg:tracer().
dbg:p(Pid|all,[c]).                      -- trace function calls
dbg:p(Pid|all,[m,c,p,sos,timestamp]).    -- more useful info
dbg:tpl(Module,[{'_'},[],[{return_trace}]}]).
dbg:tpl(Module,[{'_'},[],[{exception_trace}]}]).

erlang:trace(whereis(Regname),false,[arity]).
