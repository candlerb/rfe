For tokenising, parsing, compiling and loading code dynamically, see
* <http://jacobvorreuter.com/hacking-erlang-at-euc-2009>
* <http://github.com/JacobVorreuter/dynamic_compile>
* [Abstract form](http://www.erlang.org/doc/apps/erts/absform.html)
* [Compiler overview](http://forum.trapexit.org/mailinglists/viewtopic.php?p=23378&sid=6554b2600dde58c7d218567f8ce53ef3)

Compiling and loading forms:

    {ok, Mod, Bin} = compile:forms(Forms,[]),
    code:load_binary(Mod, [], Bin).

Pass option {parse_transform, Mod} to the compiler, and it will call
Mod:parse_transform/2 after parsing but before the code is checked for
errors.

If a module is compiled with debug_info, it will contain an abstract_code
chunk.  This can be retrieved from the beam file:

    {ok,{Mod,[{abstract_code,{raw_abstract_v1,Forms}}]}} =
        beam_lib:chunks("ebin/rfe_cmd",[abstract_code]).

The shell function m(Mod) will show you parameters for a loaded module; it
just calls Mod:module_info(), see
<http://www.erlang.org/doc/reference_manual/modules.html>.  This lets you
get at a lot of metadata about the module, but the abstract code is
discarded.

See also:
    code:is_loaded(Mod)
    code:all_loaded()
    erlang:loaded()

Shell functions
---------------

Shell functions are defined in module 'c' (lib/stdlib/src/c.erl).
