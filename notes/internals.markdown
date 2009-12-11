For tokenising, parsing, compiling and loading code dynamically, see
* http://jacobvorreuter.com/hacking-erlang-at-euc-2009
* http://github.com/JacobVorreuter/dynamic_compile
* http://www.erlang.org/doc/apps/erts/absform.html

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
