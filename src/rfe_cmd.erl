-module(rfe_cmd).
-export([convert/0]).

convert() ->
  InputType = case init:get_argument(t) of
    {ok, [[Flag]]} ->
      list_to_existing_atom(Flag);
    {ok, _} ->
      erlang:error(conflicting_flag);
    error ->
      auto
  end,
  OutputType = case init:get_argument(b) of
    {ok, [[Flag2]]} ->
      list_to_existing_atom(Flag2);
    {ok, _} ->
      erlang:error(conflicting_flag);
    error ->
      erl  % default output type
  end,
  Files = init:get_plain_arguments(),
  lists:foreach(fun(File) ->
    convert(InputType, OutputType, File) end, Files).

convert(auto, OutputType, File) ->
  InputType = case re:run(File, "\.[^.]+$", [{capture,all,list}]) of
    {match, [Ext]} ->
      io:write(Ext),
      case Ext of
        ".erl" -> erl;
        ".hrl" -> erl;
        ".beam" -> beam;
        ".rfe" -> rfe
      end;
    _ ->
      erlang:error(unknown_extension)
  end,
  convert(InputType, OutputType, File);

convert(InputType, OutputType, File) ->
  Forms = parse(InputType, File),
  emit(OutputType, Forms).

parse(beam, Fname) ->
  {ok, File} = file:read_file(Fname),
  Chunks = beam_lib:chunks(File, [abstract_code]),
  case Chunks of
  {ok, {_Mod, [{abstract_code,{raw_abstract_v1,Forms}}]}}
    -> Forms;
  {ok,{rfe_parse,[{abstract_code,no_abstract_code}]}}
    -> erlang:error(no_abstract_code)
  end;

parse(erl, Fname) ->
  % {ok, File} = file:read_file(Fname),
  % {ok, Tokens, _} = erl_scan:string(binary_to_list(File)),
  % {ok, Form} = erl_parse:parse_form(Tokens),
  {ok, Forms} = epp:parse_file(Fname, [], []),
  Forms;

parse(rfe, Fname) ->
  {ok, Forms} = rfepp:parse_file(Fname, [], []),
  Forms.

emit(abs, Forms) ->
  io:format("% ~w forms~n~p", [length(Forms), Forms]);

emit(erl, Forms) ->
  lists:foreach(fun(Form) ->
	case Form of
		{function, _, _, _, _} ->
			io:nl();
		_ ->
			undefined
	end,
	io:put_chars(erl_pp:form(Form))
  end, Forms).
