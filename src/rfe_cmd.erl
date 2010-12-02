-module(rfe_cmd).
-export([erl2erl/0, erl2abs/0, beam2erl/0]).

erl2abs() ->
  Forms = parse(erl),
  emit(abs, Forms).

erl2erl() ->
  Forms = parse(erl),
  emit(erl, Forms).

beam2erl() ->
  Forms = parse(beam),
  emit(erl, Forms).

parse(beam) ->
  [Fname] = init:get_plain_arguments(),
  {ok, File} = file:read_file(Fname),
  Chunks = beam_lib:chunks(File, [abstract_code]),
  case Chunks of
  {ok, {_Mod, [{abstract_code,{raw_abstract_v1,Forms}}]}}
    -> Forms;
  {ok,{rfe_parse,[{abstract_code,no_abstract_code}]}}
    -> erlang:error(no_abstract_code)
  end;

parse(erl) ->
  [Fname] = init:get_plain_arguments(),
  % {ok, File} = file:read_file(Fname),
  % {ok, Tokens, _} = erl_scan:string(binary_to_list(File)),
  % {ok, Form} = erl_parse:parse_form(Tokens),
  {ok, Forms} = epp:parse_file(Fname, [], []),
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
