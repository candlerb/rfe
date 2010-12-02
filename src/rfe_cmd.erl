-module(rfe_cmd).
-export([erl2erl/0, erl2term/0, beam2erl/0]).

erl2term() ->
  Forms = parse(erl),
  emit(term, Forms).

erl2erl() ->
  Forms = parse(erl),
  emit(erl, Forms).

beam2erl() ->
  Forms = parse(beam),
  emit(erl, Forms).

parse(beam) ->
  [Fname] = init:get_plain_arguments(),
  {ok, File} = file:read_file(Fname),
  {ok, {Mod, [{abstract_code,{raw_abstract_v1,Forms}}]}} = beam_lib:chunks(File, [abstract_code]),
  Forms;

parse(erl) ->
  [Fname] = init:get_plain_arguments(),
  % {ok, File} = file:read_file(Fname),
  % {ok, Tokens, _} = erl_scan:string(binary_to_list(File)),
  % {ok, Form} = erl_parse:parse_form(Tokens),
  {ok, Forms} = epp:parse_file(Fname, [], []),
  Forms.

emit(term, Forms) ->
  io:format("% ~w forms~n~p", [length(Forms), Forms]);

emit(erl, Forms) ->
  lists:foreach(fun(Form) -> io:put_chars(erl_pp:form(Form)) end, Forms).
