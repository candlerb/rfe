-module(rfe_cmd).
-export([erl2erl/0, erl2term/0]).

erl2term() ->
  Forms = parsefile(),
  io:format("% ~w forms~n~p", [length(Forms), Forms]).

erl2erl() ->
  Forms = parsefile(),
  lists:foreach(fun(Form) -> io:put_chars(erl_pp:form(Form)) end, Forms).

parsefile() ->
  [Fname] = init:get_plain_arguments(),
  % {ok, File} = file:read_file(Fname),
  % {ok, Tokens, _} = erl_scan:string(binary_to_list(File)),
  % {ok, Form} = erl_parse:parse_form(Tokens),
  {ok, Forms} = epp:parse_file(Fname, [], []),
  Forms.

