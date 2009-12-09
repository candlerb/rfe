%% -*- erlang -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% Modified by Brian Candler 2009.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%% Definition of the Ruby-Flavoured Erlang grammar.
%% This is just a hacked-up version of stdlib erl_parse.yrl

Nonterminals
form
attribute attr_val
function function_clauses function_clause
clause_args clause_guard clause_body
expr expr_100 expr_150 expr_160 expr_200 expr_300 expr_400 expr_500
expr_600 expr_700 expr_800 expr_900
expr_max
list tail
list_comprehension lc_expr lc_exprs
binary_comprehension 
tuple
atom1
%struct
record_expr record_tuple record_field record_fields
if_expr if_clause if_clauses case_expr cr_clause cr_clauses receive_expr
fun_expr fun_clause fun_clauses
%% cond_expr cond_clause cond_clauses
try_expr try_catch try_clause try_clauses query_expr
function_call argument_list
exprs guard
atomic strings
prefix_op mult_op add_op list_op comp_op
rule rule_clauses rule_clause rule_body
binary bin_elements bin_element bit_expr
opt_bit_size_expr bit_size_expr opt_bit_type_list bit_type_list bit_type
top_type top_type_100 top_types type typed_expr typed_attr_val
type_sig type_sigs type_guard type_guards fun_type fun_type_100 binary_type
type_spec spec_fun typed_exprs typed_record_fields field_types field_type
bin_base_type bin_unit_type int_type.

Terminals
char integer float atom string var

'(' ')' ',' '->' ':-' '{' '}' '[' ']' '|' '||' '<-' ';' ':' '#' '.'
'after' 'begin' 'case' 'try' 'catch' 'end' 'fun' 'if' 'of' 'receive' 'when'
'andalso' 'orelse' 'query' 'spec'
%% 'cond'
'bnot' 'not'
'*' '/' 'div' 'rem' 'band' 'and'
'+' '-' 'bor' 'bxor' 'bsl' 'bsr' 'or' 'xor'
'++' '--'
'==' '/=' '=<' '<' '>=' '>' '=:=' '=/=' '<='
'<<' '>>'
'!' '=' '::'
dot.

Expect 2.

Rootsymbol form.

form -> attribute dot : '$1'.
form -> function dot : '$1'.
form -> rule dot : '$1'.

attribute -> '-' atom attr_val               : build_attribute('$2', '$3').
attribute -> '-' atom typed_attr_val         : build_typed_attribute('$2','$3').
attribute -> '-' atom '(' typed_attr_val ')' : build_typed_attribute('$2','$4').
attribute -> '-' 'spec' type_spec            : build_type_spec('$2', '$3').
   
atom1 -> 'spec' : {atom, ?line('$1'), 'spec'}.
atom1 -> atom   : '$1'.

type_spec -> spec_fun type_sigs : {'$1', '$2'}.
type_spec -> '(' spec_fun type_sigs ')' : {'$2', '$3'}.

spec_fun ->                            atom1 : '$1'.
spec_fun ->                  atom1 ':' atom1 : {'$1', '$3'}.
%% The following two are retained only for backwards compatibility;
%% they are not part of the EEP syntax and should be removed.
spec_fun ->           atom1 '/' integer '::' : {'$1', '$3'}.
spec_fun -> atom1 ':' atom1 '/' integer '::' : {'$1', '$3', '$5'}.

typed_attr_val -> expr ',' typed_record_fields : {typed_record, '$1', '$3'}.
typed_attr_val -> expr '::' top_type           : {type_def, '$1', '$3'}.

typed_record_fields -> '{' typed_exprs '}' : {tuple, ?line('$1'), '$2'}.

typed_exprs -> typed_expr                 : ['$1'].
typed_exprs -> typed_expr ',' typed_exprs : ['$1'|'$3'].
typed_exprs -> expr ',' typed_exprs       : ['$1'|'$3'].
typed_exprs -> typed_expr ',' exprs       : ['$1'|'$3'].

typed_expr -> expr '::' top_type          : {typed,'$1','$3'}.

type_sigs -> type_sig                     : ['$1'].
type_sigs -> type_sig ';' type_sigs       : ['$1'|'$3'].

type_sig -> fun_type                      : '$1'.
type_sig -> fun_type 'when' type_guards   : {type, ?line('$1'), bounded_fun, 
                                             ['$1','$3']}.

type_guards -> type_guard                 : ['$1'].
type_guards -> type_guard ',' type_guards : ['$1'|'$3'].

type_guard -> atom1 '(' top_types ')'     : {type, ?line('$1'), constraint, 
                                             ['$1', '$3']}.

top_types -> top_type                     : ['$1'].
top_types -> top_type ',' top_types       : ['$1'|'$3'].

top_type -> var '::' top_type_100         : {ann_type, ?line('$1'), ['$1','$3']}.
top_type -> top_type_100                  : '$1'.

top_type_100 -> type                      : '$1'.
top_type_100 -> type '|' top_type_100     : lift_unions('$1','$3').

type -> '(' top_type ')'                  : {paren_type, ?line('$2'), ['$2']}.
type -> var                               : '$1'.
type -> atom1                             : '$1'.
type -> atom1 '(' ')'                     : build_gen_type('$1').
type -> atom1 '(' top_types ')'           : {type, ?line('$1'), 
                                             normalise('$1'), '$3'}.
type -> atom1 ':' atom1 '(' ')'           : {remote_type, ?line('$1'), 
                                             ['$1', '$3', []]}.
type -> atom1 ':' atom1 '(' top_types ')' : {remote_type, ?line('$1'), 
                                             ['$1', '$3', '$5']}.
type -> '[' ']'                           : {type, ?line('$1'), nil, []}.
type -> '[' top_type ']'                  : {type, ?line('$1'), list, ['$2']}.
type -> '[' top_type ',' '.' '.' '.' ']'  : {type, ?line('$1'), 
                                             nonempty_list, ['$2']}.
type -> '{' '}'                           : {type, ?line('$1'), tuple, []}.
type -> '{' top_types '}'                 : {type, ?line('$1'), tuple, '$2'}.
type -> '#' atom1 '{' '}'                 : {type, ?line('$1'), record, ['$2']}.
type -> '#' atom1 '{' field_types '}'     : {type, ?line('$1'), 
                                             record, ['$2'|'$4']}.
type -> binary_type                       : '$1'.
type -> int_type                          : '$1'.
type -> int_type '.' '.' int_type         : {type, ?line('$1'), range, 
                                             ['$1', '$4']}.
type -> 'fun' '(' ')'                     : {type, ?line('$1'), 'fun', []}.
type -> 'fun' '(' fun_type_100 ')'        : '$3'.

int_type -> integer                       : '$1'.
int_type -> '-' integer                   : abstract(-normalise('$2'), 
                                                     ?line('$2')).

fun_type_100 -> '(' '.' '.' '.' ')' '->' top_type 
                                          : {type, ?line('$1'), 'fun',
                                             [{type, ?line('$1'), any}, '$7']}.
fun_type_100 -> fun_type                  : '$1'.

fun_type -> '(' ')' '->' top_type  : {type, ?line('$1'), 'fun',
                                      [{type, ?line('$1'), product, []}, '$4']}.
fun_type -> '(' top_types ')' '->' top_type 
                                   : {type, ?line('$1'), 'fun',
                                      [{type, ?line('$1'), product, '$2'},'$5']}.

field_types -> field_type                 : ['$1'].
field_types -> field_type ',' field_types : ['$1'|'$3'].

field_type -> atom1 '::' top_type          : {type, ?line('$1'), field_type, 
                                             ['$1', '$3']}.

binary_type -> '<<' '>>'                  : {type, ?line('$1'),binary, 
					     [abstract(0, ?line('$1')), 
					      abstract(0, ?line('$1'))]}.
binary_type -> '<<' bin_base_type '>>'    : {type, ?line('$1'),binary,
					     ['$2', abstract(0, ?line('$1'))]}.
binary_type -> '<<' bin_unit_type '>>'    : {type, ?line('$1'),binary,
                                             [abstract(0, ?line('$1')), '$2']}.
binary_type -> '<<' bin_base_type ',' bin_unit_type '>>'
                                    : {type, ?line('$1'), binary, ['$2', '$4']}.

bin_base_type -> var ':' integer          : build_bin_type(['$1'], '$3').

bin_unit_type -> var ':' var '*' integer  : build_bin_type(['$1', '$3'], '$5').

attr_val -> expr                     : ['$1'].
attr_val -> expr ',' exprs           : ['$1' | '$3'].
attr_val -> '(' expr ',' exprs ')'   : ['$2' | '$4'].

function -> function_clauses : build_function('$1').

function_clauses -> function_clause : ['$1'].
function_clauses -> function_clause ';' function_clauses : ['$1'|'$3'].

function_clause -> atom1 clause_args clause_guard clause_body :
	{clause,?line('$1'),element(3, '$1'),'$2','$3','$4'}.


clause_args -> argument_list : element(1, '$1').

clause_guard -> 'when' guard : '$2'.
clause_guard -> '$empty' : [].

clause_body -> '->' exprs: '$2'.


expr -> 'catch' expr : {'catch',?line('$1'),'$2'}.
expr -> expr_100 : '$1'.

expr_100 -> expr_150 '=' expr_100 : {match,?line('$2'),'$1','$3'}.
expr_100 -> expr_150 '!' expr_100 : ?mkop2('$1', '$2', '$3').
expr_100 -> expr_150 : '$1'.

expr_150 -> expr_160 'orelse' expr_150 : ?mkop2('$1', '$2', '$3').
expr_150 -> expr_160 : '$1'.

expr_160 -> expr_200 'andalso' expr_160 : ?mkop2('$1', '$2', '$3').
expr_160 -> expr_200 : '$1'.

expr_200 -> expr_300 comp_op expr_300 :
	?mkop2('$1', '$2', '$3').
expr_200 -> expr_300 : '$1'.

expr_300 -> expr_400 list_op expr_300 :
	?mkop2('$1', '$2', '$3').
expr_300 -> expr_400 : '$1'.

expr_400 -> expr_400 add_op expr_500 :
	?mkop2('$1', '$2', '$3').
expr_400 -> expr_500 : '$1'.

expr_500 -> expr_500 mult_op expr_600 :
	?mkop2('$1', '$2', '$3').
expr_500 -> expr_600 : '$1'.

expr_600 -> prefix_op expr_700 :
	?mkop1('$1', '$2').
expr_600 -> expr_700 : '$1'.

expr_700 -> function_call : '$1'.
expr_700 -> record_expr : '$1'.
expr_700 -> expr_800 : '$1'.

expr_800 -> expr_900 ':' expr_max :
	{remote,?line('$2'),'$1','$3'}.
expr_800 -> expr_900 : '$1'.

expr_900 -> '.' atom1 :
	{record_field,?line('$1'),{atom,?line('$1'),''},'$2'}.
expr_900 -> expr_900 '.' atom1 :
	{record_field,?line('$2'),'$1','$3'}.
expr_900 -> expr_max : '$1'.

expr_max -> var : '$1'.
expr_max -> atomic : '$1'.
expr_max -> list : '$1'.
expr_max -> binary : '$1'.
expr_max -> list_comprehension : '$1'.
expr_max -> binary_comprehension : '$1'.
expr_max -> tuple : '$1'.
%%expr_max -> struct : '$1'.
expr_max -> '(' expr ')' : '$2'.
expr_max -> 'begin' exprs 'end' : {block,?line('$1'),'$2'}.
expr_max -> if_expr : '$1'.
expr_max -> case_expr : '$1'.
expr_max -> receive_expr : '$1'.
expr_max -> fun_expr : '$1'.
%%expr_max -> cond_expr : '$1'.
expr_max -> try_expr : '$1'.
expr_max -> query_expr : '$1'.


list -> '[' ']' : {nil,?line('$1')}.
list -> '[' expr tail : {cons,?line('$1'),'$2','$3'}.

tail -> ']' : {nil,?line('$1')}.
tail -> '|' expr ']' : '$2'.
tail -> ',' expr tail : {cons,?line('$2'),'$2','$3'}.


binary -> '<<' '>>' : {bin,?line('$1'),[]}.
binary -> '<<' bin_elements '>>' : {bin,?line('$1'),'$2'}.

bin_elements -> bin_element : ['$1'].
bin_elements -> bin_element ',' bin_elements : ['$1'|'$3'].

bin_element -> bit_expr opt_bit_size_expr opt_bit_type_list :
	{bin_element,?line('$1'),'$1','$2','$3'}.

bit_expr -> prefix_op expr_max : ?mkop1('$1', '$2').
bit_expr -> expr_max : '$1'.

opt_bit_size_expr -> ':' bit_size_expr : '$2'.
opt_bit_size_expr -> '$empty' : default.

opt_bit_type_list -> '/' bit_type_list : '$2'.
opt_bit_type_list -> '$empty' : default.

bit_type_list -> bit_type '-' bit_type_list : ['$1' | '$3'].
bit_type_list -> bit_type : ['$1'].

bit_type -> atom1             : element(3,'$1').
bit_type -> atom1 ':' integer : { element(3,'$1'), element(3,'$3') }.

bit_size_expr -> expr_max : '$1'.


list_comprehension -> '[' expr '||' lc_exprs ']' :
	{lc,?line('$1'),'$2','$4'}.
binary_comprehension -> '<<' binary '||' lc_exprs '>>' :
	{bc,?line('$1'),'$2','$4'}.
lc_exprs -> lc_expr : ['$1'].
lc_exprs -> lc_expr ',' lc_exprs : ['$1'|'$3'].

lc_expr -> expr : '$1'.
lc_expr -> expr '<-' expr : {generate,?line('$2'),'$1','$3'}.
lc_expr -> binary '<=' expr : {b_generate,?line('$2'),'$1','$3'}.

tuple -> '{' '}' : {tuple,?line('$1'),[]}.
tuple -> '{' exprs '}' : {tuple,?line('$1'),'$2'}.


%%struct -> atom1 tuple :
%%	{struct,?line('$1'),element(3, '$1'),element(3, '$2')}.


%% N.B. This is called from expr_700.
%% N.B. Field names are returned as the complete object, even if they are
%% always atoms for the moment, this might change in the future.

record_expr -> '#' atom1 '.' atom1 :
	{record_index,?line('$1'),element(3, '$2'),'$4'}.
record_expr -> '#' atom1 record_tuple :
	{record,?line('$1'),element(3, '$2'),'$3'}.
record_expr -> expr_max '#' atom1 '.' atom1 :
	{record_field,?line('$2'),'$1',element(3, '$3'),'$5'}.
record_expr -> expr_max '#' atom1 record_tuple :
	{record,?line('$2'),'$1',element(3, '$3'),'$4'}.

record_tuple -> '{' '}' : [].
record_tuple -> '{' record_fields '}' : '$2'.

record_fields -> record_field : ['$1'].
record_fields -> record_field ',' record_fields : ['$1' | '$3'].

record_field -> var '=' expr : {record_field,?line('$1'),'$1','$3'}.
record_field -> atom1 '=' expr : {record_field,?line('$1'),'$1','$3'}.

%% N.B. This is called from expr_700.

function_call -> expr_800 argument_list :
	{call,?line('$1'),'$1',element(1, '$2')}.


if_expr -> 'if' if_clauses 'end' : {'if',?line('$1'),'$2'}.

if_clauses -> if_clause : ['$1'].
if_clauses -> if_clause ';' if_clauses : ['$1' | '$3'].

if_clause -> guard clause_body :
	{clause,?line(hd(hd('$1'))),[],'$1','$2'}.


case_expr -> 'case' expr 'of' cr_clauses 'end' :
	{'case',?line('$1'),'$2','$4'}.

cr_clauses -> cr_clause : ['$1'].
cr_clauses -> cr_clause ';' cr_clauses : ['$1' | '$3'].

cr_clause -> expr clause_guard clause_body :
	{clause,?line('$1'),['$1'],'$2','$3'}.

receive_expr -> 'receive' cr_clauses 'end' :
	{'receive',?line('$1'),'$2'}.
receive_expr -> 'receive' 'after' expr clause_body 'end' :
	{'receive',?line('$1'),[],'$3','$4'}.
receive_expr -> 'receive' cr_clauses 'after' expr clause_body 'end' :
	{'receive',?line('$1'),'$2','$4','$5'}.


fun_expr -> 'fun' atom1 '/' integer :
	{'fun',?line('$1'),{function,element(3, '$2'),element(3, '$4')}}.
fun_expr -> 'fun' atom1 ':' atom1 '/' integer :
	{'fun',?line('$1'),{function,element(3, '$2'),element(3, '$4'),element(3,'$6')}}.
fun_expr -> 'fun' fun_clauses 'end' :
	build_fun(?line('$1'), '$2').

fun_clauses -> fun_clause : ['$1'].
fun_clauses -> fun_clause ';' fun_clauses : ['$1' | '$3'].

fun_clause -> argument_list clause_guard clause_body :
	{Args,Pos} = '$1',
	{clause,Pos,'fun',Args,'$2','$3'}.

try_expr -> 'try' exprs 'of' cr_clauses try_catch :
	build_try(?line('$1'),'$2','$4','$5').
try_expr -> 'try' exprs try_catch :
	build_try(?line('$1'),'$2',[],'$3').

try_catch -> 'catch' try_clauses 'end' :
	{'$2',[]}.
try_catch -> 'catch' try_clauses 'after' exprs 'end' :
	{'$2','$4'}.
try_catch -> 'after' exprs 'end' :
	{[],'$2'}.

try_clauses -> try_clause : ['$1'].
try_clauses -> try_clause ';' try_clauses : ['$1' | '$3'].

try_clause -> expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,[{atom,L,throw},'$1',{var,L,'_'}]}],'$2','$3'}.
try_clause -> atom1 ':' expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.
try_clause -> var ':' expr clause_guard clause_body :
	L = ?line('$1'),
	{clause,L,[{tuple,L,['$1','$3',{var,L,'_'}]}],'$4','$5'}.

%%cond_expr -> 'cond' cond_clauses 'end' : {'cond',?line('$1'),'$2'}.

%%cond_clauses -> cond_clause : ['$1'].
%%cond_clauses -> cond_clause ';' cond_clauses : ['$1' | '$3'].

%%cond_clause -> expr clause_body :
%%	{clause,?line('$1'),[],[['$1']],'$2'}.

query_expr -> 'query' list_comprehension 'end' :
	{'query',?line('$1'),'$2'}.


argument_list -> '(' ')' : {[],?line('$1')}.
argument_list -> '(' exprs ')' : {'$2',?line('$1')}.


exprs -> expr : ['$1'].
exprs -> expr ',' exprs : ['$1' | '$3'].

guard -> exprs : ['$1'].
guard -> exprs ';' guard : ['$1'|'$3'].

atomic -> char : '$1'.
atomic -> integer : '$1'.
atomic -> float : '$1'.
atomic -> atom1 : '$1'.
atomic -> strings : '$1'.

strings -> string : '$1'.
strings -> string strings :
	{string,?line('$1'),element(3, '$1') ++ element(3, '$2')}.

prefix_op -> '+' : '$1'.
prefix_op -> '-' : '$1'.
prefix_op -> 'bnot' : '$1'.
prefix_op -> 'not' : '$1'.

mult_op -> '/' : '$1'.
mult_op -> '*' : '$1'.
mult_op -> 'div' : '$1'.
mult_op -> 'rem' : '$1'.
mult_op -> 'band' : '$1'.
mult_op -> 'and' : '$1'.

add_op -> '+' : '$1'.
add_op -> '-' : '$1'.
add_op -> 'bor' : '$1'.
add_op -> 'bxor' : '$1'.
add_op -> 'bsl' : '$1'.
add_op -> 'bsr' : '$1'.
add_op -> 'or' : '$1'.
add_op -> 'xor' : '$1'.

list_op -> '++' : '$1'.
list_op -> '--' : '$1'.

comp_op -> '==' : '$1'.
comp_op -> '/=' : '$1'.
comp_op -> '=<' : '$1'.
comp_op -> '<' : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '>' : '$1'.
comp_op -> '=:=' : '$1'.
comp_op -> '=/=' : '$1'.

rule -> rule_clauses : build_rule('$1').

rule_clauses -> rule_clause : ['$1'].
rule_clauses -> rule_clause ';' rule_clauses : ['$1'|'$3'].

rule_clause -> atom1 clause_args clause_guard rule_body :
	{clause,?line('$1'),element(3, '$1'),'$2','$3','$4'}.

rule_body -> ':-' lc_exprs: '$2'.


Erlang code.

-export([parse_form/1,parse_exprs/1,parse_term/1]).
-export([normalise/1,abstract/1,tokens/1,tokens/2]).
-export([abstract/2, package_segments/1]).
-export([inop_prec/1,preop_prec/1,func_prec/0,max_prec/0]).
-export([set_line/2,get_attribute/2,get_attributes/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).


%% mkop(Op, Arg) -> {op,Line,Op,Arg}.
%% mkop(Left, Op, Right) -> {op,Line,Op,Left,Right}.

-define(mkop2(L, OpPos, R), 
        begin 
            {Op,Pos} = OpPos,
            {op,Pos,Op,L,R}
        end).

-define(mkop1(OpPos, A),
        begin
            {Op,Pos} = OpPos,
            {op,Pos,Op,A}
        end).

%% keep track of line info in tokens
-define(line(Tup), element(2, Tup)).

%% Entry points compatible to old erl_parse.
%% These really suck and are only here until Calle gets multiple
%% entry points working.

parse_form(Tokens) ->
    parse(Tokens).

parse_exprs(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],Exprs}]}} ->
	    {ok,Exprs};
	{error,_} = Err -> Err
    end.

parse_term(Tokens) ->
    case parse([{atom,0,f},{'(',0},{')',0},{'->',0}|Tokens]) of
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[Expr]}]}} ->
	    try normalise(Expr) of
		Term -> {ok,Term}
	    catch
		_:_R -> {error,{?line(Expr),?MODULE,"bad term"}}
	    end;
	{ok,{function,_Lf,f,0,[{clause,_Lc,[],[],[_E1,E2|_Es]}]}} ->
	    {error,{?line(E2),?MODULE,"bad term"}};
	{error,_} = Err -> Err
    end.

-type attributes() :: 'export' | 'file' | 'import' | 'module'
		    | 'opaque' | 'record' | 'type'.

build_typed_attribute({atom,La,record}, 
		      {typed_record, {atom,_Ln,RecordName}, RecTuple}) ->
    {attribute,La,record,{RecordName,record_tuple(RecTuple)}};
build_typed_attribute({atom,La,Attr},
                      {type_def, {call,_,{atom,_,TypeName},Args}, Type})
  when Attr =:= 'type' ; Attr =:= 'opaque' ->
    case lists:all(fun({var, _, _}) -> true;
                      (_)           -> false
                   end, Args) of
        true -> {attribute,La,Attr,{TypeName,Type,Args}};
        false -> error_bad_decl(La, Attr)
    end;
build_typed_attribute({atom,La,Attr},_) ->
    case Attr of
        record -> error_bad_decl(La, record);
        type   -> error_bad_decl(La, type);
	opaque -> error_bad_decl(La, opaque);
        _      -> ret_err(La, "bad attribute")
    end.

build_type_spec({spec,La}, {SpecFun, TypeSpecs}) ->
    NewSpecFun =
	case SpecFun of
	    {atom, _, Fun} -> 
		{Fun, find_arity_from_specs(TypeSpecs)};
	    {{atom,_, Mod}, {atom,_, Fun}} ->
		{Mod,Fun,find_arity_from_specs(TypeSpecs)};
	    {{atom, _, Fun}, {integer, _, Arity}} ->
		%% Old style spec. Allow this for now.
		{Fun,Arity};
	    {{atom,_, Mod}, {atom, _, Fun}, {integer, _, Arity}} ->
		%% Old style spec. Allow this for now.
		{Mod,Fun,Arity}
	    end,
    {attribute,La,spec,{NewSpecFun, TypeSpecs}}.

find_arity_from_specs([Spec|_]) ->
    %% Use the first spec to find the arity. If all are not the same,
    %% erl_lint will find this.
    Fun = case Spec of
	      {type, _, bounded_fun, [F, _]} -> F;
	      {type, _, 'fun', _} = F -> F
	  end,
    {type, _, 'fun', [{type, _, product, Args},_]} = Fun,
    length(Args).

lift_unions(T1, {type, _La, union, List}) ->
    {type, ?line(T1), union, [T1|List]};
lift_unions(T1, T2) ->
    {type, ?line(T1), union, [T1, T2]}.

build_gen_type({atom, La, tuple}) ->
    {type, La, tuple, any};
build_gen_type({atom, La, Name}) ->
    {type, La, Name, []}.

build_bin_type([{var, _, '_'}|Left], Int) ->
    build_bin_type(Left, Int);
build_bin_type([], Int) ->
    Int;
build_bin_type([{var, La, _}|_], _) ->
    ret_err(La, "Bad binary type").

%% build_attribute(AttrName, AttrValue) ->
%%	{attribute,Line,module,Module}
%%	{attribute,Line,export,Exports}
%%	{attribute,Line,import,Imports}
%%	{attribute,Line,record,{Name,Inits}}
%%	{attribute,Line,file,{Name,Line}}
%%	{attribute,Line,Name,Val}

build_attribute({atom,La,module}, Val) ->
    case Val of
	[{atom,_Lm,Module}] ->
	    {attribute,La,module,Module};
	[{atom,_Lm,Module},ExpList] ->
	    {attribute,La,module,{Module,var_list(ExpList)}};
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,Module}
	    end;
	[Name,ExpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, module);
		Module ->
		    {attribute,La,module,{Module,var_list(ExpList)}}
	    end;
	_Other ->
	    error_bad_decl(La, module)
    end;
build_attribute({atom,La,export}, Val) ->
    case Val of
	[ExpList] ->
	    {attribute,La,export,farity_list(ExpList)};
	_Other -> error_bad_decl(La, export)
    end;
build_attribute({atom,La,import}, Val) ->
    case Val of
	[Name] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,Module}
	    end;
	[{atom,_Lm,Mod},ImpList] ->
	    {attribute,La,import,{Mod,farity_list(ImpList)}};
	[Name, ImpList] ->
	    case package_segments(Name) of
		error ->
		    error_bad_decl(La, import);
		Module ->
		    {attribute,La,import,{Module,farity_list(ImpList)}}
	    end;
	_Other -> error_bad_decl(La, import)
    end;
build_attribute({atom,La,record}, Val) ->
    case Val of
	[{atom,_Ln,Record},RecTuple] ->
	    {attribute,La,record,{Record,record_tuple(RecTuple)}};
	_Other -> error_bad_decl(La, record)
    end;
build_attribute({atom,La,file}, Val) ->
    case Val of
	[{string,_Ln,Name},{integer,_Ll,Line}] ->
	    {attribute,La,file,{Name,Line}};
	_Other -> error_bad_decl(La, file)
    end;
build_attribute({atom,La,Attr}, Val) ->
    case Val of
	[Expr0] ->
	    Expr = attribute_farity(Expr0),
	    {attribute,La,Attr,term(Expr)};
	_Other -> ret_err(La, "bad attribute")
    end.

var_list({cons,_Lc,{var,_,V},Tail}) ->
    [V|var_list(Tail)];
var_list({nil,_Ln}) -> [];
var_list(Other) ->
    ret_err(?line(Other), "bad variable list").

attribute_farity({cons,L,H,T}) ->
    {cons,L,attribute_farity(H),attribute_farity(T)};
attribute_farity({tuple,L,Args0}) ->
    Args = attribute_farity_list(Args0),
    {tuple,L,Args};
attribute_farity({op,L,'/',{atom,_,_}=Name,{integer,_,_}=Arity}) ->
    {tuple,L,[Name,Arity]};
attribute_farity(Other) -> Other.

attribute_farity_list(Args) ->
    [attribute_farity(A) || A <- Args].
    
-spec error_bad_decl(integer(), attributes()) -> no_return().

error_bad_decl(L, S) ->
    ret_err(L, io_lib:format("bad ~w declaration", [S])).

farity_list({cons,_Lc,{op,_Lo,'/',{atom,_La,A},{integer,_Li,I}},Tail}) ->
    [{A,I}|farity_list(Tail)];
farity_list({nil,_Ln}) -> [];
farity_list(Other) ->
    ret_err(?line(Other), "bad function arity").

record_tuple({tuple,_Lt,Fields}) ->
    record_fields(Fields);
record_tuple(Other) ->
    ret_err(?line(Other), "bad record declaration").

record_fields([{atom,La,A}|Fields]) ->
    [{record_field,La,{atom,La,A}}|record_fields(Fields)];
record_fields([{match,_Lm,{atom,La,A},Expr}|Fields]) ->
    [{record_field,La,{atom,La,A},Expr}|record_fields(Fields)];
record_fields([{typed,Expr,TypeInfo}|Fields]) ->
    [Field] = record_fields([Expr]),
    TypeInfo1 = 
	case Expr of
	    {match, _, _, _} -> TypeInfo; %% If we have an initializer.
	    {atom, La, _} -> 
                lift_unions(abstract(undefined, La), TypeInfo)
	end, 
    [{typed_record_field,Field,TypeInfo1}|record_fields(Fields)];
record_fields([Other|_Fields]) ->
    ret_err(?line(Other), "bad record field");
record_fields([]) -> [].

term(Expr) ->
    try normalise(Expr)
    catch _:_R -> ret_err(?line(Expr), "bad attribute")
    end.

package_segments(Name) ->
    package_segments(Name, [], []).

package_segments({record_field, _, F1, F2}, Fs, As) ->
    package_segments(F1, [F2 | Fs], As);
package_segments({atom, _, A}, [F | Fs], As) ->
    package_segments(F, Fs, [A | As]);
package_segments({atom, _, A}, [], As) ->
    lists:reverse([A | As]);
package_segments(_, _, _) ->
    error.

%% build_function([Clause]) -> {function,Line,Name,Arity,[Clause]}

build_function(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {function,?line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_rule([Clause]) -> {rule,Line,Name,Arity,[Clause]'}

build_rule(Cs) ->
    Name = element(3, hd(Cs)),
    Arity = length(element(4, hd(Cs))),
    {rule,?line(hd(Cs)),Name,Arity,check_clauses(Cs, Name, Arity)}.

%% build_fun(Line, [Clause]) -> {'fun',Line,{clauses,[Clause]}}.

build_fun(Line, Cs) ->
    Arity = length(element(4, hd(Cs))),
    {'fun',Line,{clauses,check_clauses(Cs, 'fun', Arity)}}.

check_clauses(Cs, Name, Arity) ->
     mapl(fun ({clause,L,N,As,G,B}) when N =:= Name, length(As) =:= Arity ->
		 {clause,L,As,G,B};
	     ({clause,L,_N,_As,_G,_B}) ->
		 ret_err(L, "head mismatch") end, Cs).

build_try(L,Es,Scs,{Ccs,As}) ->
    {'try',L,Es,Scs,Ccs,As}.

ret_err(L, S) ->
    {location,Location} = get_attribute(L, location),
    return_error(Location, S).

%% mapl(F,List)
%% an alternative map which always maps from left to right
%% and makes it possible to interrupt the mapping with throw on
%% the first occurence from left as expected.
%% can be removed when the jam machine (and all other machines)
%% uses the standardized (Erlang 5.0) evaluation order (from left to right)
mapl(F, [H|T]) ->
	V = F(H),
	[V | mapl(F,T)];
mapl(_, []) ->
	[].

%% normalise(AbsTerm)
%% abstract(Term)
%%  Convert between the abstract form of a term and a term.

normalise({char,_,C}) -> C;
normalise({integer,_,I}) -> I;
normalise({float,_,F}) -> F;
normalise({atom,_,A}) -> A;
normalise({string,_,S}) -> S;
normalise({nil,_}) -> [];
normalise({bin,_,Fs}) ->
    {value, B, _} =
	eval_bits:expr_grp(Fs, [],
			   fun(E, _) ->
				   {value, normalise(E), []}
			   end, [], true),
    B;
normalise({cons,_,Head,Tail}) ->
    [normalise(Head)|normalise(Tail)];
normalise({tuple,_,Args}) ->
    list_to_tuple(normalise_list(Args));
%% Atom dot-notation, as in 'foo.bar.baz'
normalise({record_field,_,_,_}=A) ->
    case package_segments(A) of
	error -> erlang:error({badarg, A});
	As -> list_to_atom(packages:concat(As))
    end;
%% Special case for unary +/-.
normalise({op,_,'+',{char,_,I}}) -> I;
normalise({op,_,'+',{integer,_,I}}) -> I;
normalise({op,_,'+',{float,_,F}}) -> F;
normalise({op,_,'-',{char,_,I}}) -> -I;		%Weird, but compatible!
normalise({op,_,'-',{integer,_,I}}) -> -I;
normalise({op,_,'-',{float,_,F}}) -> -F;
normalise(X) -> erlang:error({badarg, X}).

normalise_list([H|T]) ->
    [normalise(H)|normalise_list(T)];
normalise_list([]) ->
    [].

abstract(T) when is_integer(T) -> {integer,0,T};
abstract(T) when is_float(T) -> {float,0,T};
abstract(T) when is_atom(T) -> {atom,0,T};
abstract([]) -> {nil,0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C|T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H|T]) ->
    {cons,0,abstract(H),abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
    {tuple,0,abstract_list(tuple_to_list(Tuple))}.

abstract_string([C|T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C|T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H|T]) ->
    [abstract(H)|abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

%%% abstract/2 keeps the line number
abstract(T, Line) when is_integer(T) -> {integer,Line,T};
abstract(T, Line) when is_float(T) -> {float,Line,T};
abstract(T, Line) when is_atom(T) -> {atom,Line,T};
abstract([], Line) -> {nil,Line};
abstract(B, Line) when is_bitstring(B) ->
    {bin, Line, [abstract_byte(Byte, Line) || Byte <- bitstring_to_list(B)]};
abstract([C|T], Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C], Line);
abstract([H|T], Line) ->
    {cons,Line,abstract(H, Line),abstract(T, Line)};
abstract(Tuple, Line) when is_tuple(Tuple) ->
    {tuple,Line,abstract_list(tuple_to_list(Tuple), Line)}.

abstract_string([C|T], String, Line) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C|String], Line);
abstract_string([], String, Line) ->
    {string, Line, lists:reverse(String)};
abstract_string(T, String, Line) ->
    not_string(String, abstract(T, Line), Line).

not_string([C|T], Result, Line) ->
    not_string(T, {cons, Line, {integer, Line, C}, Result}, Line);
not_string([], Result, _Line) ->
    Result.

abstract_list([H|T], Line) ->
    [abstract(H, Line)|abstract_list(T, Line)];
abstract_list([], _Line) ->
    [].

%% tokens(AbsTerm) -> [Token]
%% tokens(AbsTerm, More) -> [Token]
%%  Generate a list of tokens representing the abstract term.

tokens(Abs) ->
    tokens(Abs, []).

tokens({char,L,C}, More) -> [{char,L,C}|More];
tokens({integer,L,N}, More) -> [{integer,L,N}|More];
tokens({float,L,F}, More) -> [{float,L,F}|More];
tokens({atom,L,A}, More) -> [{atom,L,A}|More];
tokens({var,L,V}, More) -> [{var,L,V}|More];
tokens({string,L,S}, More) -> [{string,L,S}|More];
tokens({nil,L}, More) -> [{'[',L},{']',L}|More];
tokens({cons,L,Head,Tail}, More) ->
    [{'[',L}|tokens(Head, tokens_tail(Tail, More))];
tokens({tuple,L,[]}, More) ->
    [{'{',L},{'}',L}|More];
tokens({tuple,L,[E|Es]}, More) ->
    [{'{',L}|tokens(E, tokens_tuple(Es, ?line(E), More))].

tokens_tail({cons,L,Head,Tail}, More) ->
    [{',',L}|tokens(Head, tokens_tail(Tail, More))];
tokens_tail({nil,L}, More) ->
    [{']',L}|More];
tokens_tail(Other, More) ->
    L = ?line(Other),
    [{'|',L}|tokens(Other, [{']',L}|More])].

tokens_tuple([E|Es], Line, More) ->
    [{',',Line}|tokens(E, tokens_tuple(Es, ?line(E), More))];
tokens_tuple([], Line, More) ->
    [{'}',Line}|More].

%% Give the relative precedences of operators.

inop_prec('=') -> {150,100,100};
inop_prec('!') -> {150,100,100};
inop_prec('orelse') -> {160,150,150};
inop_prec('andalso') -> {200,160,160};
inop_prec('==') -> {300,200,300};
inop_prec('/=') -> {300,200,300};
inop_prec('=<') -> {300,200,300};
inop_prec('<') -> {300,200,300};
inop_prec('>=') -> {300,200,300};
inop_prec('>') -> {300,200,300};
inop_prec('=:=') -> {300,200,300};
inop_prec('=/=') -> {300,200,300};
inop_prec('++') -> {400,300,300};
inop_prec('--') -> {400,300,300};
inop_prec('+') -> {400,400,500};
inop_prec('-') -> {400,400,500};
inop_prec('bor') -> {400,400,500};
inop_prec('bxor') -> {400,400,500};
inop_prec('bsl') -> {400,400,500};
inop_prec('bsr') -> {400,400,500};
inop_prec('or') -> {400,400,500};
inop_prec('xor') -> {400,400,500};
inop_prec('*') -> {500,500,600};
inop_prec('/') -> {500,500,600};
inop_prec('div') -> {500,500,600};
inop_prec('rem') -> {500,500,600};
inop_prec('band') -> {500,500,600};
inop_prec('and') -> {500,500,600};
inop_prec('#') -> {800,700,800};
inop_prec(':') -> {900,800,900};
inop_prec('.') -> {900,900,1000}.

-type pre_op() :: 'catch' | '+' | '-' | 'bnot' | '#'.

-spec preop_prec(pre_op()) -> {0 | 600 | 700, 100 | 700 | 800}.

preop_prec('catch') -> {0,100};
preop_prec('+') -> {600,700};
preop_prec('-') -> {600,700};
preop_prec('bnot') -> {600,700};
preop_prec('not') -> {600,700};
preop_prec('#') -> {700,800}.

-spec func_prec() -> {800,700}.

func_prec() -> {800,700}.

-spec max_prec() -> 1000.

max_prec() -> 1000.

%%% [Experimental]. The parser just copies the attributes of the
%%% scanner tokens to the abstract format. This design decision has
%%% been hidden to some extent: use set_line() and get_attribute() to
%%% access the second element of (almost all) of the abstract format
%%% tuples. A typical use is to negate line numbers to prevent the
%%% compiler from emitting warnings and errors. The second element can
%%% (of course) be set to any value, but then these functions no
%%% longer apply. To get all present attributes as a property list
%%% get_attributes() should be used.

set_line(L, F) ->
    rfe_scan:set_attribute(line, L, F).

get_attribute(L, Name) ->
    rfe_scan:attributes_info(L, Name).

get_attributes(L) ->
    rfe_scan:attributes_info(L).
