%% This file is a copy of erl_id_trans.erl from the R14B02 Erlang/OTP
%% distribution, with modifications to make it implement Haskell-style
%% 'do' syntax in Erlang.

%% All modifications are (C) 2011-2013 VMware, Inc; Eduard Sergeev.

%%
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%

-module(do).

-export([parse_transform/2, format_error/1]).

parse_transform(Forms, _Options) ->
    Forms1 = forms(Forms),
    %%io:format("~s~n", [erl_prettypr:format(erl_syntax:form_list(Forms1))]),
    Forms1.

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = try form(F0)
         catch throw:{Error, Line} ->
                 {error, {Line, ?MODULE, Error}}
         end,
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

%% -type form(Form) -> Form.

form({attribute,Line,Attr,Val}) ->      %The general attribute.
    {attribute,Line,Attr,Val};
form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
% Mnemosyne, ignore...
form({rule,Line,Name,Arity,Body}) ->
    {rule,Line,Name,Arity,Body}; % Dont dig into this
%% Extra forms from the parser.
form({error,E}) -> {error,E};
form({warning,W}) -> {warning,W};
form({eof,Line}) -> {eof,Line}.

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name, Arity, Clauses1}.

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs]) ->
    C1 = clause(C0, []),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause, Line, Head, Guard, Body}, MonadStack) ->
    {clause, Line, Head, Guard, exprs(Body, MonadStack)}.

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.

pattern({var,Line,V}) -> {var,Line,V};
pattern({match,Line,L0,R0}) ->
    L1 = pattern(L0),
    R1 = pattern(R0),
    {match,Line,L1,R1};
pattern({integer,Line,I}) -> {integer,Line,I};
pattern({char,Line,C}) -> {char,Line,C};
pattern({float,Line,F}) -> {float,Line,F};
pattern({atom,Line,A}) -> {atom,Line,A};
pattern({string,Line,S}) -> {string,Line,S};
pattern({nil,Line}) -> {nil,Line};
pattern({cons,Line,H0,T0}) ->
    H1 = pattern(H0),
    T1 = pattern(T0),
    {cons,Line,H1,T1};
pattern({tuple,Line,Ps0}) ->
    Ps1 = pattern_list(Ps0),
    {tuple,Line,Ps1};
%%pattern({struct,Line,Tag,Ps0}) ->
%%    Ps1 = pattern_list(Ps0),
%%    {struct,Line,Tag,Ps1};
pattern({record,Line,Name,Pfs0}) ->
    Pfs1 = pattern_fields(Pfs0),
    {record,Line,Name,Pfs1};
pattern({record_index,Line,Name,Field0}) ->
    Field1 = pattern(Field0),
    {record_index,Line,Name,Field1};
%% record_field occurs in query expressions
pattern({record_field,Line,Rec0,Name,Field0}) ->
    Rec1 = expr(Rec0, []),
    Field1 = expr(Field0, []),
    {record_field,Line,Rec1,Name,Field1};
pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0, []),
    Field1 = expr(Field0, []),
    {record_field,Line,Rec1,Field1};
pattern({bin,Line,Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin,Line,Fs2};
pattern({op,Line,Op,A}) ->
    {op,Line,Op,A};
pattern({op,Line,Op,L,R}) ->
    {op,Line,Op,L,R}.

pattern_grp([{bin_element,L1,E1,S1,T1} | Fs]) ->
    S2 = case S1 of
             default ->
                 default;
             _ ->
                 expr(S1, [])
         end,
    T2 = case T1 of
             default ->
                 default;
             _ ->
                 bit_types(T1)
         end,
    [{bin_element,L1,expr(E1, []),S2,T2} | pattern_grp(Fs)];
pattern_grp([]) ->
    [].

bit_types([]) ->
    [];
bit_types([Atom | Rest]) when is_atom(Atom) ->
    [Atom | bit_types(Rest)];
bit_types([{Atom, Integer} | Rest])
  when is_atom(Atom) andalso is_integer(Integer) ->
    [{Atom, Integer} | bit_types(Rest)].


%% -type pattern_list([Pattern]) -> [Pattern].
%%  These patterns are processed "in parallel" for purposes of variable
%%  definition etc.

pattern_list([P0|Ps]) ->
    P1 = pattern(P0),
    [P1|pattern_list(Ps)];
pattern_list([]) -> [].

%% -type pattern_fields([Field]) -> [Field].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

pattern_fields([{record_field,Lf,{atom,La,F},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{atom,La,F},P1}|pattern_fields(Pfs)];
pattern_fields([{record_field,Lf,{var,La,'_'},P0}|Pfs]) ->
    P1 = pattern(P0),
    [{record_field,Lf,{var,La,'_'},P1}|pattern_fields(Pfs)];
pattern_fields([]) -> [].

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es], MonadStack) ->
    E1 = expr(E0, MonadStack),
    [E1|exprs(Es, MonadStack)];
exprs([], _MonadStack) -> [].

%% -type expr(Expression) -> Expression.

expr({var, Line, V}, _MonadStack)     -> {var, Line, V};
expr({integer, Line, I}, _MonadStack) -> {integer, Line, I};
expr({float, Line, F}, _MonadStack)   -> {float, Line, F};
expr({atom, Line, A}, _MonadStack)    -> {atom, Line, A};
expr({string, Line, S}, _MonadStack)  -> {string, Line, S};
expr({char, Line, C}, _MonadStack)    -> {char, Line, C};
expr({nil, Line}, _MonadStack)        -> {nil, Line};
expr({cons, Line, H0, T0}, MonadStack) ->
    H1 = expr(H0, MonadStack),
    T1 = expr(T0, MonadStack), %% They see the same variables
    {cons, Line, H1, T1};
expr({lc, Line, E0, Qs0}, MonadStack) ->
    Qs1 = lc_bc_quals(Qs0, MonadStack),
    E1 = expr(E0, MonadStack),
    {lc, Line, E1, Qs1};
expr({bc, Line, E0, Qs0}, MonadStack) ->
    Qs1 = lc_bc_quals(Qs0, MonadStack),
    E1 = expr(E0, MonadStack),
    {bc, Line, E1, Qs1};
expr({tuple, Line, Es0}, MonadStack) ->
    Es1 = expr_list(Es0, MonadStack),
    {tuple, Line, Es1};
expr({record_index, Line, Name, Field0}, MonadStack) ->
    Field1 = expr(Field0, MonadStack),
    {record_index, Line, Name, Field1};
expr({record, Line, Name, Inits0}, MonadStack) ->
    Inits1 = record_inits(Inits0, MonadStack),
    {record, Line, Name, Inits1};
expr({record_field, Line, Rec0, Name, Field0}, MonadStack) ->
    Rec1 = expr(Rec0, MonadStack),
    Field1 = expr(Field0, MonadStack),
    {record_field, Line, Rec1, Name, Field1};
expr({record, Line, Rec0, Name, Upds0}, MonadStack) ->
    Rec1 = expr(Rec0, MonadStack),
    Upds1 = record_updates(Upds0, MonadStack),
    {record, Line, Rec1, Name, Upds1};
expr({record_field, Line, Rec0, Field0}, MonadStack) ->
    Rec1 = expr(Rec0, MonadStack),
    Field1 = expr(Field0, MonadStack),
    {record_field, Line, Rec1, Field1};
expr({block, Line, Es0}, MonadStack) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0, MonadStack),
    {block, Line, Es1};
expr({'if', Line, Cs0}, MonadStack) ->
    Cs1 = icr_clauses(Cs0, MonadStack),
    {'if', Line, Cs1};
expr({'case', Line, E0, Cs0}, MonadStack) ->
    E1 = expr(E0, MonadStack),
    Cs1 = icr_clauses(Cs0, MonadStack),
    {'case', Line, E1, Cs1};
expr({'receive', Line, Cs0}, MonadStack) ->
    Cs1 = icr_clauses(Cs0, MonadStack),
    {'receive', Line, Cs1};
expr({'receive', Line, Cs0, To0, ToEs0}, MonadStack) ->
    To1 = expr(To0, MonadStack),
    ToEs1 = exprs(ToEs0, MonadStack),
    Cs1 = icr_clauses(Cs0, MonadStack),
    {'receive', Line, Cs1, To1, ToEs1};
expr({'try', Line, Es0, Scs0, Ccs0, As0}, MonadStack) ->
    Es1 = exprs(Es0, MonadStack),
    Scs1 = icr_clauses(Scs0, MonadStack),
    Ccs1 = icr_clauses(Ccs0, MonadStack),
    As1 = exprs(As0, MonadStack),
    {'try', Line, Es1, Scs1, Ccs1, As1};
expr({'fun', Line, Body}, MonadStack) ->
    case Body of
        {clauses, Cs0} ->
            Cs1 = fun_clauses(Cs0, MonadStack),
            {'fun', Line, {clauses, Cs1}};
        {function, F, A} ->
            {'fun', Line, {function, F, A}};
        {function, M, F, A} -> %% R10B-6: fun M:F/A.
            {'fun', Line, {function, M, F, A}}
    end;
expr({named_fun, Line, Name, Cs0}, MonadStack) -> % OTP 17.0: EEP 37: Funs with names
    Cs1 = fun_clauses(Cs0, MonadStack),
    {named_fun, Line, Name, Cs1};
%%  do syntax detection:
expr({call, Line, {atom, _Line1, do},
      [{lc, _Line2, {AtomOrVar, _Line3, _MonadModule} = Monad, Qs}]},
     MonadStack) when AtomOrVar =:= atom orelse AtomOrVar =:= var ->
    %% 'do' calls of a particular form:
    %%  do([ MonadMod || Qualifiers ])
    {call, Line,
     {'fun', Line,
      {clauses,
       [{clause, Line, [], [], do_syntax(Qs, [Monad | MonadStack])}]}}, []};
%%  'return' and 'fail' syntax detection and transformation:
expr({call, Line, {atom, Line1, ReturnOrFail}, As0},
     [Monad|_Monads] = MonadStack) when ReturnOrFail =:= return orelse
                                        ReturnOrFail =:= fail->
    %% 'return' calls of a particular form:
    %%  return(Arguments), and
    %% 'fail' calls of a particular form:
    %%  fail(Arguments)
    %% Transformed to:
    %% "Monad:return(Args)" or "Monad:fail(Args)" in monadic context
    {call, Line, {remote, Line1, Monad, {atom, Line1, ReturnOrFail}},
     expr_list(As0, MonadStack)};
expr({call, Line, F0, As0}, MonadStack) ->
    %% N.B. If F an atom then call to local function or BIF,  if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0, MonadStack),
    As1 = expr_list(As0, MonadStack),
    {call, Line, F1, As1};
expr({'catch', Line, E0}, MonadStack) ->
    %% No new variables added.
    E1 = expr(E0, MonadStack),
    {'catch', Line, E1};
expr({'query',  Line,  E0}, MonadStack) ->
    %% lc expression
    E = expr(E0, MonadStack),
    {'query',  Line,  E};
expr({match, Line, P0, E0}, MonadStack) ->
    E1 = expr(E0, MonadStack),
    P1 = pattern(P0),
    {match, Line, P1, E1};
expr({bin, Line, Fs}, _MonadStack) ->
    Fs2 = pattern_grp(Fs),
    {bin, Line, Fs2};
expr({op, Line, Op, A0}, MonadStack) ->
    A1 = expr(A0, MonadStack),
    {op, Line, Op, A1};
expr({op, Line, Op, L0, R0}, MonadStack) ->
    L1 = expr(L0, MonadStack),
    R1 = expr(R0, MonadStack), %% They see the same variables
    {op, Line, Op, L1, R1};
%% The following are not allowed to occur anywhere!
expr({remote, Line, M0, F0}, MonadStack) ->
    M1 = expr(M0, MonadStack),
    F1 = expr(F0, MonadStack),
    {remote, Line, M1, F1}.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es], MonadStack) ->
    E1 = expr(E0, MonadStack),
    [E1|expr_list(Es, MonadStack)];
expr_list([], _MonadStack) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field, Lf, {atom, La, F}, Val0}|Is], MonadStack) ->
    Val1 = expr(Val0, MonadStack),
    [{record_field, Lf, {atom, La, F}, Val1}|record_inits(Is, MonadStack)];
record_inits([{record_field, Lf, {var, La, '_'}, Val0}|Is], MonadStack) ->
    Val1 = expr(Val0, MonadStack),
    [{record_field, Lf, {var, La, '_'}, Val1}|record_inits(Is, MonadStack)];
record_inits([], _MonadStack) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field, Lf, {atom, La, F}, Val0}|Us], MonadStack) ->
    Val1 = expr(Val0, MonadStack),
    [{record_field, Lf, {atom, La, F}, Val1}|record_updates(Us, MonadStack)];
record_updates([], _MonadStack) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs], MonadStack) ->
    C1 = clause(C0, MonadStack),
    [C1|icr_clauses(Cs, MonadStack)];
icr_clauses([], _MonadStack) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate, Line, P0, E0}|Qs], MonadStack) ->
    E1 = expr(E0, MonadStack),
    P1 = pattern(P0),
    [{generate, Line, P1, E1}|lc_bc_quals(Qs, MonadStack)];
lc_bc_quals([{b_generate, Line, P0, E0}|Qs], MonadStack) ->
    E1 = expr(E0, MonadStack),
    P1 = pattern(P0),
    [{b_generate, Line, P1, E1}|lc_bc_quals(Qs, MonadStack)];
lc_bc_quals([E0|Qs], MonadStack) ->
    E1 = expr(E0, MonadStack),
    [E1|lc_bc_quals(Qs, MonadStack)];
lc_bc_quals([], _MonadStack) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs], MonadStack) ->
    C1 = clause(C0, MonadStack),
    [C1|fun_clauses(Cs, MonadStack)];
fun_clauses([], _MonadStack) -> [].

%%  'do' syntax transformation:
do_syntax([], [{_AtomOrVar, MLine, _MonadModule} | _MonadStack]) ->
    transform_error("A 'do' construct cannot be empty", MLine);
do_syntax([{GenerateOrMatch, Line, _Pattern, _Expr}], _MonadStack)
  when GenerateOrMatch =:= generate orelse GenerateOrMatch =:= match ->
    transform_error("The last statement in a 'do' construct must be an expression", Line);
do_syntax([{generate, Line, {var, _Line, _Var} = Pattern, Expr} | Exprs],
          [Monad | _Monads] = MonadStack) ->
    %% "Pattern <- Expr, Tail" where Pattern is a simple variable
    %% is transformed to
    %% "Monad:'>>='(Expr, fun (Pattern) -> Tail')"
    %% without a fail to match clause
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [expr(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line, [Pattern], [], do_syntax(Exprs, MonadStack)}]}}]}];
do_syntax([{generate, Line, Pattern, Expr} | Exprs],
          [Monad | _Monads] = MonadStack) ->
    %% "Pattern <- Expr, Tail" where Pattern is not a simple variable
    %% is transformed to
    %% "Monad:'>>='(Expr, fun (Pattern) -> Tail')"
    %% with a fail clause if the function does not match
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [expr(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line, [Pattern], [], do_syntax(Exprs, MonadStack)},
          {clause, Line, [{var, Line, '_'}], [],
           [{call, Line, {remote, Line, Monad, {atom, Line, 'fail'}},
             [{atom, Line, 'monad_badmatch'}]}]}]}}]}];
do_syntax([Expr], MonadStack) ->
    [expr(Expr, MonadStack)]; %% Don't do '>>' chaining on the last elem
do_syntax([{match, _Line, _Pattern, _Expr} = Expr | Exprs],
          MonadStack) ->
    %% Handles 'let binding' in do expression a-la Haskell
    [expr(Expr, MonadStack) | do_syntax(Exprs, MonadStack)];
do_syntax([Expr | Exprs], [Monad | _Monads] = MonadStack) ->
    %% "Expr, Tail" is transformed to "Monad:'>>='(Expr, fun (_) -> Tail')"
    %% Line is always the 2nd element of Expr
    Line = element(2, Expr),
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}},
      [expr(Expr, MonadStack),
       {'fun', Line,
        {clauses,
         [{clause, Line,
           [{var, Line, '_'}], [], do_syntax(Exprs, MonadStack)}]}}]}].

%% Use this function to report any parse_transform error. The
%% resulting error message will be displayed as an ordinary
%% compilation error in a standard format.
transform_error(Message, Line) ->
    throw({Message, Line}).

%% This function is called by the Erlang compiler to obtain an error
%% message which will be shown to the user.
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.
