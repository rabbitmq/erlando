%% This file is a copy of erl_id_trans.erl from the R14B02 Erlang/OTP
%% distribution, with modifications to make it implement Scheme
%% Notation for Specializing Parameters without Currying
%% (http://srfi.schemers.org/srfi-26/srfi-26.html).

%% All modifications are (C) 2011-2011 VMware, Inc.

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

-module(cut).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %%io:format("~p~n", [Forms]),
    put(var_count, 0),
    Forms1 = forms(Forms),
    %%io:format("After:~n~s~n~n", [erl_prettypr:format(erl_syntax:form_list(Forms1))]),
    Forms1.

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = form(F0),
    Fs1 = forms(Fs0),
    [F1|Fs1];
forms([]) -> [].

form({function,Line,Name0,Arity0,Clauses0}) ->
    {Name,Arity,Clauses} = function(Name0, Arity0, Clauses0),
    {function,Line,Name,Arity,Clauses};
form(Other) ->
    Other.

%% -type function(atom(), integer(), [Clause]) -> {atom(),integer(),[Clause]}.

function(Name, Arity, Clauses0) ->
    Clauses1 = clauses(Clauses0),
    {Name, Arity, Clauses1}.

%% -type clauses([Clause]) -> [Clause].

clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause, Line, Head, Guard, Body}) ->
    {clause, Line, Head, Guard, exprs(Body)}.

%% -type exprs([Expression]) -> [Expression].
%%  These expressions are processed "sequentially" for purposes of variable
%%  definition etc.

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

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
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field,Line,Rec1,Name,Field1};
pattern({record_field,Line,Rec0,Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
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
                 expr(S1)
         end,
    T2 = case T1 of
             default ->
                 default;
             _ ->
                 bit_types(T1)
         end,
    [{bin_element,L1,expr(E1),S2,T2} | pattern_grp(Fs)];
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

%% -type expr(Expression) -> Expression.

expr({var, Line, V})     -> {var, Line, V};
expr({integer, Line, I}) -> {integer, Line, I};
expr({float, Line, F})   -> {float, Line, F};
expr({atom, Line, A})    -> {atom, Line, A};
expr({string, Line, S})  -> {string, Line, S};
expr({char, Line, C})    -> {char, Line, C};
expr({nil, Line})        -> {nil, Line};
expr({cons, Line, H0, T0}) ->
    H1 = expr(H0),
    T1 = expr(T0), %% They see the same variables
    {cons, Line, H1, T1};
expr({lc, Line, E0, Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {lc, Line, E1, Qs1};
expr({bc, Line, E0, Qs0}) ->
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    {bc, Line, E1, Qs1};
expr({tuple, Line, Es0}) ->
    Es1 = expr_list(Es0),
    case find_cut_vars(Es1) of
        {[],     _Es2} ->
            {tuple, Line, Es1};
        {Pattern, Es2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{tuple, Line, Es2}]}]}}
    end;
expr({record_index, Line, Name, Field0}) ->
    Field1 = expr(Field0),
    {record_index, Line, Name, Field1};
expr({record, Line, Name, Inits0}) ->
    Inits1 = record_inits(Inits0),
    case find_record_inits_cut_vars(Inits1) of
        {[],     _Inits2} ->
            {record, Line, Name, Inits1};
        {Pattern, Inits2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{record, Line, Name, Inits2}]}]}}
    end;
expr({record_field, Line, Rec0, Name, Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field, Line, Rec1, Name, Field1};
expr({record, Line, Rec0, Name, Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    {record, Line, Rec1, Name, Upds1};
expr({record_field, Line, Rec0, Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field, Line, Rec1, Field1};
expr({block, Line, Es0}) ->
    %% Unfold block into a sequence.
    Es1 = exprs(Es0),
    {block, Line, Es1};
expr({'if', Line, Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if', Line, Cs1};
expr({'case', Line, E0, Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    {'case', Line, E1, Cs1};
expr({'receive', Line, Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive', Line, Cs1};
expr({'receive', Line, Cs0, To0, ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive', Line, Cs1, To1, ToEs1};
expr({'try', Line, Es0, Scs0, Ccs0, As0}) ->
    Es1 = exprs(Es0),
    Scs1 = icr_clauses(Scs0),
    Ccs1 = icr_clauses(Ccs0),
    As1 = exprs(As0),
    {'try', Line, Es1, Scs1, Ccs1, As1};
expr({'fun', Line, Body}) ->
    case Body of
        {clauses, Cs0} ->
            Cs1 = fun_clauses(Cs0),
            {'fun', Line, {clauses, Cs1}};
        {function, F, A} ->
            {'fun', Line, {function, F, A}};
        {function, M, F, A} -> %% R10B-6: fun M:F/A.
            {'fun', Line, {function, M, F, A}}
    end;
expr({call, Line, F0, As0}) ->
    %% N.B. If F an atom then call to local function or BIF,  if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    F1 = expr(F0),
    As1 = expr_list(As0),
    case find_cut_vars(As1) of
        {[], _Vars} ->
            {call, Line, F1, As1};
        {Pattern, Vars} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{call, Line, F1, Vars}]}]}}
    end;
expr({'catch', Line, E0}) ->
    %% No new variables added.
    E1 = expr(E0),
    {'catch', Line, E1};
expr({'query',  Line,  E0}) ->
    %% lc expression
    E = expr(E0),
    {'query',  Line,  E};
expr({match, Line, P0, E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match, Line, P1, E1};
expr({bin, Line, Fs}) ->
    Fs2 = pattern_grp(Fs),
    {bin, Line, Fs2};
expr({op, Line, Op, A0}) ->
    A1 = expr(A0),
    case find_cut_vars([A1]) of
        {[], _Vars} ->
            {op, Line, Op, A1};
        {Pattern, [A2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{op, Line, Op, A2}]}]}}
    end;
expr({op, Line, Op, L0, R0}) ->
    L1 = expr(L0),
    R1 = expr(R0), %% They see the same variables
    case find_cut_vars([L1, R1]) of
        {[], _Vars} ->
            {op, Line, Op, L1, R1};
        {Pattern, [L2, R2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{op, Line, Op, L2, R2}]}]}}
    end;
%% The following are not allowed to occur anywhere!
expr({remote, Line, M0, F0}) ->
    M1 = expr(M0),
    F1 = expr(F0),
    {remote, Line, M1, F1}.

%% -type expr_list([Expression]) -> [Expression].
%%  These expressions are processed "in parallel" for purposes of variable
%%  definition etc.

expr_list([E0|Es]) ->
    E1 = expr(E0),
    [E1|expr_list(Es)];
expr_list([]) -> [].

%% -type record_inits([RecordInit]) -> [RecordInit].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_inits([{record_field, Lf, {atom, La, F}, Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field, Lf, {atom, La, F}, Val1}|record_inits(Is)];
record_inits([{record_field, Lf, {var, La, '_'}, Val0}|Is]) ->
    Val1 = expr(Val0),
    [{record_field, Lf, {var, La, '_'}, Val1}|record_inits(Is)];
record_inits([]) -> [].

%% -type record_updates([RecordUpd]) -> [RecordUpd].
%%  N.B. Field names are full expressions here but only atoms are allowed
%%  by the *linter*!.

record_updates([{record_field, Lf, {atom, La, F}, Val0}|Us]) ->
    Val1 = expr(Val0),
    [{record_field, Lf, {atom, La, F}, Val1}|record_updates(Us)];
record_updates([]) -> [].

%% -type icr_clauses([Clause]) -> [Clause].

icr_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|icr_clauses(Cs)];
icr_clauses([]) -> [].

%% -type lc_bc_quals([Qualifier]) -> [Qualifier].
%%  Allow filters to be both guard tests and general expressions.

lc_bc_quals([{generate, Line, P0, E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{generate, Line, P1, E1}|lc_bc_quals(Qs)];
lc_bc_quals([{b_generate, Line, P0, E0}|Qs]) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    [{b_generate, Line, P1, E1}|lc_bc_quals(Qs)];
lc_bc_quals([E0|Qs]) ->
    E1 = expr(E0),
    [E1|lc_bc_quals(Qs)];
lc_bc_quals([]) -> [].

%% -type fun_clauses([Clause]) -> [Clause].

fun_clauses([C0|Cs]) ->
    C1 = clause(C0),
    [C1|fun_clauses(Cs)];
fun_clauses([]) -> [].

find_record_inits_cut_vars(Inits) ->
    find_record_inits_cut_vars(Inits, [], []).

find_record_inits_cut_vars([], Pattern, InitsAcc) ->
    {lists:reverse(Pattern), lists:reverse(InitsAcc)};
find_record_inits_cut_vars(
  [{record_field, Line, FieldName, {var, Line1, '_'}}|Inits],
  Pattern, InitsAcc) ->
    VarName = make_var_name(),
    Var = {var, Line1, VarName},
    find_record_inits_cut_vars(
      Inits, [Var|Pattern], [{record_field, Line, FieldName, Var}|InitsAcc]);
find_record_inits_cut_vars([Init|Inits], Pattern, InitsAcc) ->
    find_record_inits_cut_vars(Inits, Pattern, [Init|InitsAcc]).

find_cut_vars(As) ->
    find_cut_vars(As, [], []).

find_cut_vars([], Pattern, Vars) ->
    {lists:reverse(Pattern), lists:reverse(Vars)};
find_cut_vars([{var, Line, '_'}|As], Pattern, Vars) ->
    VarName = make_var_name(),
    Var = {var, Line, VarName},
    find_cut_vars(As, [Var|Pattern], [Var|Vars]);
find_cut_vars([A|As], Pattern, Vars) ->
    find_cut_vars(As, Pattern, [A|Vars]).

make_var_name() ->
    VarCount = get(var_count),
    put(var_count, VarCount+1),
    list_to_atom("__cut_" ++ integer_to_list(VarCount)).
