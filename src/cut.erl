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
    %%io:format("Before:~n~p~n~n", [Forms]),
    put(var_count, 0),
    Forms1 = forms(Forms),
    %% io:format("After:~n~s~n~n",
    %%          [erl_prettypr:format(erl_syntax:form_list(Forms1))]),
    Forms1.

%% forms(Fs) -> lists:map(fun (F) -> form(F) end, Fs).

forms([F0|Fs0]) ->
    F1 = form(F0),
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
    C1 = clause(C0),
    [C1|clauses(Cs)];
clauses([]) -> [].

%% -type clause(Clause) -> Clause.

clause({clause, Line, Head, Guard, Body}) ->
    {clause, Line, Head, Guard, exprs(Body)}.

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
bit_types([{Atom, Integer} | Rest]) when is_atom(Atom), is_integer(Integer) ->
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

exprs([E0|Es]) ->
    E1 = expr(E0),
    [E1|exprs(Es)];
exprs([]) -> [].

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
    case find_cut_vars([H1, T1]) of
        {[], _H2T2} ->
            {cons, Line, H1, T1};
        {Pattern, [H2, T2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{cons, Line, H2, T2}]}]}}
    end;
expr({lc, Line, E0, Qs0}) ->
    %% Note that it is nonsensical to allow a cut on E0, as in all
    %% useful cases, it is defined by some expression of Qs0. Cuts are
    %% allowed only on generators of Qs0.
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    Qs = find_comprehension_cut_vars(Qs1),
    case Qs of
        {[], _Qs2} ->
            {lc, Line, E1, Qs1};
        {Pattern, Qs2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{lc, Line, E1, Qs2}]}]}}
    end;
expr({bc, Line, E0, Qs0}) ->
    %% Notes for {lc,...} above apply here too.
    Qs1 = lc_bc_quals(Qs0),
    E1 = expr(E0),
    Qs = find_comprehension_cut_vars(Qs1),
    case Qs of
        {[], _Qs2} ->
            {bc, Line, E1, Qs1};
        {Pattern, Qs2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{bc, Line, E1, Qs2}]}]}}
    end;
expr({tuple, Line, Es0}) ->
    Es1 = expr_list(Es0),
    case find_cut_vars(Es1) of
        {[],     _Es2} ->
            {tuple, Line, Es1};
        {Pattern, Es2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{tuple, Line, Es2}]}]}}
    end;
%%expr({struct,Line,Tag,Es0}) ->
%%    Es1 = pattern_list(Es0),
%%    {struct,Line,Tag,Es1};
expr({record_index, Line, Name, Field0}) ->
    %% The parser prevents Field0 from being a genuine expression, so
    %% can't do a cut here.
    Field1 = expr(Field0),
    {record_index, Line, Name, Field1};
expr({record, Line, Name, Inits0}) ->
    Inits1 = record_inits(Inits0),
    case find_record_cut_vars(Inits1) of
        {[],     _Inits2} ->
            {record, Line, Name, Inits1};
        {Pattern, Inits2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{record, Line, Name, Inits2}]}]}}
    end;
expr({record_field, Line, Rec0, Name, Field0}) ->
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    case find_cut_vars([Rec1]) of
        {[], _Rec2} ->
            {record_field, Line, Rec1, Name, Field1};
        {Pattern, [Rec2]} ->
            {'fun', Line, {clauses,
                           [{clause, Line, Pattern, [],
                             [{record_field, Line, Rec2, Name, Field1}]}]}}
    end;
expr({record, Line, Rec0, Name, Upds0}) ->
    Rec1 = expr(Rec0),
    Upds1 = record_updates(Upds0),
    Rec = find_cut_vars([Rec1]),
    Upds = find_record_cut_vars(Upds1),
    case {Rec, Upds} of
        {{[], _Rec2}, {[], _Upds2}} ->
            {record, Line, Rec1, Name, Upds1};
        {{Pattern1, [Rec2]}, {Pattern2, Upds2}} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern1++Pattern2, [],
                                      [{record, Line, Rec2, Name, Upds2}]}]}}
    end;
expr({record_field, Line, Rec0, Field0}) ->
    %% This only occurs within an mnesia query, let's not cut here
    Rec1 = expr(Rec0),
    Field1 = expr(Field0),
    {record_field, Line, Rec1, Field1};
expr({block, Line, Es0}) ->
    %% Unfold block into a sequence.
    %% Nonsensical to allow cuts here.
    Es1 = exprs(Es0),
    {block, Line, Es1};
expr({'if', Line, Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'if', Line, Cs1};
expr({'case', Line, E0, Cs0}) ->
    E1 = expr(E0),
    Cs1 = icr_clauses(Cs0),
    case find_cut_vars([E1]) of
        {[], _E2} ->
            {'case', Line, E1, Cs1};
        {Pattern, [E2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{'case', Line, E2, Cs1}]}]}}
    end;
expr({'receive', Line, Cs0}) ->
    Cs1 = icr_clauses(Cs0),
    {'receive', Line, Cs1};
expr({'receive', Line, Cs0, To0, ToEs0}) ->
    To1 = expr(To0),
    ToEs1 = exprs(ToEs0),
    Cs1 = icr_clauses(Cs0),
    {'receive', Line, Cs1, To1, ToEs1};
expr({'try', Line, Es0, Scs0, Ccs0, As0}) ->
    %% It doesn't make sense to allow a cut on Es0 (the main
    %% expression) because it would have to be evaluated as an arg to
    %% the function, and thus would never be caught.  Further, it
    %% doesn't even make sense to allow cuts in the after, because the
    %% only reason for using an after is for being able to side-effect
    %% in there, and again, it would have to be evaluated as an arg to
    %% the function. So no cuts at all allowed in try.
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
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
    %%
    %% If F0 is a remote call then we want to allow cuts, but we don't
    %% want F0 to end up forming a separate function. Thus we have
    %% find_call_cut_vars and we brings cuts from within that up here.
    F1 = expr(F0),
    As1 = expr_list(As0),
    F = find_call_cut_vars(F1),
    As = find_cut_vars(As1),
    case {F, As} of
        {{[], _F2}, {[], _As2}} ->
            {call, Line, F1, As1};
        {{Pattern1, [F2]}, {Pattern2, As2}} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern1++Pattern2, [],
                                      [{call, Line, F2, As2}]}]}}
    end;
expr({'catch', Line, E0}) ->
    %% No new variables added.
    %% See 'try' above for reasoning around no cuts here.
    E1 = expr(E0),
    {'catch', Line, E1};
expr({'query', Line, E0}) ->
    %% lc expression
    E1 = expr(E0),
    {'query', Line, E1};
expr({match, Line, P0, E0}) ->
    E1 = expr(E0),
    P1 = pattern(P0),
    {match, Line, P1, E1};
expr({bin, Line, Fs}) ->
    Fs1 = pattern_grp(Fs),
    case find_binary_cut_vars(Fs1) of
        {[], _Fs2} ->
            {bin, Line, Fs1};
        {Pattern, Fs2} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{bin, Line, Fs2}]}]}}
    end;
expr({op, Line, Op, A0}) ->
    A1 = expr(A0),
    case find_cut_vars([A1]) of
        {[], _A2} ->
            {op, Line, Op, A1};
        {Pattern, [A2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{op, Line, Op, A2}]}]}}
    end;
expr({op, Line, Op, L0, R0}) ->
    L1 = expr(L0),
    R1 = expr(R0), %% They see the same variables
    case find_cut_vars([L1, R1]) of
        {[], _L2R2} ->
            {op, Line, Op, L1, R1};
        {Pattern, [L2, R2]} ->
            {'fun', Line, {clauses, [{clause, Line, Pattern, [],
                                      [{op, Line, Op, L2, R2}]}]}}
    end;
%% The following are not allowed to occur anywhere!
expr({remote, Line, M0, F0}) ->
    %% see {call,...} for why cuts aren't here.
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

%% Turns out you can't abstract out binary types:
%% 1> X = binary, Y = fun (Z) -> <<Z/X>> end.
%% * 1: syntax error before: X
%% I didn't know that. I still support that in cuts though you can't
%% use it on the grounds that Erlang might fix this at some later
%% point.
find_binary_cut_vars(BinFields) ->
    cut_vars(
      fun ({bin_element, _Line, Var, Size, Type}) ->
              [V || V = {var, _Line1, '_'} <- [Var, Size, Type]];
          (_) ->
              []
      end,
      fun ({bin_element, Line, Var, Size, Type}, Vars) ->
              {[Var1, Size1, Type1], []} =
                  lists:foldr(
                    fun ({var, _Line, '_'}, {Res, [V|Vs]}) -> {[V|Res], Vs};
                        (V,                 {Res, Vs})     -> {[V|Res], Vs}
                    end, {[], Vars}, [Var, Size, Type]),
              {bin_element, Line, Var1, Size1, Type1}
      end,
      BinFields).

find_record_cut_vars(RecFields) ->
    cut_vars(
      fun ({record_field, _Line, _FName, {var, _Line1, '_'} = Var}) -> [Var];
          (_)                                                       -> []
      end,
      fun ({record_field, Line, FName, _Var}, [Var]) ->
              {record_field, Line, FName, Var}
      end,
      RecFields).

find_comprehension_cut_vars(Qs) ->
    cut_vars(
      fun ({generate,   _Line, _P0, {var, _Line1, '_'} = Var}) -> [Var];
          ({generate,   _Line, _P0, _E0})                      -> [];
          ({b_generate, _Line, _P0, {var, _Line1, '_'} = Var}) -> [Var];
          ({b_generate, _Line, _P0, _E0})                      -> [];
          (_)                                                  -> []
      end,
      fun ({generate,   Line, P0, _Var}, [Var]) -> {generate, Line, P0, Var};
          ({b_generate, Line, P0, _Var}, [Var]) -> {b_generate, Line, P0, Var}
      end,
      Qs).

find_call_cut_vars(F) ->
    cut_vars(
      fun ({remote, _Line, M0, F0}) -> [V || V = {var, _Line1, '_'} <- [M0,F0]];
          ({var, _Line, '_'} = Var) -> [Var];
          (_)                       -> []
      end,
      fun ({remote, Line, M0, F0}, Vars) ->
              {[M1, F1], []} =
                  lists:foldr(
                    fun ({var, _Line, '_'}, {Res, [V|Vs]}) -> {[V|Res], Vs};
                        (V,                 {Res, Vs})     -> {[V|Res], Vs}
                    end, {[], Vars}, [M0, F0]),
              {remote, Line, M1, F1};
          ({var, _Line, _Var}, [Var]) -> Var
      end,
      [F]).

find_cut_vars(As) ->
    cut_vars(fun ({var, _Line, '_'} = Var) -> [Var];
                 (_)                       -> []
             end,
             fun (_, [{var, _Line, _Var} = Var]) -> Var end,
             As).

cut_vars(TestFun, CombFun, AstFrag) ->
    cut_vars(TestFun, CombFun, AstFrag, [], []).

cut_vars(_TestFun, _CombFun, [], Pattern, AstAcc) ->
    {lists:reverse(Pattern), lists:reverse(AstAcc)};
cut_vars(TestFun, CombFun, [Frag|AstFrags], Pattern, AstAcc) ->
    case TestFun(Frag) of
        [] ->
            cut_vars(TestFun, CombFun, AstFrags, Pattern, [Frag|AstAcc]);
        Vars ->
            Vars1 = [{var, Line, make_var_name()} || {var, Line, _} <- Vars],
            Frag1 = CombFun(Frag, Vars1),
            cut_vars(TestFun, CombFun, AstFrags,
                     Vars1 ++ Pattern, [Frag1|AstAcc])
    end.

make_var_name() ->
    VarCount = get(var_count),
    put(var_count, VarCount+1),
    list_to_atom("__cut_" ++ integer_to_list(VarCount)).
