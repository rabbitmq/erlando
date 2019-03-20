%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at https://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(test_cut).
-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-compile(export_all).

-record(r, { f1 = false,
             f2 = wibble,
             f3 = juice }).

test_cut() ->
    F0 = foo(a, b, _, 5+6, _),
    F1 = F0(_, e),
    ok = F1(c),
    F2 = _(1,2),
    3 = F2(fun erlang:'+'/2),
    -1 = F2(fun erlang:'-'/2),
    F3 = _:foo(a, b, c, _, e),
    ok = F3(?MODULE, 11),
    F4 = _:_(_), %% this isn't getting silly at all...
    true = 3 == F4(math, sqrt, 9).

foo(a, b, c, 11, e) -> ok.

test_cut_nested() ->
    F = f1(1, f2(1 + _), _),
    %% should be:
    %% F = \X -> f1(1, f2(\Y -> 1 + Y), X)
    ok = F(3).

f1(N, M, L) when N + M =:= L -> ok.
f2(Nf) -> Nf(1).

test_cut_op() ->
    F = 1 + _,
    3 = F(2).

test_cut_unary_op() ->
    F = -_,
    0 = 1 + F(1).

test_cut_tuple() ->
    {foo, _} = {foo, F} = {foo, {bar, _}},
    {bar, qux} = F(qux).

test_cut_record() ->
    true = #r{} =/= #r{f3 = _},
    orange = ((#r{f3 = _})(orange))#r.f3,
    {r, foo, bar, baz} = (#r{f1 = _, f3 = _, f2 = _})(foo, baz, bar),
    R = #r{},
    F = R#r{f3 = _, f2 = _},
    wobble = (F(orange, wobble))#r.f2,
    Getter = _#r.f2,
    wibble = Getter(R),
    Setter = _#r{f2 = gerbil},
    gerbil = Getter(Setter(R)),
    Setter2 = _#r{f2 = _},
    hamster = Getter(Setter2(R, hamster)).

test_cut_record_nested() ->
    F = #r{f1 = #r{f1 = _, f3 = _}, f2 = _},
    R = F(apple),
    F1 = R#r.f1,
    #r{f1 = orange, f3 = banana} = F1(orange, banana).

test_cut_map() ->
    true = #{} =/= #{f3 => _},
    orange = maps:get(f3, (#{f3 => _})(orange)),
    #{f1 := foo, f2 := bar, f3 := baz}
        = (#{f1 => _, f3 => _, f2 => _})(foo, baz, bar),
    M = #{f1 => false, f2 => wibble, f3 => juice},
    F = M#{f3 => _, f2 => _},
    wobble = maps:get(f2, F(orange, wobble)),
    Getter = maps:get(f2, _),
    wibble = Getter(M),
    Setter = _#{f2 := gerbil},
    gerbil = Getter(Setter(M)),
    Setter2 = _#{f2 := _},
    hamster = Getter(Setter2(M, hamster)).

test_cut_map_nested() ->
    F = #{f1 => #{f1 => _, f3 => _}, f2 => _},
    M = F(apple),
    F1 = maps:get(f1, M),
    #{f1 := orange, f3 := banana} = F1(orange, banana).

test_cut_binary() ->
    <<"AbA", _/binary>> = (<<65, _, 65>>)($b),
    F = <<_:_>>,
    G = F(15, _),
    <<>> = G(0),
    <<1:1/unsigned, 1:1/unsigned, 1:1/unsigned, 1:1/unsigned>> = G(4).

test_cut_list() ->
    F = [_|_],
    [a,b] = F(a,[b]),
    G = [_, _ | [33]],
    [a,b,33] = G(a,b),

    H = [1, _, _, [_], 5 | [6, [_] | [_]]],
    %% This is the same as:
    %%  [1, _, _, [_], 5, 6, [_], _]
    %% And thus becomes
    %% \A, B, C -> [1, A, B, \D -> [D], 5, 6, \E -> [E], C]
    [1, 2, 3, H1, 5, 6, H2, 8] = H(2, 3, 8),
    [4] = H1(4),
    [7] = H2(7),

    I = [_, [_]],
    [a, I1] = I(a),
    [b] = I1(b).

test_cut_case() ->
    F = case _ of
            N when is_integer(N) andalso 0 =:= (N rem 2) ->
                even;
            N when is_integer(N) ->
                odd;
            _ ->
                not_a_number
        end,
    even = F(1234),
    odd = F(6789),
    not_a_number = F(my_atom).

test_cut_comprehensions() ->
    F = << <<(1 + (X*2))>> || _ <- _, X <- _ >>, %% Note, this'll only be a /2 !
    <<"AAA">> = F([a,b,c], [32]),
    F1 = [ {X, Y, Z} || X <- _, Y <- _, Z <- _,
                        math:pow(X,2) + math:pow(Y,2) == math:pow(Z,2) ],
    [{3,4,5}, {4,3,5}, {6,8,10}, {8,6,10}] =
        lists:usort(F1(lists:seq(1,10), lists:seq(1,10), lists:seq(1,10))).

test_cut_named_fun() ->
    Add  = _ + _,
    Fib  = fun Self (0) -> 1;
               Self (1) -> 1;
               Self (N) -> (Add(_, _))(N, Self(N-1))
           end,
    true = (Fib(_))(10) =:= 55.

test() ->
    test:test([{?MODULE, [test_cut,
                          test_cut_nested,
                          test_cut_op,
                          test_cut_unary_op,
                          test_cut_tuple,
                          test_cut_map,
                          test_cut_map_nested,
                          test_cut_record,
                          test_cut_record_nested,
                          test_cut_binary,
                          test_cut_list,
                          test_cut_case,
                          test_cut_comprehensions,
                          test_cut_named_fun]}],
              [report, {name, ?MODULE}]).
