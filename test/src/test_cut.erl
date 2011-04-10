%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is Erlando.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2011-2011 VMware, Inc.  All rights reserved.
%%

-module(test_cut).
-compile({parse_transform, cut}).
-compile({parse_transform, erlando}).

-compile(export_all).

-record(r, { f1 = false,
             f2 = wibble,
             f3 = juice }).

test_cut() ->
    F0 = foo(a, b, _, 5+6, _),
    F1 = F0(_, e),
    ok = F1(c),
    passed.

foo(a,b,c,11,e) -> ok.

test_cut_op() ->
    F = 1 + _,
    3 = F(2),
    passed.

test_cut_unary_op() ->
    F = -_,
    0 = 1 + F(1),
    passed.

test_cut_tuple() ->
    {foo, _} = {foo, F} = {foo, {bar, _}},
    {bar, qux} = F(qux),
    passed.

test_cut_record() ->
    true = #r{} =/= #r{f3 = _},
    orange = ((#r{f3 = _})(orange))#r.f3,
    {r, foo, bar, baz} = (#r{f1 = _, f3 = _, f2 = _})(foo, baz, bar),
    R = #r{},
    F = R#r{f3 = _, f2 = _},
    wobble = (F(orange, wobble))#r.f2,
    Getter = _#r.f2,
    wibble = Getter(#r{}),
    Setter = _#r{f2 = gerbil},
    gerbil = Getter(Setter(#r{})),
    Setter2 = _#r{f2 = _},
    hamster = Getter(Setter2(#r{}, hamster)),
    passed.

test_cut_binary() ->
    <<"AbA", _/binary>> = (<<65, _, 65>>)($b),
    F = <<_:_>>,
    G = F(15, _),
    <<>> = G(0),
    <<1:1/unsigned, 1:1/unsigned, 1:1/unsigned, 1:1/unsigned>> = G(4),
    passed.

test() ->
    passed = do([test_m || test_cut(),
                           test_cut_op(),
                           test_cut_unary_op(),
                           test_cut_tuple(),
                           test_cut_record(),
                           test_cut_binary()]).
