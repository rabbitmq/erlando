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

-compile(export_all).

-record(r, { f1 = false,
             f2 = wibble,
             f3 = juice }).

test_cut() ->
    F0 = foo(a, b, _, 5+6, _),
    F1 = F0(_, e),
    F1(c).

foo(a,b,c,11,e) -> ok.

test_cut_op() ->
    F = 1 + _,
    F(2).

test_cut_unary_op() ->
    F = -_,
    F(1).

test_cut_tuple() ->
    {foo, _} = {foo, {bar, _}},
    {foo, F} = {foo, {baz, _}},
    {baz, qux} = F(qux).

test_cut_record() ->
    true = #r{} =/= #r{f3 = _},
    orange = ((#r{f3 = _})(orange))#r.f3,
    {r, foo, bar, baz} = (#r{f1 = _, f3 = _, f2 = _})(foo, baz, bar).
