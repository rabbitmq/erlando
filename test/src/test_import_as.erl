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
%% Copyright (c) 2011-2013 VMware, Inc.  All rights reserved.
%%

-module(test_import_as).

-compile({parse_transform, import_as}).
-compile({parse_transform, cut}).

-import_as({lists, [{seq/3, ls}]}).
-import_as([{lists, [{seq/2, ls}, {sum/1, sum}]}, {lists, [{reverse/1, rev}]}]).

-export([test_import_as/0, test/0]).

test_import_as() ->
    L = rev(lists:seq(1, 10)),
    L = lists:reverse(ls(1, 10)),
    L = rev(ls(1, 10)),
    Fun1 = rev(_),
    [c, b, a] = Fun1([a, b, c]),
    Fun2 = fun rev/1,
    [b, a] = Fun2([a, b]),
    [1, 3, 5, 7, 9] = ls(1, 10, 2),
    25 = sum(rev(ls(1, 10, 2))).

test() ->
    test:test([{?MODULE, [test_import_as]}], [report, {name, ?MODULE}]).
