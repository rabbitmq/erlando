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

-module(test_import_as).

-compile({parse_transform, import_as}).

-import_as([{lists, [{seq/2, ls}, {reverse/1, rev}]}]).

-compile(export_all).

test_import_as() ->
    L = rev(lists:seq(1,10)),
    L = lists:reverse(ls(1,10)),
    Fun = fun rev/1,
    [b, a] = Fun([a, b]).

test() ->
    test:test([{?MODULE, [test_import_as]}], [report, {name, ?MODULE}]).
