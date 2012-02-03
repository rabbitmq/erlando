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
%% The Initial Developer of the Original Code is Alex Kropivny.
%% Copyright (c) 2011-2012 Alex Kropivny; VMware, Inc.
%% All rights reserved.
%%

-module(erlando_test).
-compile(export_all).


all_test_() ->
    [ fun test_cut:test/0,
      fun test_do:test/0,
      fun test_import_as:test/0 ].
