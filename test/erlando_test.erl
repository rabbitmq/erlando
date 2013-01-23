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
%% Copyright (c) 2011-2013 Alex Kropivny; VMware, Inc; Eduard Sergeev.
%% All rights reserved.
%%

-module(erlando_test).
-export([all_test_/0]).

all_test_() ->
    Modules = [test_cut,
               test_do,
               test_import_as],
    [{Mod,Fun} || Mod <- Modules,
                  Fun <- extract_tests(Mod)].

extract_tests(Mod) ->
    [Fun || {Fun, 0} <- Mod:module_info(exports),
            lists:prefix("test_", atom_to_list(Fun))].
