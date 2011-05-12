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

-module(test_m).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1]).

-ifdef(use_specs).
%% test_m _ is a test outcome
-type(monad(_A) :: 'passed' | {'failed', string(), any()}).
-include("monad_specs.hrl").
-endif.

-export([mktest/1, report/2]).

'>>='(passed, MFun) -> MFun(nil);
'>>='(Failure = {failed, _S, _X}, _Fun) -> Failure.

return(_A) -> passed.
fail(X)   -> {failed, "test_m", X}.

%% Convert a function into a safe test outcome producing function
%% mktest :: (string() -> b) -> (string() -> test_m b)
mktest(Fun) when is_function(Fun, 1) ->
    fun (String) ->
        try Fun(String), passed
        catch Class:Reason ->
            {failed, String, {Class, Reason}}
        end
    end.

report(String, passed) ->
    io_lib:format("Test suite '~s' passed.~n", [String]);
report(String, {failed, Test, Failure}) ->
    io_lib:format("Test suite '~s' failed in test '~s'.~n ===> ~p~n", [String, Test, Failure]).