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

-module(test_t, [InnerMonad]).
-compile({parse_transform, do}).

-behaviour(monad).
-export(['>>='/2, return/1, fail/1, run/1, test/2]).

-ifdef(use_specs).
-type(monad(_A) :: fun (() -> 'passed' | {'failed', any()})).
-include("monad_specs.hrl").
-endif.

'>>='(X, Fun) -> fun () ->
                         case X() of
                             passed -> (Fun(passed))();
                             Failed -> Failed
                         end
                 end.

return(_A) -> fun () -> InnerMonad:return(passed) end.

%% This is the equivalent of
%%     fail msg = ErrorT $ return (Left (strMsg msg))
%% from the instance (Monad m, Error e) => Monad (ErrorT e m)
%%
%% http://hackage.haskell.org/packages/archive/mtl/1.1.0.2/doc/html/src/Control-Monad-Error.html#ErrorT
%%
%% I.e. note that calling fail on the outer monad is not a failure of
%% the inner monad: it is success of the inner monad, but the failure
%% is encapsulated.
fail(X)    -> fun () -> InnerMonad:return({failed, X}) end.

run(Fun) -> Fun().

test(Funs, Options) ->
    Result = run(monad:sequence(THIS, test(Funs))),
    case proplists:get_bool(report, Options) of
        true ->
            Name = proplists:get_value(name, Options, anonymous),
            case Result of
                passed ->
                    io:format("Test suite '~p' passed.~n", [Name]);
                {failed, Reason} ->
                    io:format("Test suite '~p' failed with ~p.~n",
                              [Name, Reason])
            end;
        false ->
            ok
    end,
    Result.


test([]) ->
    [];

test([{Module, {Label, FunName}}|Funs])
  when is_atom(Module) andalso is_atom(FunName) ->
    [do([THIS || hoist(Label, fun () -> Module:FunName() end)]) | test(Funs)];

test([{Module, FunName}|Funs])
  when is_atom(Module) andalso is_atom(FunName)
       andalso is_function({Module, FunName}, 0) ->
    [do([THIS || hoist(FunName, fun () -> Module:FunName() end)]) | test(Funs)];

test([{_Module, []}|Funs]) ->
    test(Funs);

test([{Module, [FunName|FunNames]}|Funs])
  when is_atom(Module) andalso is_atom(FunName) ->
    test([{Module, FunName}, {Module, FunNames} | Funs]);

test([{Label, Fun}|Funs]) when is_function(Fun, 0) ->
    [do([THIS || hoist(Label, Fun)]) | test(Funs)];

test([Fun|Funs]) when is_function(Fun, 0) ->
    [do([THIS || hoist(anonymous_function, Fun)]) | test(Funs)].


hoist(Label, PlainFun) ->
    try
        PlainFun(),
        THIS:return(passed)
    catch
        Class:Reason ->
            THIS:fail({Label, Class, Reason, erlang:get_stacktrace()})
    end.
