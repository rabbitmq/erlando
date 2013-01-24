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

-module(import_as).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    %% io:format("Before:~n~p~n~n", [Forms]),
    {[], Forms1} = lists:foldl(fun import_as/2, {[], []}, Forms),
    Forms2 = lists:reverse(Forms1),
    %% io:format("After:~n~s~n~n",
    %%           [erl_prettypr:format(erl_syntax:form_list(Forms2))]),
    Forms2.

import_as({attribute, Line, import_as, {Module, Aliases}}, Acc) ->
    import_as({attribute, Line, import_as, [{Module, Aliases}]}, Acc);
import_as({attribute, Line, import_as, List}, {Funs, Acc})
  when is_list(List) ->
    Funs1 =
        lists:foldl(
          fun ({Module, Aliases}, Acc1) when is_list(Aliases) ->
                  [alias_fun(Module, Line, Alias) || Alias <- Aliases] ++ Acc1;
              (WrongThing, Acc1) ->
                  [general_error_fun(Line, WrongThing) | Acc1]
          end, Funs, List),
    {Funs1, Acc};
import_as({attribute, Line, import_as, WrongThing}, {Funs, Acc}) ->
    {Funs, [general_error(Line, WrongThing) | Acc]};
import_as({eof, Line}, {Funs, Acc}) ->
    {Acc1, Line1} =
        lists:foldl(
          fun (Fun, {AccN, LineN}) -> {[Fun(LineN) | AccN], LineN + 1} end,
          {Acc, Line}, Funs),
    {[], [{eof, Line1} | Acc1]};
import_as(Other, {Funs, Acc}) ->
    {Funs, [Other | Acc]}.

alias_fun(Module, _AttLine, {{Dest, Arity}, Alias}) when is_atom(Module) andalso
                                                         is_atom(Dest) andalso
                                                         is_atom(Alias) andalso
                                                         is_integer(Arity) ->
    fun (Line) ->
            Vars = [{var, Line, list_to_atom("Var_" ++ integer_to_list(N))} ||
                       N <- lists:seq(1, Arity)],
            Body = {call, Line,
                    {remote, Line, {atom, Line, Module}, {atom, Line, Dest}},
                    Vars},
            {function, Line, Alias, Arity, [{clause, Line, Vars, [], [Body]}]}
    end;
alias_fun(_Module, AttLine, WrongThing) ->
    fun (_Line) ->
            Str = io_lib:format("~p", [WrongThing]),
            {error, {AttLine, erl_parse,
                     ["-import_as: Expected a pair of "
                      "{target_fun/arity, alias}, not: ", Str]}}
    end.

general_error_fun(AttLine, WrongThing) ->
    fun (_Line) -> general_error(AttLine, WrongThing) end.

general_error(AttLine, WrongThing) ->
    Str = io_lib:format("~p", [WrongThing]),
    {error, {AttLine, erl_parse, ["-import_as: invalid attribute value: ", Str]}}.
