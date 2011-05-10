Erlando
=======



Introduction
------------

Erlando is a set of syntax extensions for Erlang. Currently it
consists of two syntax extensions, both of which take the form of
[parse transformers](http://www.erlang.org/doc/man/erl_id_trans.html).

* [**Cut**](#cut): This adds support for cuts to Erlang. These are
  inspired by the
  [Scheme form of cuts](http://srfi.schemers.org/srfi-26/srfi-26.html). Cuts
  can be thought of as a light-weight form of abstraction, with
  similarities to partial application (or currying).
    
* [**Do**](#do): This adds support for do-syntax and monads to
  Erlang. These are heavily inspired by [Haskell](http://haskell.org),
  and the monads and libraries are near-mechanical translations from
  the Haskell GHC libraries.



Use
---

To use any of these parse transformers, you must add the necessary
`-compile` attributes to your Erlang source files. For example:

    -module(test).
    -compile({parse_transform, cut}).
    -compile({parse_transform, do}).
    ...

Then, when compiling `test.erl`, you must ensure `erlc` can locate
`cut.beam` by passing the suitable path to `erlc` with a `-pa` or
`-pz` argument. For example:

    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/cut.erl
    erlc -Wall +debug_info -I ./include -pa ebin -o ebin  src/do.erl
    erlc -Wall +debug_info -I ./include -pa test/ebin -pa ./ebin -o test/ebin test/src/test.erl

Note, if you're using QLC, you may find you need to be careful as to
the order of the parse transforms: I've found that the
`-compile({parse_transform, cut}).` must occur before the
`-include_lib("stdlib/include/qlc.hrl").`



Cut {#cut}
----------

### Motivation

Cut is motivated by the frequency with which simple abstractions (in a
lambda-calculus sense) are used in Erlang, and the relatively noisy
nature of declaring `fun`s. For example, it's quite common to see code
like:

    with_resource(Resource, Fun) ->
        case lookup_resource(Resource) of
            {ok, R}          -> Fun(R);
            {error, _} = Err -> Err
        end.
    
    my_fun(A, B, C) ->
        with_resource(A, fun (Resource) ->
                             my_resource_modification(Resource, B, C)
                         end).

I.e. a fun is very simply created in order to perform variable capture
from the its surrounding scope but to leave holes for further
arguments to be provided. Using a cut, the function `my_fun` can be
rewritten as:

    my_fun(A, B, C) ->
        with_resource(A, my_resource_modification(_, B, C)).


### Definition

Normally, the variable `_` can only occur in patterns: i.e. where
match occurs. This can be in assignment, in cases, and in function
heads. For example:

    {_, bar} = {foo, bar}.

Cut uses `_` in expressions to indicate where abstraction should
occur. Abstraction from cuts is __always__ performed on the
_shallowest_ enclosing expression. For example:

    list_to_binary([1, 2, math:pow(2, _)]).

will create the expression

    list_to_binary([1, 2, fun (X) -> math:pow(2, X) end]).

and not

    fun (X) -> list_to_binary([1, 2, math:pow(2, X)]) end.

It is fine to use multiple cuts in the same expression, and the
arguments to the created abstraction will match the order in which the
`_` var is found in the expression. For example:

    assert_sum_3(X, Y, Z, Sum) when X + Y + Z == S -> ok;
    assert_sum_3(_X, _Y, _Z, _Sum) -> {error, not_sum}.
    
    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        ok = Equals12(9, 2, 1).

It is perfectly legal to take cuts of cuts as the abstraction created
by the cut is a normal `fun` expression and thus can be re-cut as
necessary:

    test() ->
        Equals12 = assert_sum_3(_, _, _, 12),
        Equals5 = Equals12(_, _, 7),
        ok = Equals5(2, 3).

Note that because a simple `fun` is being constructed by the cut, the
arguments are evaluated prior to the cut function. For example:

    f1(_, _) -> io:format("in f1~n").

    test() ->
        F = f1(io:format("test line 1~n"), _),
        F(io:format("test line 2~n")).

will print out

    test line 2
    test line 1
    in f1

This is because the cut creates `fun (X) -> f1(io:format("test line
1~n"), X) end`. Thus it is clear that `X` must be evaluated first,
before the `fun` can be invoked.

Of course, I'm sure no one is crazy enough to have side-effects in
function argument expressions, so I doubt this will ever cause any
issues!

Cuts are not limited to function calls. They can be used in any
expression where they make sense:


#### Tuples

    F = {_, 3},
    {a, 3} = F(a).


#### Lists

    dbl_cons(List) -> [_, _ | List].
    
    test() ->
        F = dbl_cons([33]),
        [7, 8, 33] = F(7, 8).

Note that if you nest a list as a list tail in Erlang, it's still
treated as one expression. For example:

    A = [a, b | [c, d | [e]]]

is exactly the same (right from the Erlang parser onwards) as:

    A = [a, b, c, d, e]

I.e. those sub-lists, when they're in the tail position **do not**
form sub-expressions. Thus:

    F = [1, _, _, [_], 5 | [6, [_] | [_]]],
    %% This is the same as:
    %%  [1, _, _, [_], 5, 6, [_], _]
    [1, 2, 3, G, 5, 6, H, 8] = F(2, 3, 8),
    [4] = G(4),
    [7] = H(7).

However, be very clear about the difference between `,` and `|`: the
tail of a list is **only** defined following a `|`. Following a `,`,
you're just defining another list element.

    F = [_, [_]],
    %% This is **not** the same as [_, _] or its synonym: [_ | [_]]
    [a, G] = F(a),
    [b] = G(b).


#### Records

    -record(vector, { x, y, z }).
    
    test() ->
        GetZ = _#vector.z,
        7 = GetZ(#vector { z = 7 }),
        SetX = _#vector{x = _},
        V = #vector{ x = 5, y = 4 } = SetX(#vector{ y = 4 }, 5).


#### Case

    F = case _ of
            N when is_integer(N) -> N + N;
            N                    -> N
        end,
    10 = F(5),
    ok = F(ok).


See
[test_cut.erl](http://hg.rabbitmq.com/erlando/file/default/test/src/test_cut.erl)
for more examples, including use of cuts in list comprehensions and
binary construction.

Note that cuts are not allowed where the result of the cut can only be
useful by interacting with the evaluation scope. For example:

    F = begin _, _, _ end.

This is not allowed, because the arguments to `F` would have to be
evaluated before the invocation of its body, which would then have no
effect, as they're already fully evaluated by that point.



Do {#do}
--------

The Do parse transformer permits Haskell-style *do-notation* in
Erlang, which makes using monads, and monad transformers possible and
easy. With *do-notation*, monads tend to look like a lot of line
noise.


### The Inevitable Monad Tutorial

What follows is a very brief and mechanical introduction to monads. It
differs from a lot of the Haskell monad tutorials, because they tend
to view monads as a means of achieving sequencing of operations in
Haskell, which is a lazy language. Erlang is not a lazy language, but
the powerful abstractions possible from using monads are still very
worthwhile. Whilst this is a very mechanical tutorial, it should be
possible to see the more advanced abstractions possible.

Let's say we have the three lines of code:

    A = foo(),
    B = bar(A, dog),
    ok.

They are three, simple statements, which are evaluated
consecutively. What a monad gives you is control over what happens
between the statements: in Erlang, it is a programmatic comma.

If you wanted to implement a programmatic comma, how would you do it?
You might start with something like:

    A = foo(),
    comma(),
    B = bar(A, dog),
    comma(),
    ok.

But that's not quite powerful enough, because unless `comma/0` throws
some sort of exception, it can't actually stop the subsequent
expression from being evaluated. Most of the time we'd probably like
the `comma/0` function to be able to act on some variables which are
currently in scope, and that's not possible here either. So we should
extend the function `comma/0` so that it takes the result of the
preceding expression, and can choose whether or not the subsequent
expressions should be evaluated:

    comma(foo(),
          fun (A) -> comma(bar(A, dog),
                           fun (B) -> ok end)).

Thus the function `comma/2` takes all results from the previous
expression, and controls how and whether they are passed to the next
expression.

As defined, the `comma/2` function is the monadic function `>>=/2`.

Now it's pretty difficult to read the program with the `comma/2`
function (especially as Erlang annoyingly doesn't allow us to define
new infix functions), which is why some special syntax is
necessary. Haskell has it's *do-notation*, and so we've borrowed from
that and abused Erlang's list comprehensions. Haskell also has lovely
type-classes, which we've sort of faked specifically for monads. So,
with the Do parse transformer, you can write in Erlang:

    do([Monad ||
        A <- foo(),
        B <- bar(A, dog),
        ok]).

which is readable and straightforward, but is transformed into:

    Monad:'>>='(foo(),
                fun (A) -> Monad:'>>='(bar(A, dog),
                                       fun (B) -> ok end))

There is no intention that this latter form is any more readable than
the `comma/2` form - it is not. However, it should be clear that the
function `Monad:'>>='/2` now has complete control over what happens:
does the fun on the right hand side ever get invoked? If so, with what
value?



License
-------

(The MPL)

Software distributed under the License is distributed on an "AS IS"
basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
the License for the specific language governing rights and limitations
under the License.

The Original Code is Erlando.

The Initial Developer of the Original Code is VMware, Inc.
Copyright (c) 2011-2011 VMware, Inc.  All rights reserved.

