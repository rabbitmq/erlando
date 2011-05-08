-module(erlando_test).
-compile(export_all).


all_test_() ->
    [ fun test_cut:test/0
    , fun test_do:test/0
    ].
