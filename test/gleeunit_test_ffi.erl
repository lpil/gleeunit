-module(gleeunit_test_ffi).
-export([rescue/1]).

rescue(F) ->
    try
        {ok, F()}
    catch
        _:Error:_ -> {error, Error}
    end.
