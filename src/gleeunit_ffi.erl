-module(gleeunit_ffi).

-export([find_files/2, should_equal/2, should_not_equal/2, should_be_ok/1,
         should_be_error/1, start_coverage/0, compile_coverage/1,
         stop_coverage/0]).

-include_lib("eunit/include/eunit.hrl").

find_files(Pattern, In) ->
  Results = filelib:wildcard(binary_to_list(Pattern), binary_to_list(In)),
  lists:map(fun list_to_binary/1, Results).


should_equal(Actual, Expected) -> 
    ?assertEqual(Expected, Actual),
    nil.
should_not_equal(Actual, Expected) -> 
    ?assertNotEqual(Expected, Actual),
    nil.
should_be_ok(A) -> 
    ?assertMatch({ok, _}, A),
    element(2, A).
should_be_error(A) -> 
    ?assertMatch({error, _}, A),
    element(2, A).

start_coverage() ->
    cover:start(),
    nil.

compile_coverage(Mod) ->
    case cover:compile_beam(Mod) of
        {ok, _} ->
            {ok, nil};
        {error, not_main_node} ->
            {error, <<"not_main_node">>};
        {error, _Mod} ->
            {error, <<"invalid_module">>}
    end.

stop_coverage() ->
    case cover:analyse() of
        {result, Values, Errors} ->
            NewValues =
                [ {Mod, {Cov, NotCov}} || {{Mod,_F,_A}, {Cov, NotCov}} <- Values ],
            NewErrors =
                [ {Mod, {0, 0}} || {not_cover_compiled, Mod} <- Errors ],
            MapValues =
                lists:foldl(fun({Mod, {Cov, NotCov}}, Acc) ->
                                MergeFun = fun({OldCov, OldNotCov}) ->
                                            NewCov = OldCov + Cov,
                                            NewNotCov = OldNotCov + NotCov,
                                            {NewCov, NewNotCov}
                                            end,
                                maps:update_with(Mod, MergeFun, {Cov, NotCov}, Acc)
                            end, #{}, NewValues ++ NewErrors),
            Calc = fun(Cov, NotCov) -> Cov / (Cov + NotCov) * 100 end,
            Get = fun(Key, List) -> proplists:get_value(Key, List) end,
            ModInfo = fun(Mod) -> filename:basename(Get(source, Get(compile, Mod:module_info()))) end,
            Return = [ {cover, ModInfo(Mod), Cov, NotCov, Calc(Cov, NotCov)} || {Mod, {Cov, NotCov}} <- maps:to_list(MapValues) ],
            NSize = lists:max([string:len(Mod) || {cover, Mod, _, _, _} <- Return]),
            Size = integer_to_list(NSize),
            io:fwrite("\n\nTest coverage:\n\n"),
            io:format("~-" ++ Size ++ "s ~7s (~s)\n", ["Module", "Perc%", "Covered/Total"]),
            io:format([
                lists:duplicate(NSize, "-"), " ",
                lists:duplicate(7, "-"), " ",
                lists:duplicate(15, "-"), "\n"
            ]),
            lists:foreach(fun(Cover) -> print_cover(Cover, Size) end, Return),
            nil;

        {error, not_main_node} ->
            nil
    end.

print_cover({cover, Mod, Cov, NotCov, Percent}, Size) ->
    Total = Cov + NotCov,
    io:format("~-" ++ Size ++ "s ~6.2f% (~b/~b)~n", [Mod, Percent, Cov, Total]),
    nil.
