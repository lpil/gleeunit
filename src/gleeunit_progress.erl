%% A formatter adapted from Sean Cribb's https://github.com/seancribbs/eunit_formatters

%% @doc A listener/reporter for eunit that prints '.' for each
%% success, 'F' for each failure, and 'E' for each error. It can also
%% optionally summarize the failures at the end.
-module(gleeunit_progress).

-include_lib("eunit/include/eunit.hrl").

-behaviour(eunit_listener).

-define(RED, "\e[0;31m").
-define(GREEN, "\e[0;32m").
-define(YELLOW, "\e[0;33m").
-define(CYAN, "\e[0;36m").
-define(RESET, "\e[0m").

%% eunit_listener callbacks
-export([
         init/1,
         handle_begin/3,
         handle_end/3,
         handle_cancel/3,
         terminate/2,
         start/0,
         start/1
        ]).

-import(gleeunit_binomial_heap, [new/0]).

-record(state, {
          status = #{}     :: euf_dict(),
          failures = []    :: [[pos_integer()]],
          skips = []       :: [[pos_integer()]],
          timings = new()  :: gleeunit_binomial_heap:binomial_heap(),
          colored = true   :: boolean(),
          profile = false  :: boolean(),
          coverage = false :: boolean()
        }).

-type euf_dict() :: #{}.

%% Startup
start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

%%------------------------------------------
%% eunit_listener callbacks
%%------------------------------------------
init(Options) ->
    Coverage =
        case proplists:get_value(cover, Options, false) of
            true -> cover:start(), true;
            false -> false
        end,

    #state{colored = proplists:get_bool(colored, Options),
           profile = proplists:get_bool(profile, Options),
           coverage = Coverage}.

handle_begin(group, Data, St) ->
    GID = proplists:get_value(id, Data),
    Map = St#state.status,
    St#state{status = maps:put(GID, maps:from_list([{type, group} | Data]), Map)};
handle_begin(test, Data, St) ->
    TID = proplists:get_value(id, Data),
    Map = St#state.status,
    St#state{status = maps:put(TID, maps:from_list([{type, test} | Data]), Map)}.

handle_end(group, Data, St) ->
    St#state{status = merge_on_end(Data, St#state.status)};
handle_end(test, Data, St) ->
    NewStatus = merge_on_end(Data, St#state.status),
    St1 = print_progress(Data, St),
    St2 = record_timing(Data, St1),
    St2#state{status = NewStatus}.

handle_cancel(_, Data, #state{status = Status, skips = Skips} = St) ->
    Status1 = merge_on_end(Data, Status),
    ID = proplists:get_value(id, Data),
    St#state{status = Status1, skips = [ID | Skips]}.

terminate({ok, Data}, St) ->
    print_failures(St),
    print_pending(St),
    print_profile(St),
    print_timing(St),
    print_results(Data, St);

terminate({error, Reason}, St) ->
    io:nl(), io:nl(),
    print_colored(io_lib:format("Eunit failed: ~25p~n", [Reason]), ?RED, St),
    sync_end(error).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.

%%------------------------------------------
%% Print and collect information during run
%%------------------------------------------
print_progress(Data, St) ->
    TID = proplists:get_value(id, Data),
    case proplists:get_value(status, Data) of
        ok ->
            print_progress_success(St),
            St;
        {skipped, _Reason} ->
            print_progress_skipped(St),
            St#state{skips = [TID | St#state.skips]};
        {error, Exception} ->
            print_progress_failed(Exception, St),
            St#state{failures = [TID | St#state.failures]}
    end.

record_timing(Data, State = #state{timings = T, profile = true}) ->
    TID = proplists:get_value(id, Data),
    case lists:keyfind(time, 1, Data) of
        {time, Int} ->
            %% It's a min-heap, so we insert negative numbers instead
            %% of the actual and normalize when we report on them.
            T1 = gleeunit_binomial_heap:insert(-Int, TID, T),
            State#state{timings = T1};
        false ->
            State
    end;
record_timing(_Data, State) ->
    State.

print_progress_success(St) ->
    print_colored(".", ?GREEN, St).

print_progress_skipped(St) ->
    print_colored("*", ?YELLOW, St).

print_progress_failed(_Exc, St) ->
    print_colored("F", ?RED, St).

merge_on_end(Data, Map) ->
    Value = #{id := ID} = maps:from_list(Data),
    maps:update_with(ID, fun(Old) -> maps:merge_with(fun merge_data/3, Old, Value) end, Value, Map).

merge_data(_K, undefined, X) -> X;
merge_data(_K, X, undefined) -> X;
merge_data(_K, _, X) -> X.

%%------------------------------------------
%% Print information at end of run
%%------------------------------------------
print_failures(#state{failures = []}) -> ok;
print_failures(#state{failures = Fails} = State) ->
    io:nl(),
    io:fwrite("Failures:~n",[]),
    lists:foldr(print_failure_fun(State), 1, Fails),
    ok.

print_failure_fun(#state{status = Status} = State) ->
    fun(Key, Count) ->
            TestData = #{status := DataStatus, output := DataOutput} = maps:get(Key, Status),
            TestId = format_test_identifier(TestData),
            io:fwrite("~n  ~p) ~ts~n", [Count, TestId]),
            print_failure_reason(DataStatus, DataOutput, State),
            io:nl(),
            Count + 1
    end.

print_gleam_location(#{function := Function, line := Line, module := Module }, State) ->
    X = indent(5, "location: ~s.~s:~p~n", [Module, Function, Line]),
    print_colored(X, ?CYAN, State);
print_gleam_location(_, _) ->
    ok.

inspect(X) ->
    gleam@string:inspect(X).

print_gleam_failure_reason(
    #{gleam_error := assert, message := Message, value := Value},
    State
) ->
    print_colored(indent(5, "~s~n", [Message]), ?RED, State),
    print_colored(indent(5, "   value: ", []), ?RED, State),
    print_colored(indent(0, "~ts~n", [inspect(Value)]), ?RESET, State);
print_gleam_failure_reason(
    #{gleam_error := todo, message := Message},
    State
) ->
    print_colored(indent(5, "todo expression run~n", []), ?RED, State),
    print_colored(indent(5, " message: ", []), ?RED, State),
    print_colored(indent(0, "~s~n", [Message]), ?RESET, State);
print_gleam_failure_reason(Error, State) ->
    print_colored(indent(5, "~p~n", [Error]), ?RED, State).

% New Gleeunit specific formatters
print_failure_reason(
    {error, {error, #{gleam_error := _} = Error, Stack}}, Output, State
) when is_list(Stack) ->
    print_gleam_failure_reason(Error, State),
    print_gleam_location(Error, State),
    print_stack(Stack, State),
    print_failure_output(5, Output, State);
print_failure_reason({error, {error, {case_clause, Value}, Stack}}, Output, State) when is_list(Stack) ->
    print_colored(indent(5, "No case clause matched~n", []), ?RED, State),
    print_colored(indent(5, "Value: ", []), ?CYAN, State),
    print_colored(indent(0, "~ts~n", [inspect(Value)]), ?RESET, State),
    print_stack(Stack, State),
    print_failure_output(5, Output, State);
% From the original Erlang version
print_failure_reason({skipped, Reason}, _Output, State) ->
    print_colored(io_lib:format("     ~ts~n", [format_pending_reason(Reason)]),
                  ?RED, State);
print_failure_reason({error, {_Class, Term, _}}, Output, State) when
        is_tuple(Term), tuple_size(Term) == 2, is_list(element(2, Term)) ->
    print_assertion_failure(Term, State),
    print_failure_output(5, Output, State);
print_failure_reason({error, {error, Error, Stack}}, Output, State) when is_list(Stack) ->
    print_colored(indent(5, "Failure: ~p~n", [Error]), ?RED, State),
    print_stack(Stack, State),
    print_failure_output(5, Output, State);
print_failure_reason({error, Reason}, Output, State) ->
    print_colored(indent(5, "Failure: ~p~n", [Reason]), ?RED, State),
    print_failure_output(5, Output, State).

gleam_format_module_name(Module) ->
    string:replace(atom_to_list(Module), "@", "/", all).

print_stack(Stack, State) ->
    print_colored(indent(5, "stacktrace:~n", []), ?CYAN, State),
    print_stackframes(Stack, State).
print_stackframes([{eunit_test, _, _, _} | Stack], State) ->
    print_stackframes(Stack, State);
print_stackframes([{eunit_proc, _, _, _} | Stack], State) ->
    print_stackframes(Stack, State);
print_stackframes([{Module, Function, _Arity, _Location} | Stack], State) ->
    GleamModule = gleam_format_module_name(Module),
    print_colored(indent(7, "~s.~p~n", [GleamModule, Function]), ?CYAN, State),
    print_stackframes(Stack, State);
print_stackframes([], _State) ->
    ok.


print_failure_output(_, <<>>, _) -> ok;
print_failure_output(_, undefined, _) -> ok;
print_failure_output(Indent, Output, State) ->
    print_colored(indent(Indent, "output: ~ts", [Output]), ?CYAN, State).

print_assertion_failure({Type, Props}, State) ->
    FailureDesc = format_assertion_failure(Type, Props, 5),
    print_colored(FailureDesc, ?RED, State),
    io:nl().

print_pending(#state{skips = []}) ->
    ok;
print_pending(#state{status = Status, skips = Skips} = State) ->
    io:nl(),
    io:fwrite("Pending:~n", []),
    lists:foreach(fun(ID) ->
                          Info = maps:get(ID, Status),
                          case maps:get(reason, Info, undefined) of
                              undefined ->
                                  ok;
                              Reason ->
                                  print_pending_reason(Reason, Info, State)
                          end
                  end, lists:reverse(Skips)),
    io:nl().

print_pending_reason(Reason0, #{type := Type} = Data, State) ->
    Text = case Type of
               group ->
                   io_lib:format("  ~ts~n", [maps:get(desc, Data)]);
               test ->
                   io_lib:format("  ~ts~n", [format_test_identifier(Data)])
           end,
    Reason = io_lib:format("    %% ~ts~n", [format_pending_reason(Reason0)]),
    print_colored(Text, ?YELLOW, State),
    print_colored(Reason, ?CYAN, State).

print_profile(#state{timings = T, status = #{ [] := #{time := TotalTime} }, profile = true} = State) ->
    TopN = gleeunit_binomial_heap:take(10, T),
    TopNTime = abs(lists:sum([ Time || {Time, _} <- TopN ])),
    if TotalTime =/= undefined andalso TotalTime > 0 andalso TopN =/= [] ->
            TopNPct = (TopNTime / TotalTime) * 100,
            io:nl(), io:nl(),
            io:fwrite("Top ~p slowest tests (~ts, ~.1f% of total time):", [length(TopN), format_time(TopNTime), TopNPct]),
            lists:foreach(print_timing_fun(State), TopN),
            io:nl();
       true -> ok
    end;
print_profile(#state{profile = false}) ->
    ok.

print_timing(#state{status = #{ [] := #{time := Time} }}) ->
    io:nl(),
    io:fwrite("Finished in ~ts~n", [format_time(Time)]),
    ok.

print_results(Data, State) ->
    Pass = proplists:get_value(pass, Data, 0),
    Fail = proplists:get_value(fail, Data, 0),
    Skip = proplists:get_value(skip, Data, 0),
    Cancel = proplists:get_value(cancel, Data, 0),
    Total = Pass + Fail + Skip + Cancel,
    {Color, Result} = if Fail > 0 -> {?RED, error};
                         Skip > 0; Cancel > 0 -> {?YELLOW, error};
                         Pass =:= 0 -> {?YELLOW, ok};
                         true -> {?GREEN, ok}
                      end,
    print_results(Color, Total, Fail, Skip, Cancel, State),
    sync_end(Result).

print_results(Color, 0, _, _, _, State) ->
    print_colored(Color, "0 tests\n", State);
print_results(Color, Total, Fail, Skip, Cancel, State) ->
    SkipText = format_optional_result(Skip, "skipped"),
    CancelText = format_optional_result(Cancel, "cancelled"),
    Text = io_lib:format("~p tests, ~p failures~ts~ts~n", [Total, Fail, SkipText, CancelText]),
    print_colored(Text, Color, State).

print_timing_fun(#state{status = Status} = State) ->
    fun({Time, Key}) ->
            TestData = maps:get(Key, Status),
            TestId = format_test_identifier(TestData),
            io:nl(),
            io:fwrite("  ~ts~n", [TestId]),
            print_colored(["    "|format_time(abs(Time))], ?CYAN, State)
    end.

%%------------------------------------------
%% Print to the console with the given color
%% if enabled.
%%------------------------------------------
print_colored(Text, Color, #state{colored = true}) ->
    io:fwrite("~s~ts~s", [Color, Text, ?RESET]);
print_colored(Text, _Color, #state{colored = false}) ->
    io:fwrite("~ts", [Text]).

%%------------------------------------------
%% Generic data formatters
%%------------------------------------------
format_function_name(M, F) ->
    M1 = gleam_format_module_name(M),
    io_lib:format("~ts.~ts", [M1, F]).

format_optional_result(0, _) ->
    [];
format_optional_result(Count, Text) ->
    io_lib:format(", ~p ~ts", [Count, Text]).

format_test_identifier(#{source := {Mod, Fun, _}} = Data) when is_map(Data) ->
    Line = case maps:get(line, Data, 0) of
               0 -> "";
               L -> io_lib:format(":~p", [L])
           end,
    Desc = case maps:get(desc, Data, undefined) of
               undefined ->  "";
               DescText -> io_lib:format(": ~ts", [DescText])
           end,
    io_lib:format("~ts~ts~ts", [format_function_name(Mod, Fun), Line, Desc]).

format_time(undefined) ->
    "? seconds";
format_time(Time) ->
    io_lib:format("~.3f seconds", [Time / 1000]).

format_pending_reason({module_not_found, M}) ->
    M1 = gleam_format_module_name(M),
    io_lib:format("Module '~ts' missing", [M1]);
format_pending_reason({no_such_function, {M, F, _}}) ->
    M1 = gleam_format_module_name(M),
    io_lib:format("Function ~ts undefined", [format_function_name(M1,F)]);
format_pending_reason({exit, Reason}) ->
    io_lib:format("Related process exited with reason: ~p", [Reason]);
format_pending_reason(Reason) ->
    io_lib:format("Unknown error: ~p", [Reason]).

%% @doc Formats all the known eunit assertions, you're on your own if
%% you make an assertion yourself.
format_assertion_failure(Type, Props, I) when Type =:= assertion_failed
                                            ; Type =:= assert ->
    Keys = proplists:get_keys(Props),
    HasEUnitProps = ([expression, value] -- Keys) =:= [],
    HasHamcrestProps = ([expected, actual, matcher] -- Keys) =:= [],
    if
        HasEUnitProps ->
            [indent(I, "Failure: ?assert(~ts)~n", [proplists:get_value(expression, Props)]),
             indent(I, "  expected: true~n", []),
             case proplists:get_value(value, Props) of
                 false ->
                     indent(I, "       got: false", []);
                 {not_a_boolean, V} ->
                     indent(I, "       got: ~p", [V])
             end];
        HasHamcrestProps ->
            [indent(I, "Failure: ?assertThat(~p)~n", [proplists:get_value(matcher, Props)]),
             indent(I, "  expected: ~ts~n", [inspect(proplists:get_value(expected, Props))]),
             indent(I, "       got: ~ts", [inspect(proplists:get_value(actual, Props))])];
        true ->
            [indent(I, "Failure: unknown assert: ~p", [Props])]
    end;

format_assertion_failure(Type, Props, I) when Type =:= assertMatch_failed
                                            ; Type =:= assertMatch ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure: ?assertMatch(~ts, ~ts)~n", [Pattern, Expr]),
     indent(I, "  expected: = ~ts~n", [Pattern]),
     indent(I, "       got: ~p", [Value])];

format_assertion_failure(Type, Props, I) when Type =:= assertNotMatch_failed
                                                             ; Type =:= assertNotMatch  ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [indent(I, "Failure: ?assertNotMatch(~ts, ~ts)~n", [Pattern, Expr]),
     indent(I, "  expected not: = ~ts~n", [Pattern]),
     indent(I, "           got:   ~p", [Value])];

format_assertion_failure(Type, Props, I) when Type =:= assertEqual_failed
                                            ; Type =:= assertEqual  ->
    Expected = inspect(proplists:get_value(expected, Props)),
    Value = inspect(proplists:get_value(value, Props)),
    [indent(I, "Values were not equal~n", []),
     indent(I, "expected: ~ts~n", [Expected]),
     indent(I, "     got: ~ts", [Value])];

format_assertion_failure(Type, Props, I) when Type =:= assertNotEqual_failed
                                            ; Type =:= assertNotEqual ->
    Value = inspect(proplists:get_value(value, Props)),
    [indent(I, "Values were equal~n", []),
     indent(I, "expected: not ~ts~n,", [Value]),
     indent(I, "     got: ~ts", [Value])];

format_assertion_failure(Type, Props, I) when Type =:= assertException_failed
                                            ; Type =:= assertException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DATA
    [indent(I, "Failure: ?assertException(~ts, ~ts, ~ts)~n", [Class, Term, Expr]),
     case proplists:is_defined(unexpected_success, Props) of
         true ->
             [indent(I, "  expected: exception ~ts but nothing was raised~n", [Pattern]),
              indent(I, "       got: value ~p", [proplists:get_value(unexpected_success, Props)])];
         false ->
             Ex = proplists:get_value(unexpected_exception, Props),
             [indent(I, "  expected: exception ~ts~n", [Pattern]),
              indent(I, "       got: exception ~p", [Ex])]
     end];

format_assertion_failure(Type, Props, I) when Type =:= assertNotException_failed
                                            ; Type =:= assertNotException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DAT
    Ex = proplists:get_value(unexpected_exception, Props),
    [indent(I, "Failure: ?assertNotException(~ts, ~ts, ~ts)~n", [Class, Term, Expr]),
     indent(I, "  expected not: exception ~ts~n", [Pattern]),
     indent(I, "           got: exception ~p", [Ex])];

format_assertion_failure(Type, Props, I) when Type =:= command_failed
                                            ; Type =:= command ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [indent(I, "Failure: ?cmdStatus(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: status ~p~n", [Expected]),
     indent(I, "       got: status ~p", [Status])];

format_assertion_failure(Type, Props, I) when Type =:= assertCmd_failed
                                            ; Type =:= assertCmd ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [indent(I, "Failure: ?assertCmdStatus(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: status ~p~n", [Expected]),
     indent(I, "       got: status ~p", [Status])];

format_assertion_failure(Type, Props, I) when Type =:= assertCmdOutput_failed
                                            ; Type =:= assertCmdOutput ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_output, Props),
    Output = proplists:get_value(output, Props),
    [indent(I, "Failure: ?assertCmdOutput(~p, ~p)~n", [Expected, Cmd]),
     indent(I, "  expected: ~p~n", [Expected]),
     indent(I, "       got: ~p", [Output])];

format_assertion_failure(Type, Props, I) ->
    indent(I, "~p", [{Type, Props}]).

indent(I, Fmt, Args) ->
    io_lib:format("~" ++ integer_to_list(I) ++ "s" ++ Fmt, [" "|Args]).

extract_exception_pattern(Str) ->
    ["{", Class, Term|_] = re:split(Str, "[, ]{1,2}", [unicode,{return,list}]),
    {Class, Term}.
