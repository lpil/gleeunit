%% A formatter adapted from Sean Cribb's https://github.com/seancribbs/eunit_formatters

-module(gleeunit_progress).
-define(NOTEST, true).

%% eunit_listener callbacks
-export([
    init/1, handle_begin/3, handle_end/3, handle_cancel/3, terminate/2,
    start/0, start/1
]).

-define(reporting, gleeunit@internal@reporting).

start() ->
    start([]).

start(Options) ->
    eunit_listener:start(?MODULE, Options).

init(_Options) ->
    ?reporting:new_state().

handle_begin(_test_or_group, _data, State) ->
    State.

handle_end(group, _data, State) ->
    State;
handle_end(test, Data, State) ->
    {AtomModule, AtomFunction, _Arity} = proplists:get_value(source, Data),
    Module = erlang:atom_to_binary(AtomModule),
    Function = erlang:atom_to_binary(AtomFunction),

    % EUnit swallows stdout, so print it to make debugging easier.
    case proplists:get_value(output, Data) of
        undefined -> ok;
        <<>> -> ok;
        Out -> print(Out)
    end,

    case proplists:get_value(status, Data) of
        ok ->
            ?reporting:test_passed(State, fun print/1);
        {skipped, _Reason} ->
            ?reporting:test_skipped(State, Module, Function, fun print/1);
        {error, {_, Exception, _Stack}} ->
            ?reporting:test_failed(State, Module, Function, Exception, fun print/1)
    end.


handle_cancel(_test_or_group, Data, State) ->
    ?reporting:test_failed(State, <<"gleeunit">>, <<"main">>, Data, fun print/1).

terminate({ok, _Data}, State) ->
    ?reporting:finished(State, fun print/1),
    ok;
terminate({error, Reason}, State) ->
    ?reporting:finished(State, fun print/1),
    io:fwrite("
Eunit failed:

~80p

This is probably a bug in gleeunit. Please report it.
", [Reason]),
    sync_end(error).

print(String) ->
    gleam@io:print(String).

sync_end(Result) ->
    receive
        {stop, Reference, ReplyTo} ->
            ReplyTo ! {result, Reference, Result},
            ok
    end.
