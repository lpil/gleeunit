-module(gleeunit_binomial_heap).

-record(node,{
    rank = 0    :: non_neg_integer(),
    key         :: term(),
    value       :: term(),
    children = new() :: binomial_heap()
  }).

-export_type([binomial_heap/0, heap_node/0]).
-type binomial_heap() :: [ heap_node() ].
-type heap_node() :: #node{}.

-export([new/0, insert/2, insert/3, merge/2, delete/1, to_list/1, take/2, size/1, link/2]).

-spec new() -> binomial_heap().
new() ->
    [].

% Inserts a new pair into the heap (or creates a new heap)
-spec insert(term(), term()) -> binomial_heap().
insert(Key, Value) ->
    insert(Key, Value, []).

-spec insert(term(), term(), binomial_heap()) -> binomial_heap().
insert(Key, Value, Forest) ->
    insTree(#node{key = Key, value = Value}, Forest).

% Merges two heaps
-spec merge(binomial_heap(), binomial_heap()) -> binomial_heap().
merge(TS1, []) when is_list(TS1) -> TS1;
merge([], TS2) when is_list(TS2) -> TS2;
merge([#node{rank = R1} = T1 | TS1], [#node{rank = R2} | _] = F2) when R1 < R2 ->
    [T1 | merge(TS1, F2)];
merge([#node{rank = R1} | _] = F1, [#node{rank = R2} = T2 | TS2]) when R2 < R1 ->
    [T2 | merge(F1, TS2)];
merge([T1 | TS1], [T2 | TS2]) ->
    insTree(link(T1, T2), merge(TS1, TS2)).

% Deletes the top entry from the heap and returns it
-spec delete(binomial_heap()) -> {{term(), term()}, binomial_heap()}.
delete(TS) ->
    {#node{key=Key,value=Value,children=TS1},TS2} = getMin(TS),
    {{Key,Value},merge(lists:reverse(TS1),TS2)}.

% Turns the heap into list in heap order
-spec to_list(binomial_heap()) -> [{term(), term()}].
to_list([]) -> [];
to_list(List) when is_list(List) ->
    to_list([],List).
to_list(Acc, []) ->
    lists:reverse(Acc);
to_list(Acc,Forest) ->
    {Next, Trees} = delete(Forest),
    to_list([Next|Acc], Trees).

% Take N elements from the top of the heap
-spec take(non_neg_integer(), binomial_heap()) -> [{term(), term()}].
take(N,Trees) when is_integer(N), is_list(Trees) ->
    take(N,Trees,[]).
take(0,_Trees,Acc) ->
    lists:reverse(Acc);
take(_N,[],Acc)->
    lists:reverse(Acc);
take(N,Trees,Acc) ->
    {Top,T2} = delete(Trees),
    take(N-1,T2,[Top|Acc]).

% Get an estimate of the size based on the binomial property
-spec size(binomial_heap()) -> non_neg_integer().
size(Forest) ->
    erlang:trunc(lists:sum([math:pow(2,R) || #node{rank=R} <- Forest])).

%% Private API
-spec link(heap_node(), heap_node()) -> heap_node().
link(#node{rank=R,key=X1,children=C1}=T1,#node{key=X2,children=C2}=T2) ->
    case X1 < X2 of
        true ->
            T1#node{rank=R+1,children=[T2|C1]};
        _ ->
            T2#node{rank=R+1,children=[T1|C2]}
    end.

insTree(Tree, []) ->
    [Tree];
insTree(#node{rank = R1} = T1, [#node{rank = R2} | _] = TS) when R1 < R2 -> [T1 | TS];
insTree(T1, [T2 | Rest]) -> insTree(link(T1, T2), Rest).

getMin([T]) -> {T, []};
getMin([#node{key = K} = T | TS]) ->
    {#node{key = K1} = T1, TS1} = getMin(TS),
    case K < K1 of
        true -> {T, TS};
        _ -> {T1, [T | TS1]}
    end.
