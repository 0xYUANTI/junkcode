%%%============================================================================
%%% File        : conc.erl
%%% Author      : jakob@canned.primat.es
%%% Description : conc lists
%%% Created     : 21 Aug 2010
%%%
%%% http://research.sun.com/projects/plrg/Publications/ICFPAugust2009Steele.pdf
%%% http://github.com/dustin/erl-conc
%%%
%%%============================================================================

-module(conc).

-export([ nil/0
        , list/1
        , conc/2
        , null_p/1
        , singleton_p/1
        , item/1
        , left/1
        , right/1
        , split/2
        , first/1
        , rest/1
        , append/2
        , addleft/2
        , addright/2
        , from_list/1
        , to_list/1
        , mapreduce/4
        , map/2
        , reduce/3
        , length/1
        , filter/2
        , reverse/1
        ]).

-include_lib("eunit/include/eunit.hrl").

%%=============================================================================
%% definition

-type empty_list()    :: nil.
-type singleton()     :: {list, _}.
-type concatenation() :: {conc, _, _}.
-type conc_list()     :: empty_list() | singleton() | concatenation().

nil()                     -> nil.
list(A)                   -> {list, A}.
conc(Ys, Zs)              -> {conc, Ys, Zs}.

null_p(nil)               -> true;
null_p({list, _})         -> false;
null_p({conc, _, _})      -> false.
singleton_p(nil)          -> false;
singleton_p({list, _})    -> true;
singleton_p({conc, _, _}) -> false.

item({list, A})           -> A.
left({conc, Ys, _})       -> Ys.
right({conc, _, Zs})      -> Zs.
split({conc, Ys, Zs}, F)  -> F(Ys, Zs).

first(nil)                -> nil;
first({list, A})          -> A;
first({conc, Ys, _})      -> first(Ys).

rest(nil)                 -> nil;
rest({list, _})           -> nil;
rest({conc, Ys, Zs})      -> append(rest(Ys), Zs).

append(nil, _)            -> nil;
append(_, nil)            -> nil;
append(Xs, Ys)            -> rebalance(conc(Xs, Ys)).

%%=============================================================================
%% term() <-> conc_list()

-spec addleft(_, conc_list())  -> conc_list().
addleft(A, Xs)                 -> append(list(A), Xs).

-spec addright(conc_list(), _) -> conc_list().
addright(Xs, A)                -> append(Xs, list(A)).

from_list([])                  -> nil;
from_list([X|Xs])              -> rebalance({conc, list(X), from_list(Xs)}).

to_list(nil)                   -> [];
to_list({list, A})             -> [A];
to_list({conc, Ys, Zs})        -> to_list(Ys) ++ to_list(Zs).

%%-----------------------------------------------------------------------------
-define(L, [23, 47, 18, 11]).
-define(C, from_list([23, 47, 18, 11])).

addleft_test()  -> from_list([42|?L])    =:= addleft(42, ?C).
addright_test() -> from_list(?L ++ [42]) =:= addright(?C, 42).
from_to_test()  -> ?L                    =:= to_list(from_list(?L)).

%%=============================================================================
%% balancing

rebalance({conc, Ys, Zs} = Xs) ->
  DepthYs = depth(Ys),
  DepthZs = depth(Zs),
  if DepthYs > DepthZs -> rebalance_children(rotate_l(Ys, Zs));
     DepthYs < DepthZs -> rebalance_children(rotate_r(Ys, Zs));
     true              -> Xs
  end;
rebalance(X) -> X.

rebalance_children({conc, Ys, Zs})     -> {conc, rebalance(Ys), rebalance(Zs)};
rebalance_children(X)                  -> X.

rotate_l(Ys, nil)                      -> Ys;
rotate_l({conc, A, B}, {list, _} = Zs) -> {conc, A, {conc, B, Zs}};
rotate_l({conc, A, B}, {conc, C, D})   -> {conc, A, {conc, {conc, B, C}, D}}.

rotate_r(nil, Zs)                      -> Zs;
rotate_r({list, _} = Ys, {conc, C, D}) -> {conc, {conc, Ys, C}, D};
rotate_r({conc, A, B}, {conc, C, D})   -> {conc, {conc, A, {conc, B, C}}, D}.

depth(nil)                             -> 1;
depth({list, _})                       -> 1;
depth({conc, Ys, Zs})                  -> 1 + max(depth(Ys), depth(Zs)).

max(X, Y) when X >= Y                  -> X;
max(_, Y)                              -> Y.

%%-----------------------------------------------------------------------------

rebalance_test() ->
  Balanced    = {conc, {conc, {list, 23}, {list, 47}},
                       {conc, {list, 18}, {list, 11}}},
  UnBalancedA = {conc, {conc, {conc, {list, 23}, {list, 47}},
                              {list, 18}},
                       {list, 11}},
  UnBalancedB = {conc, {conc, {list, 23}, {conc, {list, 47}, nil}},
                       {conc, {list, 18}, {conc, nil, {list, 11}}}},
  UnBalancedC = {conc, {list, 23},
                       {conc, {list, 47},
                              {conc, {list, 18},
                                     {conc, {list, 11}, nil}}}},
  List        = to_list(rebalance(Balanced)),
  List        = to_list(rebalance(UnBalancedA)),
  List        = to_list(rebalance(UnBalancedB)),
  List        = to_list(rebalance(UnBalancedC)),
  ?assert(depth(rebalance(UnBalancedA)) =< depth(UnBalancedA)),
  ?assert(depth(rebalance(UnBalancedB)) =< depth(UnBalancedB)),
  ?assert(depth(rebalance(UnBalancedC)) =< depth(UnBalancedC)),
  ?assert(depth(rebalance(Balanced))    =< depth(Balanced)).

%%=============================================================================
%% mapreduce

mapreduce(_F, _G, Id, nil) ->
  Id;
mapreduce(F, _G, _Id, {list, A}) ->
  F(A);
mapreduce(F, G, Id, {conc, Ys, Zs}) ->
  Parent   = self(),
  Pid1     = spawn(fun() -> Parent ! {self(), mapreduce(F, G, Id, Ys)} end),
  Pid2     = spawn(fun() -> Parent ! {self(), mapreduce(F, G, Id, Zs)} end),
  {R1, R2} = {receive {Pid1, Res1} -> Res1 end,
              receive {Pid2, Res2} -> Res2 end},
  G(R1, R2).

map(F, Xs) ->
  mapreduce(fun(X) -> list(F(X)) end, fun append/2, nil, Xs).

reduce(G, Id, Xs) ->
  mapreduce(fun(X) -> X end, G, Id,  Xs).

length(Xs) ->
  mapreduce(fun(_) -> 1 end, fun(X, Y) -> X + Y end, 0, Xs).

filter(P, Xs) ->
  mapreduce(
    fun(X) ->
        case P(X) of
          true -> list(X);
          _    -> nil
        end
    end, fun append/2, nil, Xs).

reverse(Xs) ->
  mapreduce(fun list/1, fun(Ys, Zs) -> append(Zs, Ys) end, nil, Xs).

%%-----------------------------------------------------------------------------
-define(inc,  fun(X) -> X+1 end).
-define(plus, fun(X, Y) -> X + Y end).
-define(even, fun(X) -> X rem 2 =:= 0 end).

map_test()     -> lists:map(?inc, ?L)       =:= to_list(map(?inc, ?C)).
reduce_test()  -> lists:foldl(?plus, 0, ?L) =:= reduce(?plus, 0, ?C).
length_test()  -> erlang:length(?L)         =:= conc:length(?C).
filter_test()  -> lists:filter(?even, ?L)   =:= to_list(filter(?even, ?C)).
reverse_test() -> lists:reverse(?L)         =:= to_list(reverse(?C)).

%%% local variables:
%%% erlang-indent-level: 2
%%% end:
