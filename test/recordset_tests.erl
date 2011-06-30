-module(recordset_tests).

-include_lib("eunit/include/eunit.hrl").

%% Export scoreset_sort/2 so we can test the {Module, Function} form
%% of the recordset cmp_fun() args.
-export([scoreset_sort/2]).

%% Define a simple score record with a timestamp, score, and player id.
-record(score, {timestamp=0, score=0, player=0}).


%% Scores should be sorted in ascending order by score if two scores are equal
%% the newer score should win.
scoreset_sort(#score{timestamp=TsA, score=ScoreA},
              #score{timestamp=TsB, score=ScoreB}) ->
    {ScoreA, TsA} < {ScoreB, TsB}.

%% Initialize a scoreset defining identity as the same player id, with the
%% above sort function, and a max size of 10.
scoreset() ->
    recordset:new(
      fun(#score{player=A},
          #score{player=B}) ->
              A =:= B
      end,
      fun ?MODULE:scoreset_sort/2,
      [{max_size, 10}]).


add_test() ->
    %% Initialize an empty scoreset.
    SS = scoreset(),

    %% Add a single score and the recordset now consists of a single score.
    SS1 = recordset:add(#score{score=10, player=1}, SS),

    ?assertEqual([#score{score=10, player=1}],
                 recordset:to_list(SS1)),

    %% Add a second score lower than the first by a new player and the
    %% recordset has 2 elements with the second score being before the first
    %% score in the list.
    SS2 = recordset:add(#score{score=5, player=2}, SS1),

    ?assertEqual([#score{score=5, player=2},
                  #score{score=10, player=1}],
                 recordset:to_list(SS2)),

    %% Add a new score higher than the first and second by a third player
    %% and the recordset now contains all 3 scores the highest score
    %% at the end.
    SS3 = recordset:add(#score{score=15, player=3}, SS2),

    ?assertEqual([#score{score=5, player=2},
                  #score{score=10, player=1},
                  #score{score=15, player=3}],
                 recordset:to_list(SS3)),

    %% Add a new score with the same score as the 2nd but at a later timestamp
    %% by a new player.  There are now 4 elements in the list in the
    %% appropriate order.
    SS4 = recordset:add(#score{timestamp=1, score=5, player=4}, SS3),

    ?assertEqual([#score{score=5, player=2},
                  #score{timestamp=1, score=5, player=4},
                  #score{score=10, player=1},
                  #score{score=15, player=3}],
                 recordset:to_list(SS4)),



    %% Add a higher score for the first player and the recordset still has 4
    %% elements with the last element now being the highest score for player 1
    %% and the lower score for player one has been removed.
    SS5 = recordset:add(#score{timestamp=1, score=20, player=1}, SS4),

    ?assertEqual([#score{score=5, player=2},
                  #score{timestamp=1, score=5, player=4},
                  #score{score=15, player=3},
                  #score{timestamp=1, score=20, player=1}],
                 recordset:to_list(SS5)).


max_size_test() ->
    SS0 = scoreset(),

    %% Add 11 scores to a scoreset in ascending order.  The second score added
    %% is now at the front of the list with only 10 total elements in the
    %% list.
    SS1 = lists:foldl(fun(I, SS) ->
                                recordset:add(#score{score=I, player=I}, SS)
                        end,
                        SS0, lists:seq(1,11)),

    List = recordset:to_list(SS1),
    ?assertEqual(#score{score=2,player=2}, hd(List)),
    ?assertEqual(10, length(List)).


max_size_getter_test() ->
    %% Check that we can ask what the max size of a set is.
    ?assertEqual(10, recordset:max_size(scoreset())).


size_test() ->
    SS0 = scoreset(),

    %% The size of a newly initialized empty recordset should be 0.
    ?assertEqual(0, recordset:size(SS0)),

    %% Once we add an item to the set the size should be 1.
    SS1 = recordset:add(#score{}, SS0),

    ?assertEqual(1, recordset:size(SS1)),

    %% If we add more than max_size items to the set size shouldn't exceed the
    %% max_size.
    SS2 = lists:foldl(fun(I, SS) ->
                                recordset:add(#score{score=I, player=I}, SS)
                        end,
                        SS1, lists:seq(1,11)),

    ?assertEqual(10, recordset:size(SS2)),

    %% The size of the set as an ordered list of items should be the same as
    %% the size of the recordset.
    ?assertEqual(recordset:size(SS2),
                 length(recordset:to_list(SS2))).


delete_test() ->
    %% Create a new scoreset and add 3 scores.
    SS0 = scoreset(),
    SS1 = recordset:add(#score{score=10, player=1}, SS0),
    SS2 = recordset:add(#score{score=15, player=2}, SS1),
    SS3 = recordset:add(#score{score=20, player=3}, SS2),

    %% When we delete an item from the set the should contain the 2 other
    %% scores we added but not the score we deleted.
    SS4 = recordset:delete(#score{score=15, player=2}, SS3),
    ?assertEqual([#score{score=10, player=1},
                  #score{score=20, player=3}], recordset:to_list(SS4)).


delete_identity_test() ->
    %% create a new scoreset and add 1 score.
    SS0 = scoreset(),
    SS1 = recordset:add(#score{score=10, player=1}, SS0),

    %% delete/2 should only care about the identity of the items as determined
    %% by the IdentityFun and not complete record equality.  So when we delete
    %% A score only setting the player attribute any score for that player
    %% should be removed from the set.
    SS2 = recordset:delete(#score{player=1}, SS1),
    ?assertEqual([], recordset:to_list(SS2)).


delete_non_existant_test() ->
    SS0 = scoreset(),

    %% Deleting a score that does not exist in the recordset should succeed.
    SS1 = recordset:delete(#score{player=1}, SS0),
    ?assertEqual([], recordset:to_list(SS1)).


from_list_test() ->
    SS0 = scoreset(),

    %% Creating a set from a list should add all items to the existing set.
    SS1 = recordset:from_list([#score{score=20, player=2},
                               #score{score=10, player=1}], SS0),

    ?assertEqual([#score{score=10, player=1},
                  #score{score=20, player=2}], recordset:to_list(SS1)).


from_list_newset_test() ->
    %% You should also be able to create a new set by passing IdentityFun,
    %% SortFun, and Options to from_list/4
    SS = recordset:from_list([#score{score=20, player=2},
                              #score{score=10, player=1}],
                             fun(#score{player=A}, #score{player=B}) ->
                                     A =:= B
                             end,
                             fun ?MODULE:scoreset_sort/2,
                             [{max_size, 10}]),

    ?assertEqual([#score{score=10, player=1},
                  #score{score=20, player=2}],
                 recordset:to_list(SS)).


statebox_add_test() ->
    SS0 = scoreset(),

    SS1 = statebox:apply_op(
            recordset:statebox_add(#score{score=10, player=1}),
            SS0),

    ?assertEqual([#score{score=10, player=1}], recordset:to_list(SS1)).


statebox_delete_test() ->
    SS0 = scoreset(),

    SS1 = recordset:add(#score{score=10, player=1}, SS0),

    SS2 = statebox:apply_op(
            recordset:statebox_delete(#score{score=10, player=1}),
            SS1),

    ?assertEqual([], recordset:to_list(SS2)).


is_not_recordset_test() ->
    ?assertEqual(false, recordset:is_recordset([])).

is_recordset_test() ->
    ?assertEqual(true, recordset:is_recordset(scoreset())).
