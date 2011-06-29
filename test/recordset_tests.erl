-module(recordset_tests).

-include_lib("eunit/include/eunit.hrl").

-record(score, {timestamp=0, score=0, player=0}).

scoreset() ->
    recordset:new(
      fun(#score{player=A},
          #score{player=B}) ->
              A =:= B
      end,
      fun(#score{timestamp=TsA,
                 score=ScoreA},
          #score{timestamp=TsB,
                 score=ScoreB}) ->
              if
                  ScoreA =:= ScoreB ->
                      TsB < TsA;
                  true ->
                      ScoreA < ScoreB
              end
      end,
      [{max_size, 10}]).


new_test() ->
    RS = recordset:new(fun(A, B) ->
                               A =:= B
                       end,
                       fun(A, B) ->
                               A > B
                       end,
                       []),
    ?assertEqual([], recordset:to_list(RS)).


add_test() ->
    SS = scoreset(),
    SS1 = recordset:add(#score{score=10, player=1}, SS),

    ?assertEqual([#score{score=10, player=1}],
                 recordset:to_list(SS1)),

    SS2 = recordset:add(#score{score=5, player=2}, SS1),

    ?assertEqual([#score{score=5, player=2},
                  #score{score=10, player=1}],
                 recordset:to_list(SS2)),

    SS3 = recordset:add(#score{score=15, player=3}, SS2),


    ?assertEqual([#score{score=5, player=2},
                  #score{score=10, player=1},
                  #score{score=15, player=3}],
                 recordset:to_list(SS3)),

    SS4 = recordset:add(#score{timestamp=1, score=5, player=4}, SS3),

    ?assertEqual([#score{timestamp=1, score=5, player=4},
                  #score{score=5, player=2},
                  #score{score=10, player=1},
                  #score{score=15, player=3}],
                 recordset:to_list(SS4)),

    SS5 = recordset:add(#score{timestamp=1, score=20, player=1}, SS4),

    ?assertEqual([#score{timestamp=1, score=5, player=4},
                  #score{score=5, player=2},
                  #score{score=15, player=3},
                  #score{timestamp=1, score=20, player=1}],
                 recordset:to_list(SS5)).


max_size_test() ->
    SS0 = scoreset(),
    SS1 = lists:foldl(fun(I, SS) ->
                                recordset:add(#score{score=I, player=I}, SS)
                        end,
                        SS0, lists:seq(1,11)),

    List = recordset:to_list(SS1),
    ?assertEqual(#score{score=2,player=2}, hd(List)),
    ?assertEqual(10, length(List)).
