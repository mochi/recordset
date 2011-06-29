recordset
=========

A recordset is an optionally fixed size ordered set of complex types,
typically erlang records.  It is unique from an ``ordset`` in 3 ways.

1. User defined identity.
2. User defined sorting.
3. Optional and efficient fixed-sizedness.


Example: Scores
---------------

Imagine you'd like to store the top 10 scores for a given game.  The score
board has the following properties:

1. Limited to 10 scores.
2. One score per user.
4. Highest score wins.
3. Older scores win.

So our record contains a timestamp, the score, and the uid of the
player.

    #score{timestamp=now(),
           score=10,
           player=1}.


So we would create a recordset for this record as follows:

    Scores0 = recordset:new(fun(#score{player=PlayerA},
                                #score{player=PlayerB}) ->
                                PlayerA =:= PlayerB
                            end,
                            fun(#score{timestamp=TsA, score=ScoreA},
                                #score{timestamp=TsB, score=ScoreB}) ->
                                {ScoreA, TsA} > {TsB, ScoreB}
                            end,
                            [{max_size, 10}]).


And add a single element to it:

    Scores1 = recordset:add(#score{timestamp=1, score=10, player=1}, Scores0).

The list representation of the set should then contain a single element of the
score we just added.

    [#score{timestamp=1, score=10, player=1}] = recordset:to_list(Scores1).

If we add a lower score we then the first element in the list representation
should be the lower score and we should now have 2 total elements.

    Scores2 = recordset:add(#score{timestamp=2, score=5, player=2}, Scores1).

    [#score{timestamp=2, score=5, player=2},
     #score{timestamp=1, score=10, player=1}] = recordset:to_list(Scores2).

If we add a new score for an existing user that is _higher_ than the existing
score we should see 2 elements in the list with the new score for that user.

    Scores3 = recordset:add(#score{timestamp=3, score=20, player=1}, Scores2),

    [#score{timestamp=2, score=5, player=2},
     #score{timestamp=3, score=20, player=1}] = recordset:to_list(Scores3).

And if we add a lower score for an existing user:

    Scores4 = recordset:add(#score{timestamp=4, score=1, player=2}, Scores3).

    Score4 = Score3.
