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
 3. Newest score wins.

So our record contains a timestamp, the score, and the uid of the
player.

    #score{timestamp=now(),
           score=10,
           player=1}.


To initialize an empty recordset we call ``recordset:new/3`` which takes 3
arguments:

 1. ``IdentityFun`` - A 2-arity fun which will return ``true`` if it's two
arguments are have the same identity.
 2. ``SortFun`` - A 2-arity fun which will return ``true`` if it's first
argument is less than it's second argument.
 3. ``Options`` - This is an erlang ``proplist`` of options.  Currently the
only supported option is ``max_size`` which is a positive integer indicating
the maximum number of items allowed to be in the set.

Below we will initialize a set ``Scores0`` which satisfies the previously
described scoreboard properties.

    Scores0 = recordset:new(fun(#score{player=PlayerA},
                                #score{player=PlayerB}) ->
                                PlayerA =:= PlayerB
                            end,
                            fun(#score{timestamp=TsA, score=ScoreA},
                                #score{timestamp=TsB, score=ScoreB}) ->
                                {ScoreA, TsA} < {ScoreB, TsB}
                            end,
                            [{max_size, 10}]).

Our ``IdentityFun`` compares only the player ids of the two scores.  This
ensures that only a single score for each player exists in the set.  While our
``SortFun`` compares both the timestamp values and the score values ensuring
that higher scores will be preferred over lower scores and newer scores over
older ones.  The only option in the ``Options`` list is of course ``max_size``
which limits us to 10 scores at a time.

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

    Scores4 = Scores3.

You may also remove scores for a user with ``recordset:delete/2``.

    Scores5 = recordset:delete(#score{player=2}, Scores4).

    [#score{timestamp=3, score=20, player=1}] = recordset:to_list(Scores5).

Notice that we did not specify a score or timestamp value when calling
``recordset:delete/2``.  We can do this because ``recordset:delete/2`` is only
concerned about the identity of a term as determined by the ``IdentityFun``.


Statebox
--------

recordset also provides helper functions ``recordset:statebox_add/1`` and
``recordset:statebox_delete/1`` which return [statebox] operations and allow
you to easily store recordsets in an eventually consistent data store like
[riak].

[statebox]: https://github.com/mochi/statebox
[riak]: http://www.basho.com/products_riak_overview.php
