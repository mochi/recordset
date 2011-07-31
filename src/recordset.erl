%% @doc An optionally fixed-sized ordered set of complex terms, including
%% Erlang records.
%%
%% <h2>Rationale</h2>
%%
%% A <code>recordset</code> provides 3 unique properties not found with
%% <code>ordsets</code>.
%%
%% <ol>
%% <li>User defined identity of elements. <p>
%%
%%   One of the most useful features of a set is that each item only exists in
%%   the set once.  However both <code>sets</code> and <code>ordsets</code>
%%   require that the entire term match (<code>=:=</code>) or compare equal
%%   (<code>==</code>) respectively.  This requirement can be too strict for
%%   certain types of data (such as a set of 10 high scores with only a single
%%   score per user.)  <code>recordset</code> allows you to define a 2-arity
%%   function which will be used to compare the identity of two elements in
%%   the set.
%%
%%   <h3>Example</h3>
%%
%%   This <code>IdentityFun</code> compares 2 <code>#score{}</code> records
%%   and determines that they are the same if and only if they were created by
%%   the same player.  If two elements are the same, only one of them is
%%   allowed to exist in the list.
%%
%%   <pre>
%%     fun(#score{player=A}, #score{player=B}) ->
%%         A =:= B
%%     end.
%%   </pre>
%%
%% </p></li>
%%
%% <li>User defined sorting of elements. <p>
%%
%%   The most useful feature of <code>ordsets</code> is that they are indeed
%%   ordered.  However as with identity of elements, the sorting of elements
%%   is based on the entirety of the terms in the set.  <code>recordset</code>
%%   allows you to define another 2-arity function which will be used to sort
%%   the elements.
%%
%%   <h3>Example</h3>
%%
%%   This <code>SortFun</code> will cause <code>#score{}</code> records
%%   to be sorted by score in ascending order.  So that when converted to a
%%   list the lowest scores will be at the front of the list.
%%
%%   <pre>
%%     fun(#score{score=A}, #score{score=B}) ->
%%         A &lt; B
%%     end.
%%   </pre>
%%
%% </p></li>
%% <li>Optionally fixed size. <p>
%%
%%   <code>sets</code>, <code>ordsets</code>, and by default
%%   <code>recordset</code> will also grow in size as items are added to the
%%   set.  However it may be desirable to store a fixed number of elements
%%   (such as 50 or 100 highest scores.)  In which case <code>recordset</code>
%%   can be given a <code>max_size</code>, and once more than
%%   <code>max_size</code> items are added to the set items which sort lowest
%%   based on the supplied <code>SortFun</code> will be removed.
%%
%% </p></li>
%% </ol>
%%

-module(recordset).

-author('David Reid <dreid@mochimedia.com>').
-copyright('2011 Mochi Media, Inc.').


-export_type([recordset/0]).

-record(recordset, {
          max_size :: undefined | pos_integer(),
          identity_function :: cmp_fun(),
          sort_function :: cmp_fun(),
          set = [] :: list()
         }).

-opaque recordset() :: #recordset{}.
-type cmp_fun() :: {fun((term(), term()) -> boolean())}.
-type option() :: {atom(), term()}.
-type op() :: statebox:op().

-export([new/2, new/3]).
-export([from_list/2, from_list/4, to_list/1]).
-export([is_recordset/1, size/1, max_size/1]).
-export([add/2, delete/2]).
-export([statebox_add/1, statebox_delete/1]).

%% @equiv new(IdentityFun, SortFun, [])
-spec new(cmp_fun(), cmp_fun()) -> recordset().
new(IdentityFun, SortFun) ->
    new(IdentityFun, SortFun, []).

%% @doc Create an empty <code>recordset</code>.
%% Options:
%% <dl>
%%  <dt><code>max_size</code></dt>
%%  <dd>Specifies the maximum number of elements that will exist in the set.
%%  </dd>
%% </dl>
-spec new(cmp_fun(), cmp_fun(), [option()]) -> recordset().
new(IdentityFun, SortFun, Options) ->
    #recordset{max_size=proplists:get_value(max_size, Options),
               identity_function=IdentityFun,
               sort_function=SortFun}.


%% @doc Return <code>true</code> if the argument is a <code>recordset</code>,
%%      <code>false</code> otherwise.
-spec is_recordset(any()) -> boolean().
is_recordset(#recordset{}) ->
    true;
is_recordset(_) ->
    false.


%% @equiv from_list(List, recordset:new(IdentityFun, SortFun, Options))
-spec from_list([term()], cmp_fun(), cmp_fun(), [option()]) -> recordset().
from_list(List, IdentityFun, SortFun, Options) ->
    from_list(List, recordset:new(IdentityFun, SortFun, Options)).

%% @doc Populate the specified <code>RecordSet</code> with the given
%%      <code>List</code> of elements.
-spec from_list([term()], recordset()) -> recordset().
from_list(List, RecordSet) ->
    lists:foldl(fun(Term, RS) ->
                        recordset:add(Term, RS)
                end,
                RecordSet,
                List).


%% @doc Return the elements in the <code>recordset</code> as an ordered list
%%      of elements.
-spec to_list(recordset()) -> list().
to_list(#recordset{set=Set}) ->
    Set.


%% @doc Return the current size of the given <code>recordset</code> as an
%%      integer.
-spec size(recordset()) -> integer().
size(#recordset{set=Set}) ->
    length(Set).


%% @doc Return the max size as an integer or <code>undefined</code> if this
%%      <code>recordset</code> is not of a fixed size.
-spec max_size(recordset()) -> undefined | integer().
max_size(#recordset{max_size=MaxSize}) ->
    MaxSize.


%% @doc Add <code>Term</code> to the <code>recordset</code>.
%%
%% If the <code>recordset</code> is fixed-sized and <code>Term</code> is the
%% smallest element when <code>SortFun</code> and <code>max_size</code> has
%% been exceeded, then <code>Term</code> will not be added to the set.  And if
%% <code>Term</code> is not the smallest element in the set, the new smallest
%% element will be removed.
%%
%% If the <code>recordset</code> is not fixed-sized then <code>Term</code>
%% will be added to the set as long as <code>IdentityFun</code> does not
%% determine that is the same as an existing element and <code>SortFun</code>
%% does not determine that it is smaller than the existing element with the
%% same identity.
-spec add(term(), recordset()) -> recordset().
add(Term, RecordSet = #recordset{set=[]}) ->
    RecordSet#recordset{set=[Term]};
add(Term, RecordSet = #recordset{
            max_size=MaxSize,
            identity_function=IdentityFun,
            sort_function=SortFun,
            set=Set}) ->
    Set1 = case add_1(Term, IdentityFun, SortFun, Set) of
               Set0 when is_integer(MaxSize),
                         length(Set0) > MaxSize ->
                   truncate(Set0, length(Set0) - MaxSize);
               Set0 ->
                   Set0
           end,
    RecordSet#recordset{set=Set1}.

add_1(Term, IdentityFun, SortFun, [H | Set] = FullSet) ->
    case SortFun(Term, H) of
        true ->
            case IdentityFun(Term, H) of
                true ->
                    FullSet;
                false ->
                    [Term | FullSet]
            end;
        false ->
            case IdentityFun(Term, H) of
                true ->
                    add_1(Term, IdentityFun, SortFun, Set);
                false ->
                    [H | add_1(Term, IdentityFun, SortFun, Set)]
            end
    end;
add_1(Term, _IdentityFun, _SortFun, []) ->
    [Term].

truncate(S, 0) ->
    S;
truncate([_H | Set], I) ->
    truncate(Set, I-1).


%% @doc Remove an element from the <code>recordset</code>.
-spec delete(term(), recordset()) -> recordset().
delete(_Term, RecordSet = #recordset{set=[]}) ->
    RecordSet;
delete(Term, RecordSet = #recordset{
               identity_function=IdentityFun,
               set=Set}) ->
    RecordSet#recordset{set=delete_1(Term, IdentityFun, Set)}.


delete_1(Term, IdentityFun, [H | Set]) ->
    case IdentityFun(Term, H) of
        true ->
            Set;
        false ->
            [H | delete_1(Term, IdentityFun, Set)]
    end.


%% @doc Return a <code>statebox:op()</code> which will add the given
%%      <code>Term</code> to a <code>recordset</code>.
-spec statebox_add(term()) -> op().
statebox_add(Term) ->
    {fun ?MODULE:add/2, [Term]}.


%% @doc Return a <code>statebox:op()</code> which will delete the given
%%      <code>Term</code> from a <code>recordset</code>.
-spec statebox_delete(term()) -> op().
statebox_delete(Term) ->
    {fun ?MODULE:delete/2, [Term]}.
