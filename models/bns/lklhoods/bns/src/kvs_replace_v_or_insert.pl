%% kvs_replace_v_or_insert( +KVs, +Key, -Old, +New, -NewKVs ).
%
% If Key-Old is in KVs, then replace it with New else add Key-New in the 
% appropriate place to produce NewKVs. KVs is key ordered, so is NewKVs.
% The null value for Old (when Key not in KVs) is [].
% 
%==
%  assert( kvs([1-[a,b,c], 2-[d,e]]) ).
%  kvs(Kvs), kvs_replace_v_or_insert(Kvs,2,Old,New,NewKvs), 
%  append( Old, [f], New ).
%  New = [d, e, f],
%  NewKvs = [1-[a, b, c], 2-[d, e, f]].
% 
%  kvs(Kvs), kvs_replace_v_or_insert(Kvs,3,Old,New,NewKvs), 
%==
%
% @author nicos angelopoulos
% @version  0.2 2014/6/10, added  doc. changed null value to []
%
% kvs_replace_v_or_insert( [], Key, OldVal, HVal, NxKvs ) :- fail.
kvs_replace_v_or_insert( List, Key, Old, New, NwKvs ) :-
     kvs_replace_v_or_insert( List, Key, [], Old, New, NwKvs ).

kvs_replace_v_or_insert( [], Key, Null, Old, New, NwKvs ) :-
	Old = Null,
	NwKvs = [Key-New].
kvs_replace_v_or_insert( [Hk-Hv|T], Key, Null, Old, New, NwKvs ) :-
	compare( Op, Hk, Key ),
	kvs_replace_v_or_insert_1( Op, Hk, Hv, T, Key, Null, Old, New, NwKvs ).

kvs_replace_v_or_insert_1( =, _InK, InV, T, Key, _Null, Old, New, NwKvs ) :-
	InV = Old,
	NwKvs = [Key-New|T].
kvs_replace_v_or_insert_1( <, InK, InV, T, Key, Null, Old, New, NwKvs ) :-
	NwKvs = [InK-InV|TNwKvs],
	kvs_replace_v_or_insert( T, Key, Null, Old, New, TNwKvs ).
kvs_replace_v_or_insert_1( >, InK, InV, T, Key, Null, Null, New, NwKvs ) :-
	NwKvs = [Key-New,InK-InV|T].
