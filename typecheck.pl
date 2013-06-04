

search-env(Key, env([], Inner), Value) :-
    search-env(Key, Inner, Value).

search-env(Key, env([[Key, Value]| _], _), Value).

search-env(Key, env([_|Tail], Inner), Value) :- 
    search-env(Key, env(Tail, Inner), Value).


add-env(Key, Value, env(Binds, Inner), env([[Key, Value]|Binds], Inner)).

add-env(Key, Value, env([[Key, Value]], [])).


wrap-env(A, env([], A)).

unwrap-env(env(_, A), A).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basis
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Type-check of expressions
type-of-w-env(_, _, [int, _], integer).
type-of-w-env(_, _, [float, _], float).
% coerce integers to floats, if need be
type-of-w-env(_, _, [int, _], float).

type-of-w-env(_, _, [list, []], list(A)).
type-of-w-env(Env, _, [list, [Head|[]]], list(A)) :- type-of-w-env(Env, Head, A).
type-of-w-env(Env, _, [list, [Head|Tail]], list(A)) :-
    type-of-w-env(Env, Head, A), type-of-w-env(Env, [list, Tail], list(A)).

type-of-w-env(Env, _, [id, Id], A) :-
    search-env(Id, Env, A).

callable([function|_]).

type-of-w-env(Env, _, [application, target | args ], A) :-
    % check type of target (callable?)
    type-of-w-env(Env, _, target, B),
    callable(B),
    % check type of args
    
    % A is the third member of B

% Type-check a sequence of expressions
type-of-seq(Env, _, [OnlyOne|[]], Type) :-
    type-of-w-env(Env, _, OnlyOne, Type).

type-of-seq(Env, NewEnv, [First|Rest], Type) :-
    type-of-w-env(Env, NewEnv, First, _),
    type-of-seq(NewEnv, _, Rest, Type).

% Entry point.
% Type-check a program
type-of(Stuff, Type) :-
    add-env("print", [function, [integer], [integer]], A),
    type-of-seq(A, _, Stuff, Type).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some tests
% %%%%%%%%%%%%%%%%%%%%%%%%%%%


test(0, A) :- type-of(_, [list, [[int, 1], [string, 2]]], A).
test(1, A) :- type-of(_, [list, [[int, 1], [int, 2]]], A).
test(2, A) :- type-of(_, [list, [[int, 1]]], A).
test(3, A) :- type-of(_, [int, 1], A).
test(4, A) :- type-of(_, [float, 1.0], A).
test(5, A) :- type-of(_, [list, [[int, 1], [float, 2]]], A).
