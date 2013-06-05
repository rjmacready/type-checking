
% One function may have different signatures! do not stop on the first match!
% We cant use assocs here, dont allow multiple binds ...

search-env(Key, env([[Key, Value]|_], _), Value).

search-env(Key, env([_|Rest], Inner), Value) :-
    search-env(Key, env(Rest, Inner), Value).

search-env(Key, env([], Inner), Value) :-
    search-env(Key, Inner, Value).


% This one appends a new bind to the given env
add-env(Key, Value, env(Binds, Inner), env([[Key, Value]|Binds], Inner)).

% This one creates a new env with a key-value
add-env(Key, Value, env([[Key, Value]], nil)).


wrap-env(A, env([], A)).

unwrap-env(env(_, A), A).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Our types
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

type(integer).
type(float).
type(list(type(A))) :-
    type(A).
type(function(A, B)) :-
    type_star(A), type_plus(B).

type_plus([type(_)]).
type_plus([type(_)|Rest]) :-
    type_plus(Rest).

type_star([]).
type_star([type(_)|Rest]) :-
    list_of_types(Rest).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%

init-env(A5) :-
%    add-env("print", [function, [integer], [integer]], A1),
%    add-env("print", [function, [float], [integer]], A1, A2),
%    add-env("identity", [function, [integer], [integer]], A2, A3), % 
%    add-env("identity", [function, [float], [float]], A3, A4).
% HOLY SHIT IM SMART
%    add-env("identity", [function, [A], [A]], A2, A3),
    add-env("some-fun", [function, [integer], [float]], A4), % 3, A4),
    add-env("map", [function, [[function, [A], [B]], list(A)], [list(B)]], A4, A5).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Basis
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% Type-check of expressions
type-of-w-env(Env, Env, [int, _], integer).
type-of-w-env(Env, Env, [float, _], float).
% coerce integers to floats, if need be
% type-of-w-env(Env, Env, [int, _], float).

type-of-w-env(Env, Env, [list, []], list(A)).
type-of-w-env(Env, Env, [list, [Head]], list(A)) :- type-of-w-env(Env, _, Head, A).
type-of-w-env(Env, Env, [list, [Head|Tail]], list(A)) :-
    type-of-w-env(Env, Env, Head, A), type-of-w-env(Env, _, [list, Tail], list(A)).

type-of-w-env(Env, Env, [id, Id], A) :-
    search-env(Id, Env, A).

type-of-callable([function|_]).
args([function, Args, _], Args).
ret([function, _, Ret], Ret).

type-of-w-env(Env, Env, [application, Target | Args ], A) :-
    % check type of target (callable?)
    type-of-w-env(Env, _, Target, TypeOfTarget),
    type-of-callable(TypeOfTarget),

    % check type of args with whats in TypeOfTarget
    args(TypeOfTarget, TypeOfTargetArgs),

    type-of-seq(Env, _, Args, TypeOfArgs),

    % match(TypeOfArgs, _),
    maplist(=, TypeOfTargetArgs, TypeOfArgs),

    % A is the third member of TypeOfTarget
    ret(TypeOfTarget, A).

type-of-w-env(Env, Env, [new_var, Var_name, Expr, Cont], T) :-
    type-of-w-env(Env, _, Expr, TypeExpr),
    wrap-env(Env, Wenv),
    add-env(Var_name, TypeExpr, Wenv, NewEnv),
    type-of-seq(NewEnv, _, Cont, TypeCont),
    last(TypeCont, T).



% Type-check a sequence of expressions
type-of-seq(_, _, [], []).
type-of-seq(Env, NewEnv, [First|Rest], [TypeH|Type]) :-
    type-of-w-env(Env, NewEnv, First, TypeH),
    type-of-seq(NewEnv, _, Rest, Type).


% Entry point.
% Type-check a program
type-of(Stuff, Type) :-
    init-env(A4),
    type-of-seq(A4, _, Stuff, Type).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some tests
% %%%%%%%%%%%%%%%%%%%%%%%%%%%


test( 0, A) :- type-of([[list, [[int, 1], [string, 2]]]], A).
test( 1, A) :- type-of([[list, [[int, 1], [int, 2]]]], A).
test( 2, A) :- type-of([[list, [[int, 1]]]], A).
test( 3, A) :- type-of([[int, 1]], A).
test( 4, A) :- type-of([[float, 1.0]], A).
test( 5, A) :- type-of([[list, [[int, 1], [float, 2]]]], A).
test( 6, A) :- type-of([[application, [id, "print"], [int, 1]]], A).
test( 7, A) :- type-of([[application, [id, "print"], [float, 1]]], A).
test( 8, A) :- type-of([[application, [id, "identity"], [int, 1]]], A).
test( 9, A) :- type-of([[application, [id, "identity"], [float, 1]]], A).
test(10, A) :- type-of([[new_var, "a", [int, 1], [[id, "a"]]]], A).
test(11, A) :- type-of([ [application, [id, "map"], [id, "some-fun"], [list, [[int, 1]]]] ], A).

