% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Look for types, manipulate env
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% One function may have different signatures! do not stop on the first match!
% We cant use assocs here, dont allow multiple binds ...

search_env(Key, env([[Key, Value]|_], _), Value).

search_env(Key, env([_|Rest], Inner), Value) :-
    search_env(Key, env(Rest, Inner), Value).

search_env(Key, env([], Inner), Value) :-
    search_env(Key, Inner, Value).


% This one appends a new bind to the given env
add_env(Key, Value, env(Binds, Inner), env([[Key, Value]|Binds], Inner)).

% This one creates a new env with a key-value
add_env(Key, Value, env([[Key, Value]], nil)).


wrap_env(A, env([], A)).

unwrap_env(env(_, A), A).

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
% Some aux. Core env.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

init_env(A5) :-
    add_env(print, function([type(integer)], [type(integer)]), A1),
    add_env(print, function([type(float)], [type(integer)]), A1, A2),
% HOLY SHIT IM SMART
    add_env(identity, function([A], [A]), A2, A3),
    add_env(some_fun, function([type(integer)], [type(float)]), A3, A4),
    add_env(map, function([function([A], [B]), type(list(A)) ], [type(list(B))]) , A4, A5).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type Checking
% %%%%%%%%%%%%%%%%%%%%%%%%%%%


type_of_callable(function(_, _)).
fun_args(function(Args, _), Args).
fun_ret(function(_, Ret), Ret).


% Type-check of expressions

% Scalars
type_of_w_env(Env, Env, [int, _], type(integer) ).
type_of_w_env(Env, Env, [float, _], type(float) ).

% List
type_of_w_env(Env, Env, [list, []], type(list(_)) ).
type_of_w_env(Env, Env, [list, [Head|Tail]], type(list(A)) ) :-
    type_of_w_env(Env, Env, Head, A), 
    type_of_w_env(Env, _, [list, Tail], type(list(A)) ).

% Search the env
type_of_w_env(Env, Env, [id, Id], A) :-
    search_env(Id, Env, A).

% Function application
type_of_w_env(Env, Env, [application, Target | Args ], A) :-
    % check type of target (callable?)
    type_of_w_env(Env, _, Target, TypeOfTarget),
    type_of_callable(TypeOfTarget),

    % check type of args with whats in TypeOfTarget
    fun_args(TypeOfTarget, TypeOfTargetArgs),

    type_of_seq(Env, _, Args, TypeOfArgs),

    % match(TypeOfArgs, _),
    maplist(=, TypeOfTargetArgs, TypeOfArgs),

    % A is the third member of TypeOfTarget
    fun_ret(TypeOfTarget, A).

% Assignment - for lambdas ...
type_of_w_env(Env, Env, [new_var, Var_name, Expr, Cont], T) :-
    type_of_w_env(Env, _, Expr, TypeExpr),
    wrap_env(Env, Wenv),
    add_env(Var_name, TypeExpr, Wenv, NewEnv),
    type_of_seq(NewEnv, _, Cont, TypeCont),
    last(TypeCont, T).


% Type-check a sequence of expressions
type_of_seq(_, _, [], []).
type_of_seq(Env, NewEnv, [First|Rest], [TypeH|Type]) :-
    type_of_w_env(Env, NewEnv, First, TypeH),
    type_of_seq(NewEnv, _, Rest, Type).


% Entry point.
% Type-check a program
type_of(Stuff, Type) :-
    init_env(A4),
    type_of_seq(A4, _, Stuff, Type).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some tests
% %%%%%%%%%%%%%%%%%%%%%%%%%%%


test( 0, A) :- type_of([[list, [[int, 1], [string, 2]]]], A).
test( 1, A) :- type_of([[list, [[int, 1], [int, 2]]]], A).
test( 2, A) :- type_of([[list, [[int, 1]]]], A).
test( 3, A) :- type_of([[int, 1]], A).
test( 4, A) :- type_of([[float, 1.0]], A).
test( 5, A) :- type_of([[list, [[int, 1], [float, 2]]]], A).
test( 6, A) :- type_of([[application, [id, print], [int, 1]]], A).
test( 7, A) :- type_of([[application, [id, print], [float, 1]]], A).
test( 8, A) :- type_of([[application, [id, identity], [int, 1]]], A).
test( 9, A) :- type_of([[application, [id, identity], [float, 1]]], A).
test(10, A) :- type_of([[new_var, a, [int, 1], [[id, a]]]], A).
test(11, A) :- type_of([ [application, [id, map], [id, some_fun], [list, [[int, 1]]]] ], A).

