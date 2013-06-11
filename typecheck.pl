% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Look for types, manipulate env
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

% One function may have different signatures! do not stop on the first match!
% We cant use assocs here, dont allow multiple binds ...

search_env(Key, env([pair(Key, Value)|_], _), Value). 

search_env(Key, env([_|Rest], Inner), Value) :-
    search_env(Key, env(Rest, Inner), Value).

search_env(Key, env([], Inner), Value) :-
    search_env(Key, Inner, Value).


% This one appends a new bind to the given env
add_env(Key, Value, env(Binds, Inner), env([pair(Key, Value)|Binds], Inner)).

% This one creates a new env with a key-value
add_env(Key, Value, env([pair(Key, Value)], nil)).


wrap_env(A, env([], A)).

unwrap_env(env(_, A), A).

% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Our types
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

type(t_integer).
type(t_float).
type(t_list(type(A))) :-
    type(A).
type(t_function(type(A), type(B))) :-
    type(A), type(B).

type(t_maybe(type(A))) :-
    type(A).


is_type(type(A)) :- 
    type(A).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some aux. Core env.
% %%%%%%%%%%%%%%%%%%%%%%%%%%%

init_env(A6) :-
    add_env(print, type(t_function(type(t_integer), type(t_integer) ) ), A1),
    add_env(print, type(t_function(type(t_float), type(t_integer) ) ), A1, A2),
% HOLY SHIT IM SMART
    add_env(identity, type(t_function(A, A)), A2, A3),
    add_env(some_fun, type(t_function(type(t_integer), type(t_float) )), A3, A4),
    add_env(map, type(t_function(type(t_function(A, B)), 
		      type(t_function(type(t_list(A)) , type(t_list(B)))) ) ), A4, A5),
    add_env(car, type(t_function(type(t_list(H)), H)), A5, A6).


% %%%%%%%%%%%%%%%%%%%%%%%%%%%
% Type Checking
% %%%%%%%%%%%%%%%%%%%%%%%%%%%


type_of_callable(type(t_function(_, _))).
fun_arg(type(t_function(Arg, _)), Arg).
fun_ret(type(t_function(_, Ret)), Ret).

type_of_app(T, [], T).
type_of_app(type(t_function(Arg, Ret)), [Arg|Rest], T) :-
    type_of_app(Ret, Rest, T).

% Type-check of expressions

% maybe
type_of_w_env(Env, Env, nothing, type(t_maybe(type(_)))).
type_of_w_env(Env, Env, just(Exp), type(t_maybe(type(A)))) :- type_of_w_env(Env, Env, Exp, type(A)).

% Scalars
type_of_w_env(Env, Env, int(_), type(t_integer) ).
type_of_w_env(Env, Env, float(_), type(t_float) ).

% List
type_of_w_env(Env, Env, list([]), type(t_list(_)) ).
type_of_w_env(Env, Env, list([Head|Tail]), type(t_list(A)) ) :-
    type_of_w_env(Env, Env, Head, A), 
    type_of_w_env(Env, _, list(Tail), type(t_list(A)) ).

% Search the env
type_of_w_env(Env, Env, id(Id), A) :-
    search_env(Id, Env, A).

type_of_w_env(Env, Env, cast_to(A, T), T) :-
    type_of_w_env(Env, Env, A, T).

% Function application
type_of_w_env(Env, Env, app( [ Target | Args ] ), A) :-

    % check type of target (callable?)

    type_of_w_env(Env, _, Target, TypeOfTarget),
    type_of_callable(TypeOfTarget),

    % check type of args with whats in TypeOfTarget

%    fun_arg(TypeOfTarget, TypeOfTargetArgs),
    type_of_seq(Env, _, Args, TypeOfArgs),
    %maplist(=, TypeOfTargetArgs, TypeOfArgs),
    % (...)
    type_of_app(TypeOfTarget, TypeOfArgs, A).

    % A is the third member of TypeOfTarget

    %fun_ret(TypeOfTarget, A).

% Assignment - for lambdas, lets ...
type_of_w_env(Env, Env, new_var(Var_name, Expr, Cont), T) :-
    type_of_w_env(Env, _, Expr, TypeExpr),
    wrap_env(Env, Wenv),
    add_env(Var_name, TypeExpr, Wenv, NewEnv),
    type_of_seq(NewEnv, _, Cont, TypeCont),
    last(TypeCont, T).

type_of_w_env(Env, Env, lambda([var(Type, Var_name)], Cont), T) :-
    wrap_env(Env, Wenv),
    add_env(Var_name, Type, Wenv, NewEnv),
    type_of_seq(NewEnv, _, Cont, TypeCont),
    last(TypeCont, ReturnType),
    T = type(t_function(Type, ReturnType)).

type_of_w_env(Env, Env, lambda([var(Var_name)], Cont), T) :-
    wrap_env(Env, Wenv),
    add_env(Var_name, type(A), Wenv, NewEnv),
    type_of_seq(NewEnv, _, Cont, TypeCont),
    last(TypeCont, ReturnType),
    T = type(t_function(type(A), ReturnType)).


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

% will fail
test( 0, A)  :- type_of( [list( [int(1), string(2)])], A).
test( 1, A)  :- type_of( [list( [int(1), int(2)])], A).
test( 2, A)  :- type_of( [list( [int(1)])], A).
test( 3, A)  :- type_of( [int(1)], A).
test( 4, A)  :- type_of( [float(1.0)], A).
% will fail
test( 5, A)  :- type_of( [list( [int(1), float(2)])], A).
test( 6, A)  :- type_of( [app([id(print), int(1)])], A).
test( 7, A)  :- type_of( [app([id(print), float(1)])], A).
test( 8, A)  :- type_of( [app([id(identity), int(1)])], A).
test( 9, A)  :- type_of( [app([id(identity), float(1)])], A).
test(10, A)  :- type_of( [new_var( a, int(1), [id(a)])], A).
test(11, A)  :- type_of( [app([id(map), id(some_fun), list([int(1)])]) ], A).
test(12, A)  :- type_of( [lambda( [var(a)], [app( [id(print), id(a)] )])], A).
test(13, A)  :- type_of( [lambda( [var(type(t_integer), a)], [id(a)])], A).
test(14, A)  :- type_of( [lambda( [var(a)], [id(a)])], A).
test(15, A)  :- type_of( [app([lambda([var(a)], [id(a)]) , int(1)])], A).
test(16, A)  :- type_of( [app([id(identity), id(print)])], A).
test(17, A)  :- 
    init_env(E), 
    type_of_w_env(E, E, 
		  cast_to(id(print), 
			  type(t_function(type(t_integer), type(t_integer)))), A).
test(18, A)  :-
    init_env(E), 
    type_of_w_env(E, E, 
		  cast_to(id(identity), 
			  type(t_function(type(t_integer), type(t_integer)))), A).

% will fail
test(19, A)  :-
    init_env(E), 
    type_of_w_env(E, E, 
		  cast_to(id(identity), 
			  type(t_function(type(t_float), type(t_integer)))), A).


test(20, T)  :-
    init_env(Env), type_of_w_env(Env, Env, nothing, T).

test(21, T)  :-
    init_env(Env), type_of_w_env(Env, Env, cast_to(nothing, type(t_maybe(type(t_integer)))), T).

test(22, T)  :-
    init_env(Env), type_of_w_env(Env, Env, just(int(1)), T).
