
/*
  This file is a recursive descent parser of DCG grammars
  stored using the predicate '--->'/2.

  Call like this:
  ?- parse(command, [take, the, white, ball], Tree).
  Tree = take(basic_entity(the, object(ball, -, white))) ;
  no (more) solutions

  ...or like this:
  ?- parse_all(command, [take, the, white, ball], Trees).
  Trees = [take(basic_entity(the, object(ball, -, white)))]
*/

%% parse_all(+Startcat : atom, +Sentence : list(atom), -ParseTrees : list(term))
parse_all(Cat, Tokens, Trees) :-
    findall(T, parse(Cat, Tokens, T), Trees).

%% parse_all(+Startcat : atom, +Sentence : list(atom), ?ParseTree : term)
parse(Cat, Tokens, Tree) :-
    parse_term(Cat:Tree, Tokens, []).

%% parse_term(?ParsingGoal : term, +Sentence : list(atom), ?Remainder : list(atom))
parse_term(LHS, Xs0, Xs) :-
    '--->'(LHS, RHS),
    parse_term(RHS, Xs0, Xs).
parse_term([], Xs, Xs).
parse_term([T|Ts], [T|Xs0], Xs) :-
    parse_term(Ts, Xs0, Xs).
parse_term((A, B), Xs0, Xs) :-
    parse_term(A, Xs0, Xs1),
    parse_term(B, Xs1, Xs).
parse_term((A ; B), Xs0, Xs) :-
    ( parse_term(A, Xs0, Xs)
    ; parse_term(B, Xs0, Xs)
    ).
parse_term({Goal}, Xs, Xs) :-
    call(Goal).
