% lib_prolog.pl

% (1) mergesort

merge([], List, List).
merge(List, [], List).
merge([X|Xs], [Y|Ys], [X|Result]) :-
    X =< Y,
    merge(Xs, [Y|Ys], Result).
merge([X|Xs], [Y|Ys], [Y|Result]) :-
    X > Y,
    merge([X|Xs], Ys, Result).

split([], [], []).
split([X], [X], []).
split([X,Y|Xs], [X|X1], [Y|Y1]) :-
    split(Xs, X1, Y1).

mergesort([], []).
mergesort([X], [X]).
mergesort(List, Sorted) :-
    List = [_|_],
    split(List, List1, List2),
    mergesort(List1, Sorted1),
    mergesort(List2, Sorted2),
    merge(Sorted1, Sorted2, Sorted).

% (2) de

gcd(A, 0, A, 1, 0).
gcd(A, B, G, X, Y) :-
    B \= 0,
    Q is A // B,
    R is A mod B,
    gcd(B, R, G, X1, Y1),
    X is Y1,
    Y is X1 - Q * Y1.

de(A, B, X, Y, G) :-
    gcd(A, B, G, X, Y).

% (3) prime_factors

is_prime(2).
is_prime(3).
is_prime(N) :-
    N > 3,
    N mod 2 =\= 0,
    \+ has_factor(N, 3).

has_factor(N, F) :-
    N mod F =:= 0.
has_factor(N, F) :-
    F * F < N,
    F2 is F + 2,
    has_factor(N, F2).

smallest_prime_factor(N, F) :-
    smallest_prime_factor(N, F, 2).

smallest_prime_factor(N, N, _) :- is_prime(N).
smallest_prime_factor(N, F, D) :-
    D * D =< N,
    (N mod D =:= 0 -> F = D ; D1 is D + 1, smallest_prime_factor(N, F, D1)).

prime_factors(1, []).
prime_factors(N, [F|Factors]) :-
    N > 1,
    smallest_prime_factor(N, F),
    N1 is N // F,
    prime_factors(N1, Factors).

% (4) totient

gcd(A, 0, A).
gcd(A, B, G) :-
    B \= 0,
    R is A mod B,
    gcd(B, R, G).

totient(1, 1).
totient(N, T) :-
    N > 1,
    totient(N, N, 0, T).

totient(_, 0, Acc, Acc).
totient(N, I, Acc, T) :-
    I > 0,
    gcd(N, I, 1),
    NewAcc is Acc + 1,
    NewI is I - 1,
    totient(N, NewI, NewAcc, T).
totient(N, I, Acc, T) :-
    I > 0,
    \+ gcd(N, I, 1),
    NewI is I - 1,
    totient(N, NewI, Acc, T).

% (5) primes

sieve([], []).
sieve([P|Xs], [P|Ys]) :-
    exclude(multiple_of(P), Xs, Zs),
    sieve(Zs, Ys).

multiple_of(P, X) :-
    X mod P =:= 0.

numlist(2, N, List) :- findall(X, between(2, N, X), List).

primes(N, Primes) :-
    N >= 2,
    numlist(2, N, List),
    sieve(List, Primes).

test :-

    mergesort([4, 2, 5, 3, 1], SortedList1),
    write('mergesort([4, 2, 5, 3, 1], SortedList) = '), write(SortedList1), nl,
    mergesort([13, 1, 20, 11, 15, 9, 6, 5, 16, 18, 3, 4, 17, 14, 12, 2, 8, 19, 7, 10], SortedList2),
    write('mergesort([13, 1, 20, 11, 15, 9, 6, 5, 16, 18, 3, 4, 17, 14, 12, 2, 8, 19, 7, 10], SortedList) = '), write(SortedList2), nl,
    
    de(10, 15, X1, Y1, G1),
    write('de(10, 15, X1, Y1, G1) = '), write((X1, Y1, G1)), nl,
    de(123, 72, X2, Y2, G2),
    write('de(123, 72, X2, Y2, G2) = '), write((X2, Y2, G2)), nl,

    prime_factors(100, Factors1),
    write('prime_factors(100, Factors) = '), write(Factors1), nl,
    prime_factors(185130, Factors2),
    write('prime_factors(185130, Factors) = '), write(Factors2), nl,
    
    totient(10, TotientValue1),
    write('totient(10, TotientValue) = '), write(TotientValue1), nl,
    totient(100, TotientValue2),
    write('totient(100, TotientValue) = '), write(TotientValue2), nl,
    
    primes(10, Primes1),
    write('primes(10, Primes) = '), write(Primes1), nl,
    primes(100, Primes2),
    write('primes(100, Primes) = '), write(Primes2), nl.
