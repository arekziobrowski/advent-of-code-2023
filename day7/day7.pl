:- use_module(library(readutil)).

read_file_lines(File, Tuples) :-
    read_file_to_string(File, String, []),
    string_lines(String, Lines),
    maplist(parse_file, Lines, Tuples).

parse_file(Line, [Card, BidNumber]) :- 
    atomic_list_concat([Card, Bid], ' ', Line),
    atom_number(Bid, BidNumber).

% running part1:
% ?- read_file_lines('input.txt', Tuples), part1(Tuples, Out).

part1(Tuples, Out) :-
    predsort(stronger_hand, Tuples, Sorted),
    sum_and_multiply_bids(Sorted, 1, 0, Out).

sum_and_multiply_bids([], _, Out, Out).
sum_and_multiply_bids([[_, Bid] | T], Multiplier, Acc, Out) :-
    NewAcc is Acc + (Bid * Multiplier),
    NewMultiplier is Multiplier + 1,
    sum_and_multiply_bids(T, NewMultiplier, NewAcc, Out).

stronger_hand(>, [H1, _],[H2, _]) :- stronger_hand_type(>, H1, H2).
stronger_hand(<, [H1, _],[H2, _]) :- stronger_hand_type(<, H1, H2).

stronger_hand_type(>, H1, H2) :-
    map_hands_to_type(H1, H2, T1, T2),
    (T1 = T2 -> stronger_card_order(>, H1, H2) ; T1>T2).

stronger_hand_type(<, H1, H2) :-
    map_hands_to_type(H1, H2, T1, T2),
    (T1 = T2 -> stronger_card_order(<, H1, H2) ; T1<T2).

map_hands_to_type(H1, H2, I1, I2) :-
    Strength=['high_card', 'one_pair', 'two_pair', 'three_of_a_kind', 'full_house', 'four_of_a_kind', 'five_of_a_kind'],
    map_hand_to_type(H1, T1),
    map_hand_to_type(H2, T2),
    nth0(I1, Strength, T1),
    nth0(I2, Strength, T2).

map_hand_to_type(H, T) :-
    char_occurrences(H, Occurences),
    type(Occurences, T), !.

stronger_card_order(<, H1, H2) :-
    first_different_elem_strength(H1, H2, I1, I2),
    I1 =< I2,
    !.

stronger_card_order(>, H1, H2) :-
    first_different_elem_strength(H1, H2, I1, I2),
    I1 > I2,
    !.

first_different_elem_strength(H1, H2, I1, I2) :-
    Strength=['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'],
    atom_chars(H1, L1),
    atom_chars(H2, L2),
    first_different_elem(L1, L2, C1, C2),
    nth0(I1, Strength, C1),
    nth0(I2, Strength, C2).

first_different_elem([X|_], [Y|_], X, Y) :- X \= Y, !.
first_different_elem([_|T1], [_|T2], E1, E2) :-
    first_different_elem(T1, T2, E1, E2).

char_occurrences(String, CharOccurrences) :-
    string_chars(String, Chars),
    msort(Chars, Sorted),
    char_occurrences_helper(Sorted, CharOccurrences), !.

char_occurrences_helper([], []).
char_occurrences_helper([Char | Rest], [ (Char, Count) | Tail]) :-
    count_char(Char, Rest, 1, Count, Remaining),
    char_occurrences_helper(Remaining, Tail).

count_char(_, [], Count, Count, []).
count_char(Char, [Char | Rest], Acc, Count, Remaining) :-
    NewAcc is Acc + 1,
    count_char(Char, Rest, NewAcc, Count, Remaining).
count_char(Char, [Other | Rest], Count, Count, [Other | Rest]) :-
    Char \= Other.

type(Occurences, 'five_of_a_kind') :- length(Occurences, 1), member((_, 5), Occurences).
type(Occurences, 'four_of_a_kind') :- length(Occurences, 2), member((_, 4), Occurences).
type(Occurences, 'full_house') :- length(Occurences, 2), member((_, 3), Occurences), member((_, 2), Occurences).
type(Occurences, 'three_of_a_kind') :- length(Occurences, 3), member((_, 3), Occurences).
type(Occurences, 'two_pair') :- length(Occurences, 3), findall(X, member((X, 2), Occurences), L), length(L, 2).
type(Occurences, 'one_pair') :- length(Occurences, 4), member((_, 2), Occurences).
type(Occurences, 'high_card') :- length(Occurences, 5).