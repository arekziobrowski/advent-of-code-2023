:- use_module(library(readutil)).

:- dynamic(color/3).


read_file_lines(File, Lines) :-
    read_file_to_string(File, String, []),
    split_string(String, '\n', '', Lines).

process_game_strings([]).
process_game_strings([GameString|Rest]) :-
    process_game_string(GameString),
    process_game_strings(Rest).

process_game_string(GameString) :-
    split_string(GameString, ':', '', [GameNumberPart|[EntriesPart|_]]),
    atomic_list_concat(GameEntries, ';', EntriesPart),
    game_number(GameNumberPart, GameNumber),
    process_game_entries(GameEntries, GameNumber).

process_game_entries([], _).
process_game_entries([GameEntriesString|RestGameEntriesStrings], GameNumber) :-
    atomic_list_concat(GameEntries, ',', GameEntriesString),
    assert_game_facts(GameEntries, GameNumber),
    process_game_entries(RestGameEntriesStrings, GameNumber).

assert_game_facts([], _).
assert_game_facts([GameEntry|RestEntries], GameNumber) :-
    normalize_space(atom(NormalizedGameEntry), GameEntry),
    atomic_list_concat([CountAtom, Color|_], ' ', NormalizedGameEntry),
    atom_number(CountAtom, Count),
    assertz(color(GameNumber, Color, Count)),
    assert_game_facts(RestEntries, GameNumber).

game_number(GameNumberString, Number) :- 
    string_concat('Game ', NumberString, GameNumberString),
    atom_number(NumberString, Number).

% running part 1:
% ?- read_file_lines('input.txt', Lines), process_game_strings(Lines), part1(Lines, Out).
part1_not_matching(GameNumber) :-
    color(GameNumber, red, RedCount),
    color(GameNumber, green, GreenCount),
    color(GameNumber, blue, BlueCount),
    (
        RedCount > 12;
        GreenCount > 13;
        BlueCount > 14
    ).

part1(Lines, Out) :-
    length(Lines, InLength),
    sum(InLength, Sum),
    setof(X, part1_not_matching(X), Set),
    sum_list(Set, NotMatchingSum),
    Out is Sum - NotMatchingSum.

sum(0, 0). 
sum(N, Sum) :-
    N > 0,
    N1 is N - 1,
    sum(N1, Sum1),
    Sum is Sum1 + N.

sum_list([], 0).
sum_list([Head|Tail], Sum) :-
    sum_list(Tail, SumTail),
    Sum is Head + SumTail.
