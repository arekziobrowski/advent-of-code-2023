:- use_module(library(readutil)).

read_file_lines(File, Lines) :-
    read_file_to_string(File, String, []),
    split_string(String, '\n', '', Lines).

% running part1:
% ?- read_file_lines('input.txt', L), part1(L, 0, Result).
part1([], Result, Result).
part1([Line | T], Acc, Result) :-
    scratchcard(Line, Winning, NumbersHad),
    intersection(Winning, NumbersHad, Intersection),
    intersection_score(Intersection, Score),
    AccNew is Acc + Score,
    part1(T, AccNew, Result).

intersection_score([], 0) :- !.
intersection_score(L, Score) :-
    length(L, Num),
    Score is 2 ** (Num - 1).

scratchcard(Line, Winning, NumbersHad) :-
    split_string(Line, ':', ' ', [_ | [ Numbers | []]]),
    split_string(Numbers, '|', ' ', Scratchcard),
    append([WinningString], [ NumbersHadString | []], Scratchcard),
    normalize_space(string(WinningStringNormalized), WinningString),
    atomic_list_concat(WinningAtoms, ' ', WinningStringNormalized),
    maplist(atom_number, WinningAtoms, Winning),
    normalize_space(string(NumbersHadStringNormalized), NumbersHadString),
    atomic_list_concat(NumbersHadAtoms, ' ', NumbersHadStringNormalized),
    maplist(atom_number, NumbersHadAtoms, NumbersHad).
