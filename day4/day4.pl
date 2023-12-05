:- use_module(library(readutil)).
:- dynamic(scratchcard_copy/2).
:- dynamic(scratchcard_def/2).

read_file_lines(File, Lines) :-
    read_file_to_string(File, String, []),
    split_string(String, '\n', '', Lines).


scratchcard(Line, CardNumber, Winning, NumbersHad) :-
    split_string(Line, ':', ' ', [CardNumberString | [ Numbers | []]]),
    split_string(CardNumberString,  ' ', ' ', [ _ | [ CardNumberPartString | _]]),
    atom_number(CardNumberPartString, CardNumber),
    split_string(Numbers, '|', ' ', Scratchcard),
    append([WinningString], [ NumbersHadString | []], Scratchcard),
    normalize_space(string(WinningStringNormalized), WinningString),
    atomic_list_concat(WinningAtoms, ' ', WinningStringNormalized),
    maplist(atom_number, WinningAtoms, Winning),
    normalize_space(string(NumbersHadStringNormalized), NumbersHadString),
    atomic_list_concat(NumbersHadAtoms, ' ', NumbersHadStringNormalized),
    maplist(atom_number, NumbersHadAtoms, NumbersHad).

% running part1:
% ?- read_file_lines('input.txt', L), part1(L, 0, Result).
part1([], Result, Result).
part1([Line | T], Acc, Result) :-
    scratchcard(Line, _, Winning, NumbersHad),
    intersection(Winning, NumbersHad, Intersection),
    intersection_score(Intersection, Score),
    AccNew is Acc + Score,
    part1(T, AccNew, Result).

intersection_score([], 0) :- !.
intersection_score(L, Score) :-
    length(L, Num),
    Score is 2 ** (Num - 1).

% running part2:
% ?- read_file_lines('input.txt', L), load_facts(L), part2(L, Out).

load_facts([]).
load_facts([Line | T]) :-
    scratchcard(Line, CardNumber, _, _),
    assertz(scratchcard_def(Line, CardNumber)),
    load_facts(T).

part2([], Result, Result) :- !.
part2([Line | T], Acc, Result) :-
    % process current card.
    scratchcard(Line, CardNumber, Winning, NumbersHad),
    intersection(Winning, NumbersHad, Intersection),
    length(Intersection, NumNextCopies), % we know how many next cards are copied by the current card.
    findall(scratchcard_copy(CardNumber, _), scratchcard_copy(CardNumber, _), Bag),
    length(Bag, NumCopies), % we know how many copies of the current card we have.
    TimesToCopy is NumCopies + 1, % we copy next cards for each copy and original card.
    copy_times(CardNumber, NumNextCopies, TimesToCopy),
    AccNew is Acc + 1 + NumCopies,
    part2(T, AccNew, Result).

copy_times(_, _, 0) :- !.
copy_times(CardNumber, NumNextCopies, Times) :- 
    copy(CardNumber, NumNextCopies),
    NewTimes is Times - 1,
    copy_times(CardNumber, NumNextCopies, NewTimes).

copy(CardNumber, NextN) :-
    findall(scratchcard_copy(Number, CardNumber),
        (
            To is CardNumber+NextN,
            From is CardNumber+1,
            between(From, To, Number)
        ),
        Copies
    ),
    maplist(assertz, Copies).
