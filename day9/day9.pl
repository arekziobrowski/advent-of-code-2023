:- use_module(library(readutil)).

read_file_to_numbers(File, Out) :-
    read_file_to_string(File, String, []),
    string_lines(String, Lines),
    maplist(string_to_numbers, Lines, Out).

string_to_numbers(String, Numbers) :-
    split_string(String, " ", " ", StrList),
    maplist(atom_number, StrList, Numbers).

% running part1:
% ?- read_file_to_numbers('input.txt', Numbers), part1(Numbers, Out).

part1(Numbers, Out) :- 
    maplist(extrapolate, Numbers, Nexts),
    sum_list(Nexts, Out).

extrapolate(L, Next) :- all_equal(L, 0), Next is 0, !.
extrapolate(L, Next) :-
    differences(L, Differences),
    extrapolate(Differences, NextDiff),
    last(L, Last),
    Next is Last + NextDiff.

differences([_], []).
differences([], []).
differences([X, Y|Rest], [Diff|Diffs]) :-
    Diff is Y - X,
    differences([Y|Rest], Diffs). 

all_equal([], _).
all_equal([X|Xs], X) :-
    all_equal(Xs, X).