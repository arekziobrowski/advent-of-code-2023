:- use_module(library(readutil)).

read_file_lines(File, Tuples) :-
    read_file_to_string(File, String, []),
    string_lines(String, [ Times | [ Distances | [] ]]),
    split_string(Times, ' ', ' ', [ _ | TimeEntries]),
    split_string(Distances, ' ', ' ', [_ | DistanceEntries]),
    maplist(atom_number, TimeEntries, TimeNumbers),
    maplist(atom_number, DistanceEntries, DistanceNumbers),
    zip(TimeNumbers, DistanceNumbers, Tuples).

zip([], [], []).
zip([X|Xs], [Y|Ys], [[X, Y]|Zs]) :- zip(Xs,Ys,Zs).

% running part1:
% ?- read_file_lines(File, Tuples), part1(Tuples, 1, Out).

part1([], Out, Out).
part1([[Time, Distance] | T], Acc, Out) :-
    findall(HoldFactor, (between(0, Time, HoldFactor), distance(Time, HoldFactor, ToBeat), ToBeat > Distance), L),
    length(L, NumberOfWaysToBeatRecord),
    NewAcc is Acc * NumberOfWaysToBeatRecord,
    part1(T, NewAcc, Out).


distance(Time, HoldFactor, Distance) :-
    Distance is (Time - HoldFactor) * HoldFactor.