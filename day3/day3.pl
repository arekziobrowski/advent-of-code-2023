:- use_module(library(readutil)).
:- dynamic(symbol/3).
:- dynamic(place/2).

read_file_facts(File) :-
    read_file_to_codes(File, Codes, []),
    codes_to_facts(Codes, 0, 0).

codes_to_facts([], _, _).
codes_to_facts([H|T], Line, Column) :-
    (
        H = 10 -> % Assuming newline character (code 10) is used as a line separator
            LineNew is Line + 1,
            ColumnNew is 0,
            codes_to_facts(T, LineNew, ColumnNew)
        ;
            char_code(Char, H),
            assertz(symbol(Char, Line, Column)),
            ColumnNew is Column + 1,
            codes_to_facts(T, Line, ColumnNew)
    ).

adjacent_symbols(Line, Column, AdjacentSymbols) :-
    findall(AdjSymbol, (
        AdjLine is Line-1, symbol(AdjSymbol, AdjLine, Column)
    ), Down),
    findall(AdjSymbol, (
        AdjLine is Line+1, symbol(AdjSymbol, AdjLine, Column)
    ), Up),
    findall(AdjSymbol, (
        AdjColumn is Column-1, symbol(AdjSymbol, Line, AdjColumn)
    ), Left),
    findall(AdjSymbol, (
        AdjColumn is Column+1, symbol(AdjSymbol, Line, AdjColumn)
    ), Right),
    findall(AdjSymbol, (
        AdjColumn is Column-1, AdjLine is Line-1, symbol(AdjSymbol, AdjLine, AdjColumn)
    ), LeftBottom),
    findall(AdjSymbol, (
        AdjColumn is Column-1, AdjLine is Line+1, symbol(AdjSymbol, AdjLine, AdjColumn)
    ), LeftUp),
    findall(AdjSymbol, (
        AdjColumn is Column+1, AdjLine is Line-1, symbol(AdjSymbol, AdjLine, AdjColumn)
    ), RightBottom),
    findall(AdjSymbol, (
        AdjColumn is Column+1, AdjLine is Line+1, symbol(AdjSymbol, AdjLine, AdjColumn)
    ), RightUp),
    append([Down, Up, Left, Right, LeftBottom, LeftUp, RightBottom, RightUp], AdjacentSymbols).

has_non_dot_adjacent(Line, Column) :-
    adjacent_symbols(Line, Column, AdjacentSymbols),
    dif(Char, '.'),
    dif(Char, digit),
    member(Char, AdjacentSymbols).

% running part 1:
% ?- read_file_facts('input.txt'), part1(Out).
part1(Out) :-
    setof(symbol(Char, Line, Column), (
        between(0, 139, Line),
        between(0, 139, Column),
        has_non_dot_adjacent(Line, Column),
        symbol(Char, Line, Column),
        char_type(Char, digit)
    ), 
    Set),
    findall(Sum, (between(0, 139, Line), to_sum(Set, Line, Sum)), SumList),
    sum_list(SumList, Out).

to_sum(Set, Line, Sum) :-
    findall(place(Column, Char), member(symbol(Char, Line, Column), Set), Result),
    sort(Result, Sorted),
    print(Sorted),
    once(concatenate_consecutive_chars(Sorted, 0, [], Bag)),
    maplist(atom_number, Bag, IntBag),
    sum_list(IntBag, Sum).

concatenate_consecutive_chars([], _, Acc, Acc).
concatenate_consecutive_chars([place(Num, Char) | T], PrevNum, Acc, Result) :-
    (
        succ(PrevNum, Num) ->
            append_character_to_last(Acc, Char, NewAcc),
            concatenate_consecutive_chars(T, Num, NewAcc, Result)
        ;
            append(Acc, [Char], NewAcc),
            concatenate_consecutive_chars(T, Num, NewAcc, Result)
    ).

append_character_to_last([], _, []).
append_character_to_last([X], Char, [NewLast]) :-
    atom_concat(X, Char, NewLast).
append_character_to_last([Head|Tail], Char, [Head|NewTail]) :-
    append_character_to_last(Tail, Char, NewTail).
