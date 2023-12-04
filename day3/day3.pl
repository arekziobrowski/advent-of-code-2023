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
        AdjColumn is Column-1, symbol(AdjSymbol, Line, AdjColumn), not(char_type(AdjSymbol, digit))
    ), Left),
    findall(AdjSymbol, (
        AdjColumn is Column+1, symbol(AdjSymbol, Line, AdjColumn), not(char_type(AdjSymbol, digit))
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
    remove_duplicates(Set, Dedup),
    sum_results(Dedup, 0, Out).

sum_results([], Result, Result).
sum_results([symbol(_, Line, Col)|T], Acc, Result) :-
    number_index_of(Line, Col, Number),
    AccNew is Acc + Number,
    sum_results(T, AccNew, Result). 

remove_duplicates(Facts, Result) :-
    findall(Symbol, (
        member(Symbol, Facts),
        \+ (member(OtherSymbol, Facts), Symbol \== OtherSymbol,
            Symbol = symbol(_, Line, Col1),
            OtherSymbol = symbol(_, Line, Col2),
            succ(Col1, Col2))
    ), Result).

number_index_of(Row, Col, Out) :-
    symbol(Char, Row, Col),
    char_type(Char, digit),
    once(all_left(Row, Col, [], Left)),
    once(all_right(Row, Col, [], Right)),
    reverse(Left, LeftReversed),
    append(LeftReversed, [Char], Temp),
    append(Temp, Right, OutList),
    atomic_list_concat(OutList, Conc),
    atom_number(Conc, Out).

all_left(Row, Col, Acc, Result) :-
    PrevCol is Col - 1,
    char_type(Char, digit),
    symbol(Char, Row, PrevCol),
    append(Acc, [Char], NewAcc),
    all_left(Row, PrevCol, NewAcc, Result).
all_left(_, _, Acc, Acc).

all_right(Row, Col, Acc, Result) :-
    NextCol is Col + 1,
    char_type(Char, digit),
    symbol(Char, Row, NextCol),
    append(Acc, [Char], NewAcc),
    all_right(Row, NextCol, NewAcc, Result).
all_right(_, _, Acc, Acc).

% running part 2:
% ?- read_file_facts('input.txt'), part2(Out).
part2(Out) :-
    setof(symbol(Char, Line, Column), (
        between(0, 139, Line),
        between(0, 139, Column),
        Char='*',
        symbol(Char, Line, Column)
    ), SuspectedGears),
    findall(Ratio, (member(symbol(_, Y, Z), SuspectedGears), once(gear_ratio(Y, Z, Ratio))), Ratios),
    sum_list(Ratios, Out).

gear_ratio(Line, Column, Ratio) :-
    adjacent_gear_symbols(Line, Column, AdjacentSymbols),
    length(AdjacentSymbols, 2),
    append([symbol(_, L1, C1)], [symbol(_, L2, C2)], AdjacentSymbols),
    number_index_of(L1, C1, N1),
    number_index_of(L2, C2, N2),
    Ratio is N1 * N2.

adjacent_gear_symbols(Line, Column, Out) :-
    findall(symbol(AdjSymbol, AdjLine, Column), (
        AdjLine is Line-1, char_type(AdjSymbol, digit),symbol(AdjSymbol, AdjLine, Column)
    ), Down),
    findall(symbol(AdjSymbol, AdjLine, Column), (
        AdjLine is Line+1, char_type(AdjSymbol, digit), symbol(AdjSymbol, AdjLine, Column)
    ), Up),
    findall(symbol(AdjSymbol, Line, AdjColumn), (
        AdjColumn is Column-1, char_type(AdjSymbol, digit), symbol(AdjSymbol, Line, AdjColumn)
    ), Left),
    findall(symbol(AdjSymbol, Line, AdjColumn), (
        AdjColumn is Column+1, char_type(AdjSymbol, digit), symbol(AdjSymbol, Line, AdjColumn)
    ), Right),
    findall(symbol(AdjSymbol, AdjLine, AdjColumn), (
        AdjColumn is Column-1, AdjLine is Line-1, char_type(AdjSymbol, digit),symbol(AdjSymbol, AdjLine, AdjColumn)
    ), LeftBottom),
    findall(symbol(AdjSymbol, AdjLine, AdjColumn), (
        AdjColumn is Column-1, AdjLine is Line+1, char_type(AdjSymbol, digit), symbol(AdjSymbol, AdjLine, AdjColumn)
    ), LeftUp),
    findall(symbol(AdjSymbol, AdjLine, AdjColumn), (
        AdjColumn is Column+1, AdjLine is Line-1, char_type(AdjSymbol, digit),symbol(AdjSymbol, AdjLine, AdjColumn)
    ), RightBottom),
    findall(symbol(AdjSymbol, AdjLine, AdjColumn), (
        AdjColumn is Column+1, AdjLine is Line+1, char_type(AdjSymbol, digit), symbol(AdjSymbol, AdjLine, AdjColumn)
    ), RightUp),
    append([Down, Up, Left, Right, LeftBottom, LeftUp, RightBottom, RightUp], AdjacentSymbols),
    remove_duplicates(AdjacentSymbols, Out),
    !.