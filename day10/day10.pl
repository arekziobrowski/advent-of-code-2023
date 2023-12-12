:- use_module(library(readutil)).
:- dynamic(vertex/3).

read_file_to_graph_facts(File) :-
    read_file_to_string(File, String, []),
    string_lines(String, Lines),
    maplist(string_chars, Lines, Chars),
    chars_to_vertex_facts(Chars, 0).

chars_to_vertex_facts([], _).
chars_to_vertex_facts([Chars | Tail], Line) :-
    findall(vertex(Line, Idx, Char), (length(Chars, Len), between(0, Len, Idx), nth0(Idx, Chars, Char)), Bag),
    maplist(assertz, Bag),
    NewLine is Line + 1,
    chars_to_vertex_facts(Tail, NewLine).

% running part 1:
% ?- read_file_to_graph_facts('input.txt'), part1(Out, Loop).
part1(Out, H) :-
    vertex(I, J, 'S'),
    findall(Path, (
        member(Replacement, ['-', '|', 'J', 'L', 'F', '7']),
        retract(vertex(I, J, 'S')),
        asserta(vertex(I, J, Replacement)),
        (walk(I, J, I, J, [], Path) -> true ; asserta(vertex(I, J, 'S')), fail)
    ), [H|_]),
    length(H, Len),
    Out is Len / 2.

% running part 1:
% ?- read_file_to_graph_facts('input.txt'), part2(Out).
part2(Out) :-
    vertex(I, J, 'S'),
    findall(Path, (
        member(Replacement, ['-', '|', 'J', 'L', 'F', '7']),
        retract(vertex(I, J, 'S')),
        asserta(vertex(I, J, Replacement)),
        (walk(I, J, I, J, [], Path) -> true ; asserta(vertex(I, J, 'S')), retract(vertex(I, J, Replacement)), fail)
    ), [H|_]),
    findall(BetweenNested, (between(0, 150, Row), findall(vertex(X, Y, Z), (X=Row, vertex(X, Y, Z)), Vertices), count_between(Vertices, H, 0, [], BetweenNested)), Bag),
    flatten(Bag, Flat),
    length(Flat, Out).

count_between([], _, _, Out, Out).
count_between([vertex(X, Y, C)|T], Loop, CountVertical, Acc, Out) :-
    (
        member(vertex(X, Y, C), Loop) -> 
            (member(C, ['|', 'J', 'L']) -> NewCount is CountVertical + 1 ; NewCount is CountVertical),
            NewAcc=Acc
        ; 
            NewCount is CountVertical,
            (1 is NewCount mod 2 -> append([vertex(X, Y, C)], Acc, NewAcc) ; NewAcc=Acc)
    ),
    count_between(T, Loop, NewCount, NewAcc, Out).
    
walk(StartRow, StartCol, EndRow, EndCol, Acc, Path) :- 
    vertex(StartRow, StartCol, Char),
    append([vertex(StartRow, StartCol, Char)], Acc, NewAcc),
    adjacent(StartRow, StartCol, Char, [vertex(R1, C1, Char1), vertex(R2, C2, Char2)]),
    (
        member(vertex(R1, C1, Char1), Acc) -> 
            (
                member(vertex(R2, C2, Char2), Acc) -> Path=NewAcc ; walk(R2, C2, EndRow, EndCol, NewAcc, Path) 
            )
        ; 
            walk(R1, C1, EndRow, EndCol, NewAcc, Path)
    ).

adjacent(Row, Col, 'L', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row, NewCol1 is Col + 1,
    NewRow2 is Row - 1, NewCol2 is Col,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').

adjacent(Row, Col, '|', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row - 1, NewCol1 is Col,
    NewRow2 is Row + 1, NewCol2 is Col,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').

adjacent(Row, Col, '-', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row, NewCol1 is Col +1,
    NewRow2 is Row, NewCol2 is Col - 1,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').

adjacent(Row, Col, 'J', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row, NewCol1 is Col - 1,
    NewRow2 is Row - 1, NewCol2 is Col,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').

adjacent(Row, Col, 'F', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row, NewCol1 is Col + 1,
    NewRow2 is Row + 1, NewCol2 is Col,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').

adjacent(Row, Col, '7', [vertex(NewRow1, NewCol1, Char1), vertex(NewRow2, NewCol2, Char2)]) :-
    NewRow1 is Row, NewCol1 is Col - 1,
    NewRow2 is Row + 1, NewCol2 is Col,
    vertex(NewRow1, NewCol1, Char1),
    dif(Char1, '.'),
    vertex(NewRow2, NewCol2, Char2),
    dif(Char2, '.').
