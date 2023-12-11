:- use_module(library(readutil)).

read_file_to_graph_facts(File, StartRow, StartColumn) :-
    read_file_to_string(File, String, []),
    string_lines(String, Lines),
    maplist(string_chars, Lines, Chars),
    chars_to_vertex_facts(Chars, 0),
    vertex(StartRow, StartColumn, 'S'), % save starting indexes,
    true. % replace 'S' with matching character.

chars_to_vertex_facts([], _).
chars_to_vertex_facts([Chars | Tail], Line) :-
    findall(vertex(Line, Idx, Char), (length(Chars, Len), between(0, Len, Idx), nth0(Idx, Chars, Char)), Bag),
    maplist(assertz, Bag),
    NewLine is Line + 1,
    chars_to_vertex_facts(Tail, NewLine).

% running part 1:
% ?- read_file_to_graph_facts('input.txt', I, J), part1(Out).
part1(Out) :-
    vertex(I, J, 'S'),
    findall(Path, (
        member(Replacement, ['-', '|', 'J', 'L', 'F', '7']),
        retract(vertex(I, J, 'S')),
        asserta(vertex(I, J, Replacement)),
        (walk(I, J, I, J, [], Path) -> true ; asserta(vertex(I, J, 'S')), fail)
    ), [H|_]),
    length(H, Len),
    Out is Len / 2.

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
