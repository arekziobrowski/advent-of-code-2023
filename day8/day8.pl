:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).

read_file_to_facts(File, Navigation) :-
    read_file_to_string(File, String, []),
    string_lines(String, [Navigation | [_ | Nodes]]),
    assertz_nodes(Nodes),
    !.

assertz_nodes([]).
assertz_nodes([Node | Rest]) :-
    string_codes(Node, Codes), phrase(inside_parenthesis(N,L,R), Codes),
    assertz(node(N, (L, R))),
    assertz_nodes(Rest).

inside_parenthesis(N, L, R) --> 
  string(Ns), b, "=", b, "(", b, string(Ls), b, ",", b, string(Rs), b, ")", 
  { string_codes(N, Ns), string_codes(L, Ls), string_codes(R, Rs) }.
b --> blanks.


navigate(NavigationList, Index, Direction) :-
    length(NavigationList, Len),
    I is Index mod Len,
    nth0(I, NavigationList, Direction).

% running part 1:
% ?- read_file_to_facts('input.txt', Navigation), string_chars(Navigation, NavigationList), part1(NavigationList, "AAA", 0, Out).

% uncomment for part 1.
% part1(_, "ZZZ", Index, Out) :- Out is Index, !.

% uncomment for part 2.
part1(_, Node, Index, Out) :- ends_with(Node, 'Z'), Out is Index, !.
part1(NavigationList, StartNode, NavigationIndex, Out) :-
    node(StartNode, (L, R)),
    navigate(NavigationList, NavigationIndex, Direction),
    NewIndex is NavigationIndex + 1,
    (Direction = 'L' -> part1(NavigationList, L, NewIndex, Out) ; part1(NavigationList, R, NewIndex, Out)).

% running part 2:
% ?- read_file_to_facts('input.txt', Navigation), string_chars(Navigation, NavigationList), part2(NavigationList, Out).
part2(NavigationList, Out) :-
    findall(StartNode, (node(StartNode, _), ends_with(StartNode, 'A')), NodesEndingWithA),
    findall(PathLength, (member(Node, NodesEndingWithA), part1(NavigationList, Node, 0, PathLength)), PathLengths),
    lcm_list(PathLengths, Out).


ends_with(String, Char) :-
    atom_codes(String, StringCodes),
    atom_codes(Char, [CharCode]),
    append(_, [CharCode], StringCodes).

lcm_list([X], X).
lcm_list([X | Xs], LCM) :-
    lcm_list(Xs, TempLCM),
    LCM is lcm(X, TempLCM).