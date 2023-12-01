:- use_module(library(readutil)).

read_file_lines(File, Lines) :-
    read_file_to_codes(File, Codes, []),
    codes_to_lines(Codes, Lines).

codes_to_lines([], []).
codes_to_lines(Codes, [Line|RestLines]) :-
    append(Line, [10|RestCodes], Codes), % Assuming newline character (code 10) is used as a line separator
    codes_to_lines(RestCodes, RestLines).

first_digit(0, []).
first_digit(Num, Codes) :-
    member(Code, Codes),
    char_type(Code, digit(Num)),
    !.

last_digit(0, []).
last_digit(Num, Codes) :-
    reverse(Codes, Reversed),
    first_digit(Num, Reversed).

sum(0, []).
sum(Result, [H|Tail]) :-
    sum(Tail, TailSum),
    Result is H + TailSum.

part1(0, []).
part1(Result, [Codes|RestCodes]) :-
    part1(RestSum, RestCodes),
    first_digit(First, Codes),
    last_digit(Last, Codes),
    Result is (First*10+Last) + RestSum.