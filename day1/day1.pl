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

% Running part 1:
% ?- read_file_lines('input.txt', Codes), part1(Result, Codes).
part1(0, []).
part1(Result, [Codes|RestCodes]) :-
    part1(RestSum, RestCodes),
    first_digit(First, Codes),
    last_digit(Last, Codes),
    write(First),
    write(Last),
    nl,
    Result is (First*10+Last) + RestSum.

% Running part 2:
% ?- read_file_lines('input.txt', Codes), part2(Result, Codes).

map_digit([111, 110, 101], [49]). % list for 'one' in ASCII.
map_digit([116, 119, 111], [50]). % list for 'two' in ASCII.
map_digit([116, 104, 114, 101, 101], [51]). % list for 'three' in ASCII.
map_digit([102, 111, 117, 114], [52]). % list for 'four' in ASCII.
map_digit([102, 105, 118, 101], [53]). % list for 'five' in ASCII.
map_digit([115, 105, 120], [54]). % list for 'six' in ASCII.
map_digit([115, 101, 118, 101, 110], [55]). % list for 'seven' in ASCII.
map_digit([101, 105, 103, 104, 116], [56]). % list for 'eight' in ASCII.
map_digit([110, 105, 110, 101], [57]). % list for 'nine' in ASCII.
map_digit([H|_], H).

map_digit_reversed([101, 110, 111], [49]). % list for 'neo' in ASCII.
map_digit_reversed([111, 119, 116], [50]). % list for 'owt' in ASCII.
map_digit_reversed([101, 101, 114, 104, 116], [51]). % list for 'eerth' in ASCII.
map_digit_reversed([114, 117, 111, 102], [52]). % list for 'rouf' in ASCII.
map_digit_reversed([101, 118, 105, 102], [53]). % list for 'evif' in ASCII.
map_digit_reversed([120, 105, 115], [54]). % list for 'xis' in ASCII.
map_digit_reversed([110, 101, 118, 101, 115], [55]). % list for 'neves' in ASCII.
map_digit_reversed([116, 104, 103, 105, 101], [56]). % list for 'thgie' in ASCII.
map_digit_reversed([101, 110, 105, 110], [57]). % list for 'enin' in ASCII.
map_digit_reversed([H|_], H).

replace([], Acc, Acc).
replace([Code|RestCodes], Acc, Result) :-
    map_digit(Pattern, Replace),
    append(Pattern, Rest, [Code|RestCodes]),
    replace(Rest, [Acc|Replace], Result).

replace_reverse([], Acc, Acc).
replace_reverse([Code|RestCodes], Acc, Result) :-
    map_digit_reversed(Pattern, Replace),
    append(Pattern, Rest, [Code|RestCodes]),
    replace_reverse(Rest, [Acc|Replace], Result).

replace_word_codes(Codes, Result) :-
    replace(Codes, [], Inflated),
    flatten(Inflated, Flat),
    reverse(Codes, ReversedCodes),
    replace_reverse(ReversedCodes, [], InflatedReversed),
    flatten(InflatedReversed, FlatReversed),
    reverse(FlatReversed, FlatReversedBack),
    append(Flat, FlatReversedBack, Result).
    

part2(Result, Codes) :-
    maplist(replace_word_codes, Codes, Replaced),
    part1(Result, Replaced).