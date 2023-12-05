:- use_module(library(readutil)).
:- dynamic(mapping/3).

read_file_lines_and_asserta(File, Seeds) :-
    read_file_to_string(File, String, []),
    string_lines(String, Lines),
    exclude(empty, Lines, NonEmptyLines),
    append(NonEmptyLines, ["end"], PreparedLines),
    append([SeedsString], Mappings, PreparedLines),
    atom_concat("seeds: ", SeedsNumbersString, SeedsString),
    atomic_list_concat(SeedNumbersList, ' ', SeedsNumbersString),
    maplist(atom_number, SeedNumbersList, Seeds),
    between_elements("seed-to-soil map:", "soil-to-fertilizer map:", Mappings, SeedToSoilStrings),
    between_elements("soil-to-fertilizer map:", "fertilizer-to-water map:", Mappings, SoilToFertilizerStrings),
    between_elements("fertilizer-to-water map:", "water-to-light map:", Mappings, FertilizerToWaterStrings),
    between_elements("water-to-light map:", "light-to-temperature map:", Mappings, WaterToLightStrings),
    between_elements("light-to-temperature map:", "temperature-to-humidity map:", Mappings, LightToTemperatureStrings),
    between_elements("temperature-to-humidity map:", "humidity-to-location map:", Mappings, TemperatureToHumidityStrings),
    between_elements("humidity-to-location map:", "end", Mappings, HumidityToLocationStrings),
    asserta_list(SeedToSoilStrings, "sts"),
    asserta_list(SoilToFertilizerStrings, "stf"),
    asserta_list(FertilizerToWaterStrings, "ftw"),
    asserta_list(WaterToLightStrings, "wtl"),
    asserta_list(LightToTemperatureStrings, "ltt"),
    asserta_list(TemperatureToHumidityStrings, "tth"),
    asserta_list(HumidityToLocationStrings, "htl"),
    !.

empty("").
between_elements(X, Y, List, Elements) :-
    append(_, [X | Rest], List),
    reverse(Rest, RestReversed),
    append(_, [Y | ElementsReversed], RestReversed),
    reverse(ElementsReversed, Elements),
    !.

asserta_list([], _).
asserta_list([String|Rest], Name) :-
    atomic_list_concat(Atoms, ' ', String),
    maplist(atom_number, Atoms, [DestinationRange | [StartRange | [ Step | [] ] ] ]),
    findall(_, asserta_name_fact(Name, DestinationRange, StartRange, Step), _),
    asserta_list(Rest, Name).


asserta_name_fact(Name, DR, SR, Step) :-
    between(0, Step, Inc),
    Destination is DR + Inc,
    Start is SR + Inc,
    asserta(mapping(Name, Start, Destination)).

% running part 1:
% ?- read_file_lines_and_asserta('input.txt', Seeds), part1(Seeds, Out).

part1(Seeds, Out) :-
    findall(Location, (member(Seed, Seeds), location(Seed, Location)), Bag),
    min_list(Bag, Out).


location(Seed, Location) :-
    mapping("sts", Seed, Soil),
    mapping("stf", Soil, Fertilizer),
    mapping("ftw", Fertilizer, Water),
    mapping("wtl", Water, Light),
    mapping("ltt", Light, Temperature),
    mapping("tth", Temperature, Humidity),
    mapping("htl", Humidity, Location),
    !.

mapping("sts", X, X).
mapping("stf", X, X).
mapping("ftw", X, X).
mapping("wtl", X, X).
mapping("ltt", X, X).
mapping("tth", X, X).
mapping("htl", X, X).
