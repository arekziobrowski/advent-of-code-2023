:- use_module(library(readutil)).
:- dynamic(mapping/4).
:- dynamic(seed/2).

read_file_lines_and_assertz(File, Seeds) :-
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
    assertz_list(SeedToSoilStrings, "sts"),
    assertz_list(SoilToFertilizerStrings, "stf"),
    assertz_list(FertilizerToWaterStrings, "ftw"),
    assertz_list(WaterToLightStrings, "wtl"),
    assertz_list(LightToTemperatureStrings, "ltt"),
    assertz_list(TemperatureToHumidityStrings, "tth"),
    assertz_list(HumidityToLocationStrings, "htl"),
    !.

empty("").
between_elements(X, Y, List, Elements) :-
    append(_, [X | Rest], List),
    reverse(Rest, RestReversed),
    append(_, [Y | ElementsReversed], RestReversed),
    reverse(ElementsReversed, Elements),
    !.

assertz_list([], _).
assertz_list([String|Rest], Name) :-
    atomic_list_concat(Atoms, ' ', String),
    maplist(atom_number, Atoms, [DestinationRange | [StartRange | [ Step | [] ] ] ]),
    assertz_name_fact(Name, DestinationRange, StartRange, Step),
    assertz_list(Rest, Name).


assertz_name_fact(Name, Destination, Start, Step) :-
    assertz(mapping(Name, Start, Destination, Step)).


% running part 1:
% ?- read_file_lines_and_assertz('input.txt', Seeds), part1(Seeds, Out).

part1(Seeds, Out) :-
    findall(Location, (member(Seed, Seeds), location(Seed, Location)), Bag),
    min_list(Bag, Out).

% running part 2:
% ?- read_file_lines_and_assertz('input.txt', Seeds), part2(Seeds, Out).

part2(Seeds, Out) :-
    % LOOK OUT - BRUTE FORCE BELOW, BECAUSE I GOT TIRED OF PROLOG STUFF.
    inflate_seed_facts(Seeds),
    findall(Max, seed(_, Max), List),
    max_list(List, MaxSearch),
    print(MaxSearch),
    lowest_location_for_seed(Out, MaxSearch).

inflate_seed_facts([]).
inflate_seed_facts([Start | [Range | T]]) :-
    Max is Start + Range -1,
    assertz(seed(Start, Max)),
    inflate_seed_facts(T).

lowest_location_for_seed(Location, Max) :-
    between(0, Max, Location),
    map_reversed("htl", Humidity, Location),
    map_reversed("tth", Temperature, Humidity),
    map_reversed("ltt", Light, Temperature),
    map_reversed("wtl", Water, Light),
    map_reversed("ftw", Fertilizer, Water),
    map_reversed("stf", Soil, Fertilizer),
    map_reversed("sts", Seed, Soil),
    seed(StartSeed, MaxSeed),
    between(StartSeed, MaxSeed, Seed),
    !.

map(Name, Source, Destination) :-
    (
        mapping(Name, From, To, Step), Max is From + Step, between(From, Max, Source) -> Diff is To - From, Destination is Source + Diff ; Destination is Source
    ),
    !.

map_reversed(Name, Source, Destination) :-
    (
        mapping(Name, From, To, Step), Max is To + Step, between(To, Max, Destination) -> Diff is To - From, Source is Destination - Diff ; Source is Destination
    ),
    !.