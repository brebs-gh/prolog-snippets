% https://adventofcode.com/2023/day/12
% https://swi-prolog.discourse.group/t/advent-of-code-2023/7025/119

% Not needed
%:- use_module(library(dcg/basics)).

:- set_prolog_flag(stack_limit, 3_647_483_648).
% For tabling
:- set_prolog_flag(table_space, 2_000_000_000).

advent_solve(File, Part1, Part2) :-
	once(phrase_from_file(clumped_dam_lines(LCs), File)),
	part1(LCs, Part1),
	part2(LCs, Part2).

part1(LCs, Part) :-
	aggregate_all(
		sum(Combs),
		(	member(L-C, LCs),
			% g (i.e. good, vs damaged) is a convenient, neutral end-of-previous
			clumped_dam_combs_sum(L, C, g, Combs)
		),
		Part
	).

part2(LCs, Part) :-
	aggregate_all(
		sum(Combs),
		(	member(L-C, LCs),
			part2_unfold(L, C, UFL, UFC),
			clumped_dam_combs_sum(UFL, UFC, g, Combs)
		),
		Part
	).

clumped_dam_lines([]) --> [].
clumped_dam_lines([L-C|T]) -->
	clumped_dam_line(L, C),
	clumped_dam_lines(T).

clumped_dam_line(L, C) -->
	spring_list(L),
	" ",
	damaged_count_list(C),
	"\n".

spring_list([]) --> [].
spring_list([d|T]) -->
	"#",
	spring_list(T).
spring_list([g|T]) -->
	".",
	spring_list(T).
spring_list([u|T]) -->
	"?",
	spring_list(T).

damaged_count_list([]) --> [].
damaged_count_list([I|T]) -->
	int(I),
	damaged_count_list_next(T).

damaged_count_list_next([]) --> [].
damaged_count_list_next(L) -->
	",",
	damaged_count_list(L).

int(0) --> "0".
int(I) -->
	[D],
	{ between(0'1, 0'9, D) },
	int_(Ds),
	{ number_codes(I, [D|Ds]) }.

int_([]) --> [].
int_([H|T]) -->
	[H],
	{ between(0'0, 0'9, H) },
	int_(T).

part2_unfold(L, C, UFL, UFC) :-
	append([L, [u], L, [u], L, [u], L, [u], L], UFL),
	append([C, C, C, C, C], UFC).

% Tabling needs L to be ground, hence using 'u' instead of var
:- table clumped_dam_combs_sum/4.
clumped_dam_combs_sum(L, SL, P, Combs) :-
	aggregate_all(sum(Combs), clumped_dam_combs(L, SL, P, Combs), Combs).

clumped_dam_combs([], [], _P, 1).
clumped_dam_combs([g|T], SL, _P, Combs) :-
	% Using tabling here (is quicker to *not* use it in the other predicates below)
	clumped_dam_combs_sum(T, SL, g, Combs).
clumped_dam_combs([d|T], [SL|R], g, Combs) :-
	clumped_populate_d(T, SL, T0),
	clumped_dam_combs(T0, R, d, Combs).
clumped_dam_combs([u|T], SL, P, Combs) :-
	% Ground parameters help the tabling
	member(E, [d, g]),
	clumped_dam_combs([E|T], SL, P, Combs).

clumped_populate_d(T, 1, T).
clumped_populate_d([E|T], I, R) :-
	I @> 1,
	member(E, [d, u]),
	I0 is I - 1,
	clumped_populate_d(T, I0, R).
