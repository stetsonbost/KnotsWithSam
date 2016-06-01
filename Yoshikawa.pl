#!/usr/bin/swipl -f -q main
:- initialization main.
/**
 * @author Stetson Bost
 * 
 * @tbd This will allow us to take care of surfaces related by Yoshikawa moves.
 *
 */

% some "nice" prolog settings
:- set_prolog_flag( prompt_alternatives_on, groundness ).
:- set_prolog_flag(toplevel_print_options, [quoted(true),
    portray(true), attributes(portray), max_depth(999), priority(699)]).

/*
%% Yoshikawa move 1 %%
yoshi1 [(1, A, N, A, N+1)] :-
	[ ] ; [(-1, A, N+1, A, N)] .
yoshi1 [(-1, A, N+1, A, N)] :-
	[ ] ; [(1, A, N, A, N+1)] .
yoshi1 [ ] :-
	[(1, A, N, A, N+1)] ; [(-1, A, N+1, A, N)] .

%% Yoshikawa move 2 %%
%% Opposite direction parallel strands
yoshi2opp [ ] :-
	[(1, A, N, B, M+1), (-1, A, N+1, B, M)] ;
	[(1, B, M, A, N+1), (-1, B, M+1, A, N)] .
yoshi2opp [(1, A, N, B, M+1), (-1, A, N+1, B, M)] :-
	[ ] ; [(1, B, M, A, N+1), (-1, B, M+1, A, N)] .
yoshi2opp [(1, B, M, A, N+1), (-1, B, M+1, A, N)] :-
	[ ] ;  [(1, A, N, B, M+1), (-1, A, N+1, B, M)].
%% Same direction parallel strands
yoshi2same [ ] :-
	[(1, B, M, A, N), (-1, B, M+1, A, N+1)] ;
	[(-1, A, N, B, M), (1, A, N+1, B, M+1)] .
yoshi2same [(1, B, M, A, N), (-1, B, M+1, A, N+1)] :-
	[ ] ; [(-1, A, N, B, M), (1, A, N+1, B, M+1)] .
yoshi2same [(-1, A, N, B, M), (1, A, N+1, B, M+1)] :-
	[ ] ; [(1, B, M, A, N), (-1, B, M+1, A, N+1)] .
*/



/*
	Generating set

	1. 	[(1,A,Aa,A,Aa+1)] = [ ]
	1'.	[(1,A,Aa+1,A,Aa)] = [ ]
	2.	[(1,A,Aa,B,Bb),(-1,A,Aa+1,B,Bb+1)] = [ ]
	3.	[(1,A,Aa,B,Bb+1),(1,C,Cc,A,Aa+1),(-1,C,Cc+1,B,Bb)] =
			[(1,Aa+1,B,Bb+1),(1,C,Cc+1,A,Aa),(-1,C,Cc,B,Bb)]
	4.	[(0,A,Aa,B,Bb+1),(1,C,Cc,B,Bb),(-1,C,Cc+1,B,Bb+2)] =
			[(0,A,Aa+1,B,Bb),(1,C,Cc,A,Aa+2),(-1,C,Cc+1,A,Aa)]
	4'.	[(0,A,Aa,B,Bb+1),(-1,B,Bb,C,Cc),(1,B,Bb+2,C,Cc+1)] =
		[(0,A,Aa+1,B,Bb),(1,A,Aa,C,Cc+1),(-1,A,Aa+2,C,Cc)]
	5.	[(0,A,Aa+1,B,Bb),(1,B,Bb+1,A,Aa)] = [(0,A,Aa,B,Bb+1),(1,B,Bb,A,Aa+1)]
	6.	[(0,A,Aa,B,Bb)] = []**
			**We require B = component([Bb])
	6'.	[(0,A,Aa,A,Aa+1)] = []
	7.	[(0,A,Aa,C,Cc+1),(0,B,Bb,C,Cc)] = [(0,A,Aa,B,Bb),(0,B,Bb+1,C,Cc)]
	8.	[(1,A,Aa,B,Bb+1),(-1,A,Aa+1,C,Cc+1),(0,A,Aa+2,D,Dd),(1,A,Aa+3,C,Cc+2),(-1,A,Aa+4,B,Bb),(0,B,Bb+2,C,Cc)] =
			[(-1,B,Bb+1,A,Aa),(1,C,Cc+1,A,Aa+1),(0,A,Aa+2,D,Dd),(-1,C,Cc+2,A,Aa+3),(1,B,Bb,A,Aa+4),(0,B,Bb+2,C,Cc)]
*/

%%%%%%%%%%%
%% Rules %%
%%%%%%%%%%%

%% Yoshikawa move 1 %%
yoshi1([ ]) :-
	crossing(1,A,Aa,A,Aa+1) .
yoshi1([(1,A,Aa,A,Aa+1)]) :-
	crossing() .

yoshi1_([ ]) :- 
	crossing(1,A,Aa+1,A,Aa).
yoshi1_([(1,A,Aa+1,A,Aa)]) :-
	crossing() .

%% Yoshikawa move 2 %%
yoshi2([ ]) :- 
	crossing(1,A,Aa,B,Bb), 
	crossing(-1,A,Aa+1,B,Bb+1) .
yoshi2([(1,A,Aa,B,Bb),(-1,A,Aa+1,B,Bb+1)]) :-
	crossing() .

%% Yoshikawa move 3 %%
yoshi3([(1,A,Aa,B,Bb+1),(1,C,Cc,A,Aa+1),(-1,C,Cc+1,B,Bb)]) :-
	crossing(1,Aa+1,B,Bb+1),
	crossing(1,C,Cc+1,A,Aa),
	crossing(-1,C,Cc,B,Bb) .

%%%%%%%%%%%
%% Facts %%
%%%%%%%%%%%

crossing().


main:-
  	write('Determines equivalence of surfaces under Yoshikawa moves.\n').
  	%% yoshi1([(1,'A', 2, 'A', N)]).