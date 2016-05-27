#!/usr/bin/swipl -f -q main
:- initialization main.
/**
 * @author Stetson Bost
 * 
 * @tbd This will allow us to take care of surfaces related by Yoshikawa moves.
 *
 */
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
%%    generate subdiagrams 
%%    subtract off diagram times inner product to get a result that's orthogonal
%%    output will be invariant
%%    Find a nonzero

%% Yoshikawa move 1 %%
yoshi1([(1, A, N, A, N+1)]) :-
	[ ] ; [(-1, A, N+1, A, N)] .
yoshi1([(-1, A, N+1, A, N)]) :-
	[ ] ; [(1, A, N, A, N+1)] .
yoshi1([ ]) :-
	[(1, A, N, A, N+1)] ; [(-1, A, N+1, A, N)] .


main:-
  write('Hello World\n'),
  yoshi1([(1,'A', 2, 'A', N)]).