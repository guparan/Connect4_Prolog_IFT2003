% Returns a sublist from a list
% S sublist, L list
prefix(P,L):-append(P,_,L).
sublist(S,L):-prefix(S,L).
sublist(S,[_|T]):-sublist(S,T).

% Gives the Nth element of a list or [] if the element doesn't exist.
% N index of the element, L list, X element
nthElem(N, L, []):- length(L, N1), N1 < N.
nthElem(N, L, X):- nth1(N, L, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% win_move(B, C, M) with B the board, C the column and M the mark (x or o).
% Return Yes if the move let the player win, No if not.
% Checks only the column and the line concerned by the move
% Precond. : must be called after the move
win_move(B, C, M) :- win_move_column(B, C, M), !.
win_move(B, C, M) :- line_played(B, C, N), win_move_line(B, N, M).

% Win condition (column) : 4 pieces of the same color (x or o) in a row
% B board, C column to test, M mark
win_move_column([L|_], 1, M) :- sublist([M,M,M,M], L), !.
win_move_column([_|B], C, M) :- C1 is C-1, win_move_column(B, C1, M).

% B board, C column played and N index of the line in which the piece went
% Notice : the bottom line is the 1st and the top line is the 6th
line_played([L|_], 1, N) :- length(L, N).
line_played([_|B], C, N) :- C1 is C-1, line_played(B, C1, N).

% Win condition (line) : 4 pieces of the same color (x or o) in a row
% B board, N index of the line, M mark
win_move_line(B, N, M) :- maplist(nthElem(N), B, L), sublist([M,M,M,M], L).
