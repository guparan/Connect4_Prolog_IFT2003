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
% win_move(B, C, P) with B the board, C the column and P the player (x or o).
% Return Yes if the move let the player win, No if not.
% Checks only the column and the line concerned by the move
win_move(B, C, P) :- win_move_column(B, C, P), !.
win_move(B, C, P) :- line_played(B, C, N), win_move_line(B, N, P).

% Win condition (column) : 4 pieces of the same color (x or o) in a row
% B board, C column to test, P player
win_move_column([L|_], 1, P) :- sublist([P,P,P,P], L), !.
win_move_column([_|B], C, P) :- C1 is C-1, win_move_column(B, C1, P).

% B board, C column played and N number of the line in which the piece goes
% Notice : the bottom line is the 1st and the top line is the 6th
line_played([L|_], 1, N) :- length(L, N).
line_played([_|B], C, N) :- C1 is C-1, line_played(B, C1, N).

% Win condition (line) : 4 pieces of the same color (x or o) in a row
% B board, N number of the line, P player
win_move_line(B, N, P) :- maplist(nthElem(N), B, L), sublist([P,P,P,P], L).
