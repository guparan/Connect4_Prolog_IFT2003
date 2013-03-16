%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Connect 4 game in Prolog
%%% AI : minimax algorithm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

The following conventions are used in this program...

Single letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
H - the head of a list
T - the tail of a list
P - a player number (1 or 2)
B - the board (a 7 item list of 6 item lists <=> 6x7 matrix)
    each case on the board can contain one of 2 values: x or o
C - the index of a column on the board (1 - 9)
M - a mark on a case (x or o)
U - the utility value of a board position
R - a random number
D - the depth of the minimax search tree (for outputting utility values, and for debugging)

Variables with a numeric suffix represent a variable based on another variable.
(e.g. B2 is a new board position based on B)

For predicates, the last variable is usually the "return" value.
(e.g. opponent_mark(P,M), returns the opposing mark in variable M)

Predicates with a numeric suffix represent a "nested" predicate.

e.g. myrule2(...) is meant to be called from myrule(...) 
     and myrule3(...) is meant to be called from myrule2(...)


There are only two assertions that are used in this implementation

asserta( board(B) ) - the current board 
asserta( player(P, Type) ) - indicates which players are human/computer.

*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%

next_player(1, 2).      %%% determines the next player after the given player
next_player(2, 1).

inverse_mark('x', 'o'). %%% determines the opposite of the given mark
inverse_mark('o', 'x').

player_mark(1, 'x').    %%% the mark for the given player
player_mark(2, 'o').    

opponent_mark(1, 'o').  %%% shorthand for the inverse mark of the given player
opponent_mark(2, 'x').

blank_mark('e').        %%% the mark used in an empty square

maximizing('x').        %%% the player playing x is always trying to maximize the utility of the board position
minimizing('o').        %%% the player playing o is always trying to minimize the utility of the board position

corner_square(1, 1).    %%% map corner squares to board squares
corner_square(2, 3).
corner_square(3, 7).
corner_square(4, 9).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% A quoi ca sert ..?    
lancerIA :- jouerIA([[],[],[],[],[],[],[]]).

% Lancement du jeu : grille de départ de 6*7 (vide). C'est le joueur 'o' qui commence, suivi par x, jusqu'à ce que l'un des deux gagne [ou GRILLE PLEINE]
jouer:- jouerCoupO([[],[],[],[],[],[],[]]).


run :-
    hello,          %%% Display welcome message, initialize game
    play(1),        %%% Play the game starting with player 1
    goodbye         %%% Display end of game message
    .

run :-
    goodbye
    .


hello :-
    initialize,
%    cls,
    nl,
    nl,
    nl,
    write('Welcome to Connect 4.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
%     blank_mark(E),
    asserta( board([[],[],[],[],[],[],[]]) )
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B),
    retract(board(_)),
    retract(player(_,_)),
    read_play_again(V), !,
    (V == 'Y' ; V == 'y'), 
    !,
    run
    .

read_play_again(V) :-
    nl,
    nl,
    write('Play again (Y/N)? '),
    read(V),
    (V == 'y' ; V == 'Y' ; V == 'n' ; V == 'N'), !
    .

read_play_again(V) :-
    nl,
    nl,
    write('Please enter Y or N.'),
    read_play_again(V)
    .


read_players :-
    nl,
    nl,
    write('Number of human players? '),
    read(N),
    set_players(N)
    .

set_players(0) :- 
    asserta( player(1, computer) ),
    asserta( player(2, computer) ), !
    .

set_players(1) :-
    nl,
    write('Is human playing X or O (X moves first)? '),
    read(M),
    human_playing(M), !
    .

set_players(2) :- 
    asserta( player(1, human) ),
    asserta( player(2, human) ), !
    .

set_players(N) :-
    nl,
    write('Please enter 0, 1, or 2.'),
    read_players
    .


human_playing(M) :- 
    (M == 'x' ; M == 'X'),
    asserta( player(1, human) ),
    asserta( player(2, computer) ), 
    !.

human_playing(M) :- 
    (M == 'o' ; M == 'O'),
    asserta( player(1, computer) ),
    asserta( player(2, human) ),
    !.

human_playing(M) :-
    nl,
    write('Please enter X or O.'),
    set_players(1)
    .


play(P) :-
    board(B), !,
    output_board(B), !,
    not(game_over(P, B)), !,
    make_move(P, B), !,
    next_player(P, P2), !,
    play(P2), !
    .

   
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%      LIST PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Adds an item at the end of a list
addToColumn(V, [], [V]).
addToColumn(V, [H|T1], [H|T2]) :- addToColumn(V,T1,T2).

% Gives the last item of a list
lastItem([], _).
lastItem(L, V) :- last(L, V).

% Searches a sub-list in a list L
/* Parametres : C sub-list, L list */
prefix(P, L) :- append(P, _, L).
sublist(C, L) :- prefix(C, L).
sublist(C, [_|T]) :- sublist(C, T).

% Gives the Nth element of a list
/* Parametres : N index of the element, L list, V element returned */
nthElem(N, L, []) :- length(L, N1), N1 < N.
nthElem(N, L, V) :- nth1(N, L, V).                            


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%         MOVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if we can play in a given column 
is_playable([L|_], 1) :- length(L, N), N<6.
is_playable([_|B], C) :- C > 1, C < 8, C1 is C-1, is_playable(B, C1).

% Remplit L avec les indices de toutes les colonnes jouables dans la board B
available_columns(B, L) :- findall(C, is_playable(B, C), L).

% Applies a move on the given board
% Adds a mark M in the board B at the column C and returns the new board B2
move([L|B], 1, M, B2) :- addToColumn(M,L,L2), B2=[L2|B].
move([L|B], C, M, [L|B2]) :- C > 0, C < 8, C1 is C-1, move(B, C1, M, B2).


% %........................
% % make_move
% %........................
% % requests next move from human/computer, 
% % then applies that move to the given board
% %
% 
make_move(P, B) :-
    player(P, Type),			% recuperation du type du joueur p (human ou computer)

    make_move2(Type, P, B, B2),	% demande d'un coup dans une nouvelle board

    retract( board(_) ),	% remplacement de la board precedente
    asserta( board(B2) )	% par la nouvelle board
    .
% 
% % Demande d'un coup a un humain
make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Player '),
    write(P),
    write(' move? '),
    read(C),

    blank_mark(E),		% definition de E a la valeur de la blank_mark (voir les predicats, blank_mark = 'e')
    is_playable(B, C),	    % verification de la disponibilite de la colonne demandee
    player_mark(P, M),		% recuperation de la marque M du joueur P
    move(B, C, M, B2), !	% realisation du coup 
    .
% 
% Fonction executee si la precedente echoue : l'utilisateur a entre un nombre invalide / la case est prise ou n'existe pas
make_move2(human, P, B, B2) :-
    nl,
    nl,
    write('Error : Please select a numbered column.'),		% Message d'erreur
    make_move2(human, P, B, B2)				% reexecution de la fonction precedente
    .

% Demande d'un coup a l'ordinateur
make_move2(computer, P, B, B2) :-
    nl,
    nl,
    write('Computer is thinking about next move...'),
    player_mark(P, M),		% recuperation de la marque M du joueur P
    minimax(4, B, M, C, U),	% calcul de la position C a jouer avec M
    move(B,C,M,B2),		% enregistrement du coup 

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(C),
    write('.')
    .
    

%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B,L) :-
    not(win(B,x)),                	%%% if either player already won, then there are no available moves
    not(win(B,o)),
    available_columns(B, L),
    L \= []
    .
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%         GAME OVER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% determines when the game is over
%
game_over(P, B) :-
    game_over2(P, B)
    .

game_over2(P, B) :-
    opponent_mark(P, M),   %%% game is over if opponent wins
    win(B, M)
    .

game_over2(P, B) :-
    not(is_playable(B, C))     %%% game is over if board is full
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%       WIN CONDITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% win_move(B, C, M) with B the board, C the column and M the mark (x or o).
% Return Yes if the move let the player win, No if not.
% Checks only the column and the line concerned by the move
% PRECOND. : must be called after the move
win_move(B, C, M) :- win_move_column(B, C, M), !.
win_move(B, C, M) :- line_played(B, C, N), win_move_line(B, N, M).

% Win condition (column) : 4 pieces of the same color (x or o) in a column
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


win(B, M) :- % Controle si la marque M a gagne dans la grille B (l'utilise-t-on vraiment ?)
    % ... -> TODO fonction qui check toute la board sans connaitre le dernier pion ajoute
    win_column(B, M); 
    win_line(B, M)
    .
    
% Win condition (column) : 4 pieces of the same color (x or o) in a row
% B board, M mark                                                                         
win_column([L|_], M):- sublist([M,M,M,M], L),!.
win_column([_|B], M):- win_column(B, M).

% Win condition (line) : 4 pieces of the same color (x or o) in a row
% B board, N index of the first line to check, M mark
win_line(N, B, M):- maplist(nthElem(N), B, L), sublist([M,M,M,M],L),!.
win_line(N, B, M):- N > 0, N1 is N-1, win_line(N1, B, M).
win_line(B, M):- win_line(6, B, M). 

% Writes a message to announce the winner P
% winner(P ) :- write('The player '), write(P ), write(' has won !').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    ARTIFICIAL INTELLIGENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% utility  Determine qui est le gagnant d'une grille pleine (appelee par minimax quand la grille est pleine)
% 
% Il faut faire en sorte de se passer de cette fonction. On ne veut pas tester la victoire sur une board pleine mais plutot a chaque coup
%.......................................
% determines the value of a given board position

utility(B,U) :-
    win(B,'x'),		% si les 'x' gagnent
    U = 1,		% alors U vaudra 1
    !
    .

utility(B,U) :-		% SINON (n'est execute que si la precedente a echoue)
    win(B,'o'),		% si les 'o' gagnent
    U = (-1), 		% alors U vaudra -1
    !
    .

utility(B,U) :-		% SINON (n'est execute que si la precedente a echoue)
    U = 0		% U vaudra 0
    .


%.......................................
% minimax - TODO : A revoir, il faudrait gerer la profondeur max
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

% On connait Dmax : profondeur max de recherche
% On connait D : profondeur actuelle de recherche
% On connait B : board de recherche
% On connait M : mark du joueur ('x' ou 'o')
% On cherche a determiner C, la meilleure position a jouer
% On cherche a determiner U, la meilleure evaluation qu'on a trouve, celle qui correspond a C

minimax(Dmax, B, M, C, U) :- minimax(Dmax, 0, B, M, C, U). % launcher

minimax(Dmax, D, [[],[],[],[],[],[],[]], M, C, U) :-   % Si la board est vide
%     blank_mark(E),		% toutes les cases matchent la blank_mark
    random_int_1n(7, C),	% On choisit une position a jouer au hasard
    !
    .

minimax(Dmax, D, B, M, C, U) :-	% SINON (la board n'est pas vide)
    D < Dmax,
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,C,U),  %%% recursively determine the best available move
    !
    .

% A la fin de la simulation de coups, la board est pleine
% TODO : ne pas avoir a attendre que la board soit pleine
minimax(Dmax, D, B, M, C, U) :-
    utility(B,U)      	% on retourne alors l'evaluation de la board
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list ( [C1] )
best(D,B,M,[C1],C,U) :-		
    move(B,C1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2), 	% recuperation de la mark de l'adversaire de M dans M2
    !,  
    minimax(D,B2,M2,_C,U),  %%% then recursively search for the utility value of that move. (???)
    C = C1, !,
    output_value(D,C,U),
    !
    .

% if there is more than one move in the list ( [C1|T] )
best(D,B,M,[C1|T],C,U) :-
    move(B,C1,M,B2),			%%% apply the first move (in the list) to the board,
    inverse_mark(M,M2), 		% recuperation de la mark de l'adversaire de M dans M2
    !,
    minimax(D,B2,M2,_C,U1),		%%% recursively search for the utility value of that move,
    best(D,B,M,T,C2,U2),		%%% determine the best move of the remaining moves,
    output_value(D,C1,U1),      
    better(D,M,C1,U1,C2,U2,C,U)	%%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,C1,U1,C2,U2,     C,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    C = C1,
    U = U1,
    !
    .

better(D,M,C1,U1,C2,U2,     C,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    C = C1,
    U = U1, 
    !
    .

better(D,M,C1,U1,C2,U2,     C,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(8,R),               %%% then pick one of them at random
    better2(D,R,M,C1,U1,C2,U2,C,U),    
    !
    .

better(D,M,C1,U1,C2,U2,     C,U) :-        %%% otherwise, second move is better
    C = C2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,C1,U1,C2,U2,  C,U) :-
    R < 6,
    C = C1,
    U = U1, 
    !
    .

better2(D,R,M,C1,U1,C2,U2,  C,U) :-
    C = C2,
    U = U2,
    !
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 		  OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

output_players :- 
    nl,
    player(1, V1),
    write('Player 1 is '),   %%% either human or computer
    write(V1),

    nl,
    player(2, V2),
    write('Player 2 is '),   %%% either human or computer
    write(V2), 
    !
    .


output_winner(B) :-
    win(B,x),
    write('X wins.'),
    !
    .

output_winner(B) :-
    win(B,o),
    write('O wins.'),
    !
    .

output_winner(B) :-
    write('No winner.')
    .

output_value(D,C,U) :-
    D == 1,
    nl,
    write('Column '),
    write(C),
    write(', utility: '),
    write(U), !
    .

output_value(D,C,U) :- % If previous fails, do not stop the program
    true
    .
    
    
% ..............................
%
% Fonctions de puissance4.prolog
% ..............................
afficherGrille(_,0).                                                       
afficherGrille(G, N):- 
    N > 0, 
    N1 is N-1, 
    maplist(nthElem(N), G, L), 
    afficherListe(L),
    write('\n'), 
    afficherGrille(G, N1)
    .
    
output_board(B):- afficherGrille(B,6).
 
afficherListe([]):- write('|').
afficherListe([E|L]):-  write('|'), afficherElement(E), afficherListe(L).

afficherElement([]):- write(' '),!.
afficherElement(E):- write(E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%      PSEUDO-RANDOM NUMBERS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%.......................................
% random_seed
%.......................................
% Initialize the random number generator...
% If no seed is provided, use the current time
%

random_seed :-
    random_seed(_),
    !
    .

random_seed(N) :-
    nonvar(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .

random_seed(N) :-
    var(N),
% Do nothing, SWI-Prolog does not support seeding the random number generator
    !
    .


%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
