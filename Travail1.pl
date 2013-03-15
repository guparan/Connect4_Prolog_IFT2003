%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CCT 381 -– Artificial Intelligence
%%% Robert Pinchbeck
%%% Final Project 
%%% Due December 20, 2006
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Prolog Implementation of Tic-Tac-Toe
%%% using the minimax strategy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

The following conventions are used in this program...

Cingle letter variables represent:

L - a list
N - a number, position, index, or counter
V - a value (usually a string)
A - an accumulator
H - the head of a list
T - the tail of a list

For this implementation, these single letter variables represent:

P - a player number (1 or 2)
B - the board (a 9 item list representing a 3x3 matrix)
    each "square" on the board can contain one of 3 values: x ,o, or e (for empty)
C - the number of a square on the board (1 - 9)
M - a mark on a square (x or o)
E - the mark used to represent an empty square ('e').
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     FACTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     MAIN PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    write('Welcome to Tic-Tac-Toe.'),
    read_players,
    output_players
    .

initialize :-
    random_seed,          %%% use current time to initialize random number generator
    blank_mark(E),
%     asserta( board([E,E,E, E,E,E, E,E,E]) )  %%% create a blank board
    asserta( board([[],[],[],[],[],[],[]])
    .

goodbye :-
    board(B),
    nl,
    nl,
    write('Game over: '),
    output_winner(B), % TODO
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

    
    
% ............................
%
% Fonctions de puissance4.pl
% ............................

% Fonction qui permet d'ajouter un élément en fin de liste
addToColumn(X,[],[X]).
addToColumn(X,[Y|L1],[Y|L2]) :- addToColumn(X,L1,L2).

finListe([], _).
finListe(L, E) :- last(L,E).

% Fonction qui renvoie une sous-liste à partir d'une liste L
/* Paramètres : C sous-liste, L liste */
prefix(P,L) :-append(P,_,L).
sublist(C,L) :-prefix(C,L).
sublist(C,[_|T]) :-sublist(C,T).

% Fonction qui renvoie le nième élément d'une liste 
/* Paramètres : N index de l'élement qu'on veut récupérer, L liste, X élément retourné */
nthElem(N, L, []) :- length(L, N1), N1 < N.
nthElem(N, L, X) :- nth1(N, L, X).                               

% Fonction qui declare qu'un joueur a gagne
/* Paramètres : J joueur */
gagnant(J) :-write('Le Joueur '), write(J), write(' a gagné !').

% Fonction qui enregistre un coup joué dans la grille
/* Paramètres : C numéro de la colonne dans laquelle J joue, G grille, J joueur, G' nouvelle grille */          
enregistrerCoup(1, [L|G], x, _, I) :- 
    length(L,C), 
    C >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupX(I)
    .
    
enregistrerCoup(1, [L|G], o, _, I) :- 
    length(L,C), 
    C >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupO(I)
    .
    
enregistrerCoup(1, [L|G], J, F, I) :- 
    length(L,C), 
    C < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoup(C, [L|G], x, _, I) :- 
    C > 7, 
    write('Coup Invalide\n'), 
    jouerCoupX(I)
    .
    
enregistrerCoup(C, [L|G], o, _, I) :-
    C > 7, 
    write('Coup Invalide\n'), 
    jouerCoupO(I)
    .
    
enregistrerCoup(C, [T|X], J, [T|G], I) :- 
    C > 0, 
    C1 is C-1, 
    enregistrerCoup(C1, X, J, G, I)
    .
                                        
enregistrerCoupJoueur(1, [L|G], x, _, I) :- 
    length(L,C), C >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupJoueur(I)
    .
    
enregistrerCoupJoueur(1, [L|G], J, F, I) :- 
    length(L,C), 
    C < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoupJoueur(C, [L|G], x, _, I) :- 
    C > 7, 
    write('Coup Invalide\n'), 
    jouerCoupJoueur(I)
    .
    
enregistrerCoupJoueur(C, [T|X], J, [T|G], I) :-  
    C > 0, 
    C1 is C-1, 
    enregistrerCoupJoueur(C1, X, J, G, I)
    .
    
enregistrerCoupIA(1, [L|G], J, F, I) :- 
    length(L,C), 
    C < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoupIA(C, [T|X], J, [T|G], I) :- 
    C > 0, 
    C1 is C-1, 
    enregistrerCoupIA(C1, X, J, G, I)
    .
 
% Fonctions principales dans le deroulement de la partie
/* Paramètres : G grille*/
jouerCoupX(G) :- finJeu(G,J), gagnant(J),!.
jouerCoupO(G) :- finJeu(G,J), gagnant(J),!.
jouerCoupX(G) :- 
    write('Joueur x, entrez un numéro de colonne : '),
    read(C), 
    enregistrerCoup(C,G, x, X, G),
    afficherGrille(X),
    write('\n'),
    jouerCoupO(X)
    .
jouerCoupO(G) :- 
    write('Joueur o, entrez un numéro de colonne : '),
    read(C), 
    enregistrerCoup(C,G, o, X, G),
    afficherGrille(X),
    write('\n'),
    jouerCoupX(X)
    .
jouerCoupJoueur(G) :- 
    write('Joueur x, entrez un numéro de colonne : '),
    read(C), 
    enregistrerCoupJoueur(C,G, x, X, G),
    afficherGrille(X),
    write('\n'),
    jouerIA(X)
    .
    
    jouerIA(G):-finJeu(G,J), gagnant(J),!.

%Ci un coup permet de gagner il faut le jouer.
jouerIA(G) :- 
    coupGagnant(C,G,o), 
    enregistrerCoupIA(C,G,o,X,G),
    afficherGrille(X),
    write('\n'),
    jouerCoupJoueur(X)
    .

%Ci un coup permet a l'adversaire de gagner on se défend(coup défensif).
jouerIA(G) :- 
    coupGagnant(C,G,x), 
    enregistrerCoupIA(C,G,o,X,G), 
    afficherGrille(X),
    write('\n'),
    jouerCoupJoueur(X)
    .

jouerIA(0, G) :- write('Pas de coup trouvé').

jouerIA(C, G) :- 
    enregistrerCoupIA(C,G,o,X,G),
    afficherGrille(X),
    write('\n'),
    jouerCoupJoueur(X)
    .

espaceRestant(1, [L|G], E, L) :- 
    longueur(L,C2), 
    C3 is 6-C2, 
    E=C3
    .
    
espaceRestant(C, [T|X], E, L) :- 
    C > 0, 
    C1 is C-1, 
    espaceRestant(C1, X, E, L)
    .
                                                                        
% %Ci on a pas de coup immédiat on fait un coup au centre ou au plus près possible pour une victoire possible en verticale. : Inutile
% jouerIA(G):- espaceRestant(4,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(4,G)), jouerIA(4,G).
% jouerIA(G):- espaceRestant(5,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(5,G)), jouerIA(5,G).
% jouerIA(G):- espaceRestant(3,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(3,G)), jouerIA(3,G).
% jouerIA(G):- espaceRestant(6,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(6,G)), jouerIA(6,G).
% jouerIA(G):- espaceRestant(2,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(2,G)), jouerIA(2,G).
% jouerIA(G):- espaceRestant(7,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(7,G)), jouerIA(7,G).
% jouerIA(G):- espaceRestant(1,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(1,G)), jouerIA(1,G).
% 
% %Cinon jouer au plus près du centre quand même. : Inutile
% jouerIA(G):- jouerIA(4,G),not(coupPerdantIA(4,G)).
% jouerIA(G):- jouerIA(5,G),not(coupPerdantIA(5,G)).
% jouerIA(G):- jouerIA(3,G),not(coupPerdantIA(3,G)).
% jouerIA(G):- jouerIA(6,G),not(coupPerdantIA(6,G)).
% jouerIA(G):- jouerIA(2,G),not(coupPerdantIA(2,G)).
% jouerIA(G):- jouerIA(7,G),not(coupPerdantIA(7,G)).
% jouerIA(G):- jouerIA(1,G),not(coupPerdantIA(1,G)).
% 
% %Déblocage de situation : Inutile
% jouerIA(G):- jouerIA(4,G).
% jouerIA(G):- jouerIA(5,G).
% jouerIA(G):- jouerIA(3,G).
% jouerIA(G):- jouerIA(6,G).
% jouerIA(G):- jouerIA(2,G).
% jouerIA(G):- jouerIA(7,G).
% jouerIA(G):- jouerIA(1,G).
% jouerIA(G):- jouerIA(0,G).
    
    
%.......................................
% 
% square  On n'utilise pas cette structure
%.......................................
% The mark in a square(N) corresponds to an item in a list, as follows:

% square([M,_,_,_,_,_,_,_,_],1,M).
% square([_,M,_,_,_,_,_,_,_],2,M).
% square([_,_,M,_,_,_,_,_,_],3,M).
% square([_,_,_,M,_,_,_,_,_],4,M).
% square([_,_,_,_,M,_,_,_,_],5,M).
% square([_,_,_,_,_,M,_,_,_],6,M).
% square([_,_,_,_,_,_,M,_,_],7,M).
% square([_,_,_,_,_,_,_,M,_],8,M).
% square([_,_,_,_,_,_,_,_,M],9,M).


% verification de la disponibilite de la colonne demandee
is_playable([L|_], 1) :- length(L, N), N<6.
is_playable([_|B], C) :- C1 is C-1, is_playable(B, C1).


%.......................................
% win
%.......................................
% Players win by having their mark in one of the following square configurations:

win(B, M) :- % Controle si la marque M a gagne dans la grille B (l'utilise-t-on vraiment ?)
    % ... -> TODO Lucas
    .


% %.......................................
% % move	
% %.......................................
% % applies a move on the given board
% % (put mark M in square C on board B and return the resulting board B2)
% %
% 
move(B,C,M,B2) :-
%     set_item(B,C,M,B2)
    % TODO : ajouter un M dans la grille B a la colonne C et renvoyer la nouvelle grille B2
    % -> voir les fonctions d'ajout, ligne 251
    % Controler si l'ajout de M a ete victorieux (utilisation de win ?)
    .
set_item([L|B], 1, M, B2) :- addToColumn(M,L,L2), B2=[L2|B].
set_item([L|B], C, M, [L|B2]) :- C > 0, C < 8, C1 is C-1, set_item(B, C1, M, B2).


% %.......................................
% % game_over
% %.......................................
% % determines when the game is over
% %
% game_over(P, B) :-
%     game_over2(P, B)
%     .
% 
% game_over2(P, B) :-
%     opponent_mark(P, M),   %%% game is over if opponent wins
%     win(B, M)
%     .
% 
% game_over2(P, B) :-
%     blank_mark(E),
%     not(square(B,C,E))     %%% game is over if opponent wins
%     .



%%%%%%%%%%%%%%%% controle coup gagnant %%%%%%%%%%%%%%%

% win_move(B, C, M) with B the board, C the column and M the mark (x or o).
% Return Yes if the move let the player win, No if not.
% Checks only the column and the line concerned by the move
% PRECOND. : must be called after the move
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



% %.......................................
% % make_move
% %.......................................
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
%     square(B, C, E),	 INUTILE
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
    minimax(0, B, M, C, U),	% calcul de la position C a jouer avec M
    move(B,C,M,B2),		% enregistrement du coup 

    nl,
    nl,
    write('Computer places '),
    write(M),
    write(' in column '),
    write(C),
    write('.')
    .


% Remplit L avec les indices de toutes les colonnes jouables dans la board B
available_columns(B, []).
available_columns(B, [C|L]) :- is_playable(B, C), is_playable(B, L).


%.......................................
% moves
%.......................................
% retrieves a list of available moves (empty squares) on a board.
%

moves(B,L) :-
    not(win(B,x)),                	%%% if either player already won, then there are no available moves
    not(win(B,o)),
%     blank_mark(E),			% init de E a la valeur du blank_mark
%     findall(N, square(B,N,E), L), 	% remplit L avec toutes les positions N des cases vides (qui correspondent a square(B,N,E))
    available_columns(B, L),		% TODO : remplit L avec toutes les colonnes jouables
    L \= []
    .


%.......................................
% utility  Determine qui est le gagnant d'une grille pleine (appelee par minimax quand la grille est pleine)
% 
% Il faut faire en sorte de se passer de cette fonction. On ne veut pas tester la victoire sur une board pleine mais plutot a chaque coup
%.......................................
% determines the value of a given board position
%
% utility(B,U) :-
%     win(B,'x'),		% si les 'x' gagnent
%     U = 1,		% alors U vaudra 1
%     !
%     .
% 
% utility(B,U) :-		% SINON (n'est execute que si la precedente a echoue)
%     win(B,'o'),		% si les 'o' gagnent
%     U = (-1), 		% alors U vaudra -1
%     !
%     .
% 
% utility(B,U) :-		% SINON (n'est execute que si la precedente a echoue)
%     U = 0		% U vaudra 0
%     .


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


% output_board(B) :-
%     nl,
%     nl,
%     output_square(B,1),
%     write('|'),
%     output_square(B,2),
%     write('|'),
%     output_square(B,3),
%     nl,
%     write('-----------'),
%     nl,
%     output_square(B,4),
%     write('|'),
%     output_square(B,5),
%     write('|'),
%     output_square(B,6),
%     nl,
%     write('-----------'),
%     nl,
%     output_square(B,7),
%     write('|'),
%     output_square(B,8),
%     write('|'),
%     output_square(B,9), !
%     .

% output_board :-
%     board(B),
%     output_board(B), !
%     .
% 
% output_square(B,C) :-
%     square(B,C,M),
%     write(' '), 
%     output_square2(C,M),  
%     write(' '), !
%     .
% 
% output_square2(C, E) :- 
%     blank_mark(E),
%     write(C), !              %%% if square is empty, output the square number
%     .
% 
% output_square2(C, M) :- 
%     write(M), !              %%% if square is marked, output the mark
%     .
% 

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PCEUDO-RANDOM NUMBERC 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

/*****************************************
 OTHER COMPILER CUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N), 
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Cecond,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Cecond+1) * (Tick+1)),
    randomize(N), 
    !
    .

******************************************/



%.......................................
% random_int_1n
%.......................................
% returns a random integer from 1 to N
%
random_int_1n(N, V) :-
    V is random(N) + 1,
    !
    .

/*****************************************
 OTHER COMPILER CUPPORT
******************************************

arity_prolog___random_int_1n(N, V) :-
    R is random,
    V2 is (R * N) - 0.5,           
    float_text(V2,V3,fixed(0)),
    int_text(V4,V3),
    V is V4 + 1,
    !
    .

******************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LICT PROCECCING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member([V|T], V).
member([_|T], V) :- member(T,V).

append([], L, L).
append([H|T1], L2, [H|T3]) :- append(T1, L2, T3).


% %.......................................
% % set_item   Primitives inutiles
% %.......................................
% % Given a list L, replace the item at position N with V
% % return the new list in list L2
% %
% 
% set_item(L, N, V, L2) :-
%     set_item2(L, N, V, 1, L2)
%         .
% 
% set_item2( [], N, V, A, L2) :- 
%     N == -1, 
%     L2 = []
%     .
% 
% set_item2( [_|T1], N, V, A, [V|T2] ) :- 
%     A = N,
%     A1 is N + 1,
%     set_item2( T1, -1, V, A1, T2 )
%     .
% 
% set_item2( [H|T1], N, V, A, [H|T2] ) :- 
%     A1 is A + 1, 
%     set_item2( T1, N, V, A1, T2 )
%     .


% %.......................................
% % get_item	Primitives inutiles
% %.......................................
% % Given a list L, retrieve the item at position N and return it as value V
% %
% 
% get_item(L, N, V) :-
%     get_item2(L, N, 1, V)
%     .
% 
% get_item2( [], _N, _A, V) :- 
%     V = [], !,
%     fail
%         .
% 
% get_item2( [H|_T], N, A, V) :- 
%     A = N,
%     V = H
%     .
% 
% get_item2( [_|T], N, A, V) :-
%     A1 is A + 1,
%     get_item2( T, N, A1, V)
%     .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of program
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
