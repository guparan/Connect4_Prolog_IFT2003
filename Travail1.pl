%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CST 381 -– Artificial Intelligence
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

Single letter variables represent:

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
S - the number of a square on the board (1 - 9)
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
/* Paramètres : S sous-liste, L liste */
prefix(P,L) :-append(P,_,L).
sublist(S,L) :-prefix(S,L).
sublist(S,[_|T]) :-sublist(S,T).

% Fonction qui renvoie le nième élément d'une liste 
/* Paramètres : N index de l'élement qu'on veut récupérer, L liste, X élément retourné */
nthElem(N, L, []) :- length(L, N1), N1 < N.
nthElem(N, L, X) :- nth1(N, L, X).                               

% Fonction qui declare qu'un joueur a gagne
/* Paramètres : J joueur */
gagnant(J) :-write('Le Joueur '), write(J), write(' a gagné !').

% Fonction qui enregistre un coup joué dans la grille
/* Paramètres : N numéro de la colonne dans laquelle J joue, G grille, J joueur, G' nouvelle grille */          
enregistrerCoup(1, [L|G], x, _, I) :- 
    length(L,N), 
    N >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupX(I)
    .
    
enregistrerCoup(1, [L|G], o, _, I) :- 
    length(L,N), 
    N >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupO(I)
    .
    
enregistrerCoup(1, [L|G], J, F, I) :- 
    length(L,N), 
    N < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoup(N, [L|G], x, _, I) :- 
    N > 7, 
    write('Coup Invalide\n'), 
    jouerCoupX(I)
    .
    
enregistrerCoup(N, [L|G], o, _, I) :-
    N > 7, 
    write('Coup Invalide\n'), 
    jouerCoupO(I)
    .
    
enregistrerCoup(N, [T|X], J, [T|G], I) :- 
    N > 0, 
    N1 is N-1, 
    enregistrerCoup(N1, X, J, G, I)
    .
                                        
enregistrerCoupJoueur(1, [L|G], x, _, I) :- 
    length(L,N), N >= 6, 
    write('Coup Invalide\n'), 
    jouerCoupJoueur(I)
    .
    
enregistrerCoupJoueur(1, [L|G], J, F, I) :- 
    length(L,N), 
    N < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoupJoueur(N, [L|G], x, _, I) :- 
    N > 7, 
    write('Coup Invalide\n'), 
    jouerCoupJoueur(I)
    .
    
enregistrerCoupJoueur(N, [T|X], J, [T|G], I) :-  
    N > 0, 
    N1 is N-1, 
    enregistrerCoupJoueur(N1, X, J, G, I)
    .
    
enregistrerCoupIA(1, [L|G], J, F, I) :- 
    length(L,N), 
    N < 6, 
    addToColumn(J,L,M), 
    F=[M|G]
    .
    
enregistrerCoupIA(N, [T|X], J, [T|G], I) :- 
    N > 0, 
    N1 is N-1, 
    enregistrerCoupIA(N1, X, J, G, I)
    .
 
% Fonctions principales dans le deroulement de la partie
/* Paramètres : G grille*/
jouerCoupX(G) :- finJeu(G,J), gagnant(J),!.
jouerCoupO(G) :- finJeu(G,J), gagnant(J),!.
jouerCoupX(G) :- 
    write('Joueur x, entrez un numéro de colonne : '),
    read(N), 
    enregistrerCoup(N,G, x, X, G),
    afficherGrille(X),
    write('\n'),
    jouerCoupO(X)
    .
jouerCoupO(G) :- 
    write('Joueur o, entrez un numéro de colonne : '),
    read(N), 
    enregistrerCoup(N,G, o, X, G),
    afficherGrille(X),
    write('\n'),
    jouerCoupX(X)
    .
jouerCoupJoueur(G) :- 
    write('Joueur x, entrez un numéro de colonne : '),
    read(N), 
    enregistrerCoupJoueur(N,G, x, X, G),
    afficherGrille(X),
    write('\n'),
    jouerIA(X)
    .
    
    jouerIA(G):-finJeu(G,J), gagnant(J),!.

%Si un coup permet de gagner il faut le jouer.
jouerIA(G) :- 
    coupGagnant(C,G,o), 
    enregistrerCoupIA(C,G,o,X,G),
    afficherGrille(X),
    write('\n'),
    jouerCoupJoueur(X)
    .

%Si un coup permet a l'adversaire de gagner on se défend(coup défensif).
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
    longueur(L,N2), 
    N3 is 6-N2, 
    E=N3
    .
    
espaceRestant(N, [T|X], E, L) :- 
    N > 0, 
    N1 is N-1, 
    espaceRestant(N1, X, E, L)
    .
                                                                        
%Si on a pas de coup immédiat on fait un coup au centre ou au plus près possible pour une victoire possible en verticale. : Inutile
jouerIA(G):- espaceRestant(4,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(4,G)), jouerIA(4,G).
jouerIA(G):- espaceRestant(5,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(5,G)), jouerIA(5,G).
jouerIA(G):- espaceRestant(3,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(3,G)), jouerIA(3,G).
jouerIA(G):- espaceRestant(6,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(6,G)), jouerIA(6,G).
jouerIA(G):- espaceRestant(2,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(2,G)), jouerIA(2,G).
jouerIA(G):- espaceRestant(7,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(7,G)), jouerIA(7,G).
jouerIA(G):- espaceRestant(1,G,E,L), finListe(L,o), E > 3, not(coupPerdantIA(1,G)), jouerIA(1,G).

%Sinon jouer au plus près du centre quand même. : Inutile
jouerIA(G):- jouerIA(4,G),not(coupPerdantIA(4,G)).
jouerIA(G):- jouerIA(5,G),not(coupPerdantIA(5,G)).
jouerIA(G):- jouerIA(3,G),not(coupPerdantIA(3,G)).
jouerIA(G):- jouerIA(6,G),not(coupPerdantIA(6,G)).
jouerIA(G):- jouerIA(2,G),not(coupPerdantIA(2,G)).
jouerIA(G):- jouerIA(7,G),not(coupPerdantIA(7,G)).
jouerIA(G):- jouerIA(1,G),not(coupPerdantIA(1,G)).

%Déblocage de situation : Inutile
jouerIA(G):- jouerIA(4,G).
jouerIA(G):- jouerIA(5,G).
jouerIA(G):- jouerIA(3,G).
jouerIA(G):- jouerIA(6,G).
jouerIA(G):- jouerIA(2,G).
jouerIA(G):- jouerIA(7,G).
jouerIA(G):- jouerIA(1,G).
jouerIA(G):- jouerIA(0,G).
    
    
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
% win   DEPRECATED
%.......................................
% Players win by having their mark in one of the following square configurations:

% Controle d'une victoire eventuelle
% -> Lucas

% win([M,M,M, _,_,_, _,_,_],M).
% win([_,_,_, M,M,M, _,_,_],M).
% win([_,_,_, _,_,_, M,M,M],M).
% win([M,_,_, M,_,_, M,_,_],M).
% win([_,M,_, _,M,_, _,M,_],M).
% win([_,_,M, _,_,M, _,_,M],M).
% win([M,_,_, _,M,_, _,_,M],M).
% win([_,_,M, _,M,_, M,_,_],M).


% %.......................................
% % move        
% %.......................................
% % applies a move on the given board
% % (put mark M in square S on board B and return the resulting board B2)
% %
% 
move(B,C,M,B2) :-
%     set_item(B,S,M,B2)
    % TODO : ajouter un M dans la grille B a la colonne C et renvoyer la nouvelle grille B2
    .


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
%     not(square(B,S,E))     %%% game is over if opponent wins
%     .


% METTE COUP GAGNANT ICI


% %.......................................
% % make_move
% %.......................................
% % requests next move from human/computer, 
% % then applies that move to the given board
% %
% 
make_move(P, B) :-
    player(P, Type),                    % recuperation du type du joueur p (human ou computer)

    make_move2(Type, P, B, B2), % demande d'un coup dans une nouvelle board

    retract( board(_) ),        % remplacement de la board precedente
    asserta( board(B2) )        % par la nouvelle board
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

    blank_mark(E),              % definition de E a la valeur de la blank_mark (voir les predicats, blank_mark = 'e')
%     square(B, S, E),   INUTILE
%     is_playable(B, C),        % TODO : verification de la disponibilite de la colonne demandee
    player_mark(P, M),          % recuperation de la marque M du joueur P
    move(B, C, M, B2), !        % realisation du coup 
    .
% 
% % Fonction executee si la precedente echoue : l'utilisateur a entre un nombre invalide / la case est prise ou n'existe pas
% make_move2(human, P, B, B2) :-
%     nl,
%     nl,
%     write('Error : Please select a numbered square.'),                % Message d'erreur
%     make_move2(human,P,B,B2)                                  % reexecution de la fonction precedente
%     .
% 
% % Demande d'un coup a l'ordinateur
% make_move2(computer, P, B, B2) :-
%     nl,
%     nl,
%     write('Computer is thinking about next move...'),
%     player_mark(P, M),                % recuperation de la marque M du joueur P
%     minimax(0, B, M, S, U),   % calcul de la position S a jouer avec M
%     move(B,S,M,B2),           % enregistrement du coup 
% 
%     nl,
%     nl,
%     write('Computer places '),
%     write(M),
%     write(' in square '),
%     write(S),
%     write('.')
%     .
% 
% 
% %.......................................
% % moves
% %.......................................
% % retrieves a list of available moves (empty squares) on a board.
% %
% 
% moves(B,L) :-
%     not(win(B,x)),                    %%% if either player already won, then there are no available moves
%     not(win(B,o)),
%     blank_mark(E),                    % init de E a la valeur du blank_mark
%     findall(N, square(B,N,E), L),     % remplit L avec toutes les positions N des cases vides (qui correspondent a square(B,N,E))
%     L \= []
%     .


%.......................................
% utility
%.......................................
% determines the value of a given board position
%

utility(B,U) :-
    win(B,'x'),         % si les 'x' gagnent
    U = 1,              % alors U vaudra 1
    !
    .

utility(B,U) :-         % SINON (n'est execute que si la precedente a echoue)
    win(B,'o'),         % si les 'o' gagnent
    U = (-1),           % alors U vaudra -1
    !
    .

utility(B,U) :-         % SINON (n'est execute que si la precedente a echoue)
    U = 0               % U vaudra 0
    .


%.......................................
% minimax
%.......................................
% The minimax algorithm always assumes an optimal opponent.
% For tic-tac-toe, optimal play will always result in a tie, so the algorithm is effectively playing not-to-lose.

% For the opening move against an optimal player, the best minimax can ever hope for is a tie.
% So, technically speaking, any opening move is acceptable.
% Save the user the trouble of waiting  for the computer to search the entire minimax tree 
% by simply selecting a random square.

% On connait D : profondeur de recherche
% On connait B : board de recherche
% On connait M : mark du joueur ('x' ou 'o')
% On cherche a determiner S, la meilleure position a jouer
% On cherche a determiner U, la meilleure evaluation qu'on a trouve, celle qui correspond a S


minimax(D,[E,E,E, E,E,E, E,E,E],M,S,U) :-   
    blank_mark(E),              % Si la board est vide (toutes les cases matchent la blank_mark)
    random_int_1n(9,S),         % On choisit une position a jouer au hasard
    !
    .

minimax(D,B,M,S,U) :-   % SINON (la board n'est pas vide)
    D2 is D + 1,
    moves(B,L),          %%% get the list of available moves
    !,
    best(D2,B,M,L,S,U),  %%% recursively determine the best available move
    !
    .



minimax(D,B,M,S,U) :-   % SINON (there are no more available moves)
    utility(B,U)        % then the minimax value is the utility of the given board position
    .


%.......................................
% best
%.......................................
% determines the best move in a given list of moves by recursively calling minimax
%

% if there is only one move left in the list ( [S1] )
best(D,B,M,[S1],S,U) :-         
    move(B,S1,M,B2),        %%% apply that move to the board,
    inverse_mark(M,M2),         % recuperation de la mark de l'adversaire de M dans M2
    !,  
    minimax(D,B2,M2,_S,U),  %%% then recursively search for the utility value of that move. (???)
    S = S1, !,
    output_value(D,S,U),
    !
    .

% if there is more than one move in the list ( [S1|T] )
best(D,B,M,[S1|T],S,U) :-
    move(B,S1,M,B2),                    %%% apply the first move (in the list) to the board,
    inverse_mark(M,M2),                 % recuperation de la mark de l'adversaire de M dans M2
    !,
    minimax(D,B2,M2,_S,U1),             %%% recursively search for the utility value of that move,
    best(D,B,M,T,S2,U2),                %%% determine the best move of the remaining moves,
    output_value(D,S1,U1),      
    better(D,M,S1,U1,S2,U2,S,U) %%% and choose the better of the two moves (based on their respective utility values)
    .


%.......................................
% better
%.......................................
% returns the better of two moves based on their respective utility values.
%
% if both moves have the same utility value, then one is chosen at random.

better(D,M,S1,U1,S2,U2,     S,U) :-
    maximizing(M),                     %%% if the player is maximizing
    U1 > U2,                           %%% then greater is better.
    S = S1,
    U = U1,
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    minimizing(M),                     %%% if the player is minimizing,
    U1 < U2,                           %%% then lesser is better.
    S = S1,
    U = U1, 
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-
    U1 == U2,                          %%% if moves have equal utility,
    random_int_1n(10,R),               %%% then pick one of them at random
    better2(D,R,M,S1,U1,S2,U2,S,U),    
    !
    .

better(D,M,S1,U1,S2,U2,     S,U) :-        %%% otherwise, second move is better
    S = S2,
    U = U2,
    !
    .


%.......................................
% better2
%.......................................
% randomly selects two squares of the same utility value given a single probability
%

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    R < 6,
    S = S1,
    U = U1, 
    !
    .

better2(D,R,M,S1,U1,S2,U2,  S,U) :-
    S = S2,
    U = U2,
    !
    .



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% OUTPUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% output_players :- 
%     nl,
%     player(1, V1),
%     write('Player 1 is '),   %%% either human or computer
%     write(V1),
% 
%     nl,
%     player(2, V2),
%     write('Player 2 is '),   %%% either human or computer
%     write(V2), 
%     !
%     .
% 
% 
% output_winner(B) :-
%     win(B,x),
%     write('X wins.'),
%     !
%     .
% 
% output_winner(B) :-
%     win(B,o),
%     write('O wins.'),
%     !
%     .
% 
% output_winner(B) :-
%     write('No winner.')
%     .


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
% output_square(B,S) :-
%     square(B,S,M),
%     write(' '), 
%     output_square2(S,M),  
%     write(' '), !
%     .
% 
% output_square2(S, E) :- 
%     blank_mark(E),
%     write(S), !              %%% if square is empty, output the square number
%     .
% 
% output_square2(S, M) :- 
%     write(M), !              %%% if square is marked, output the mark
%     .
% 
% output_value(D,S,U) :-
%     D == 1,
%     nl,
%     write('Square '),
%     write(S),
%     write(', utility: '),
%     write(U), !
%     .
% 
% output_value(D,S,U) :- 
%     true
%     .
    
    
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
%%% PSEUDO-RANDOM NUMBERS 
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
 OTHER COMPILER SUPPORT
******************************************

arity_prolog___random_seed(N) :-
    nonvar(N),
    randomize(N), 
    !
    .

arity_prolog___random_seed(N) :-
    var(N),
    time(time(Hour,Minute,Second,Tick)),
    N is ( (Hour+1) * (Minute+1) * (Second+1) * (Tick+1)),
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
 OTHER COMPILER SUPPORT
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
%%% LIST PROCESSING
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
% % get_item    Primitives inutiles
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
