% Fonctions recuperees dans puissance4.pl

% Fonction qui renvoie une sous-liste a partir d'une liste L
/* Parametres : S sous-liste, L liste */
prefix(P,L):-append(P,_,L).
sublist(S,L):-prefix(S,L).
sublist(S,[_|T]):-sublist(S,T).

% Fonction qui renvoie le nieme element d'une liste
/* Parametres : N index de l'element qu'on veut recuperer,
	L liste, X element retourne */
nthElem(N, L, []):- length(L, N1), N1 < N.
nthElem(N, L, X):- nth1(N, L, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% coupGagnant(G, C, J) avec G la grille, C la colonne dans
% laquelle on ajoute un pion et J le joueur (x ou o). Renvoie Yes si le
% coup est gagnant, No sinon. Vérifie uniquement la colonne, la ligne et
% les deux diagonales dont le pion fait partie.


coupGagnant(G, C, J) :- coupGagnantColonne(G, C, J) ; ligneJouee(G, C, N), coupGagnantLigne(G, N, J).

%Condition de victoire verticale : 4 jetons les uns apres les autres sur une meme colonne
/* Parametres : G grille, C colonne testee, J joueur */
coupGagnantColonne([L|_], 1, J) :- sublist([J,J,J,J], L).
coupGagnantColonne([_|G], C, J) :- C1 is C-1, coupGagnantColonne(G, C1, J).

%Parametres : G la grille, C la colonne dans laquelle ou joue et
%N le numero de la ligne dans laquelle le pion atterit
ligneJouee([L|_], 1, N) :- length(L, N).
ligneJouee([_|G], C, N) :- C1 is C-1, ligneJouee(G, C1, N).

%Condition de victoire horizontale : 4 jetons les uns apres les autres sur une meme ligne
/* Parametres : G grille, N numero de la ligne traitee, J joueur */
coupGagnantLigne(G, N, J) :- maplist(nthElem(N), G, L), sublist([J,J,J,J], L).
