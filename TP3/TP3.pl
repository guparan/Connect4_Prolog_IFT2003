% Author:
% Date: 4/17/2013
/*
    La grammaire complète permettant d'analyser les questions:
phrase -> <groupe_nominal> <groupe_verbal>
groupe_nominal-> <determinant> <nom>
groupe_nominal-> <pronom_interrogatif>
groupe_nominal-> <determinant> <nom> <determinant> <nom>
nom-> nomCommun
nom-> nomPropre
groupe_verbal-> <verbe> <groupe_nominal>
pronom_interrogatif -> Qui |
determinant -> le | de | des | les | un
verbe -> est | mange-t-il | aime-t-il | aime-t-elle | possède-t-il
nomCommun -> père | croquettes | chiens  | chats | chat
nomPropre -> Felix | Pierre| Anne | Nicolas

p( SEM ) --> gn(AGNT), gv(ACT, OBJ), {AGNT \= OBJ, SEM =..[ACT,AGNT, OBJ]}.
gn( AGNT ) --> art, nc(AGNT).
gv( ACT,OBJ ) --> v(ACT), gn(OBJ).
art --> [le].
art --> [un].
nc( chien ) --> [chien].
nc( homme ) --> [homme].                       #
v( mordre ) --> [mord].

qui est le pere de nicolas

Texte :
« Paul est le père de Nicolas. 
Nicolas est le frère de Pierre et Anne. 
Anne a un chat appelé Garfield.
Anne aime les chats et les chiens mais son frère Pierre n’aime pas les chiens. 
Felix est un chat. 
Les chats mangent des croquettes et du paté.»
 */
 
 
 %//////////////// BASE DE CONNAISSANCES ////////////////////
 
 pere(paul,nicolas).
 frere(nicolas, pierre).
 frere(nicolas, anne).
 frere(X,Y):- frere(Y,X).
 frere(X,Y):- frere(X,Z), frere(Z,Y).
 est(garfield, chat).
 est(felix, chat).
 possede(anne, garfield).
 aime(anne, chats).
 aime(anne, chiens).
 not(aime(pierre,chien)).
 mange(X, croquettes) :- est(X, chat).
 mange(X, pate):- est(X, chat).
 
 
 
 %/////////////GRAMMAIRE DES QUESTIONS /////////////////////
formater(Fait) --> groupe_nominal(X), groupe_verbal(Y,Z), {Fait=..[Y,X,Z]}.

groupe_nominal(X)--> determinant, nom(X).
groupe_nominal(X)--> nom(X).
groupe_nominal(X) --> pronom_interrogatif(X).
groupe_nominal(Y,Z) --> determinant, lien(Y), determinant, nom(Z).

groupe_verbal(Y,Z) --> verbe, groupe_nominal(Y,Z). %changement de X par Y et Y par Z pour compréhension
groupe_verbal(Y,Z) --> verbe(Y), groupe_nominal(Z).

nom(X) --> nomCommun(X).
nom(X) --> nomPropre(X).

determinant-->[un].
determinant-->[le].
determinant-->[de].
determinant-->[les].
determinant-->[des].

pronom_interrogatif(X)-->[qui].   %la réponse écrit artificiellement ici

lien(frere)-->[frere].
lien(pere)-->[pere].

nomPropre(anne) --> [anne].
nomPropre(nicolas) --> [nicolas].
nomPropre(pierre) --> [pierre].
nomPropre(paul) --> [paul].
nomPropre(felix) --> [felix].
nomPropre(garfield) --> [garfield].

nomCommun(pate) --> [pate].
nomCommun(croquettes) --> [croquettes].
nomCommun(chien) --> [chien].
nomCommun(chiens) --> [chiens].
nomCommun(chat) --> [chat].
nomCommun(chats) --> [chats].

verbe --> [est].
verbe(mange) --> [mangetil].
verbe(mange) --> [mangetelle].
verbe(aime) --> [aimetil].
verbe(aime) --> [aimetelle].
verbe(possede) --> [possedetil].
verbe(possede) --> [possedetelle].


%//////////////VALIDATION///////////////////////////////////
repondre(Faits, Reponse):- verifier(Faits), Reponse = Faits.
repondre(Faits, Reponse):- Reponse = 'false'.
verifier(pere(X,Z)):- pere(X,Z).
verifier(frere(X,Z)):- frere(X,Z).
verifier(mange(X,Z)):- mange(X,Z).
verifier(aime(X,Z)):- aime(X,Z).
verifier(possede(X,Z)):- possede(X,Z).

% valide2(E,R):- repondre(S,E,[]), repondre2(S, R).
% verifier2(pere(X,Z), X):- pere(X,Z).
% verifier2(frere(X,Z), X):- frere(X,Z).
% verifier2(mange(X,Z), X):- mange(X,Z).
% verifier2(aime(X,Z), X):- aime(X,Z).
% verifier2(possede(X,Z), X):- possede(X,Z).

%//////////////GRAMMAIRE DES REPONSES //////////////////////
% Methode 1  :
ecrire(Faits) :- Faits=='false', dire(['Non.']), nl.
ecrire(Faits) :- Faits=..[Y,X,Z], dire([X, Y, Z]), nl.

  /* Methode 2  :
ecrire(Phrase) --> <groupe_nominal2(GN)> <groupe_verbal2(GV)>, {Phrase=[GN,GV]}.
groupe_nominal2(DET,NOM) --> <determinant2(DET)> <nom2(NOM)>.
groupe_nominal2(DET,NOM,DET2,NOM2) --> determinant3(DET), lien2(NOM), determinant4(DET2), nom2(NOM2).
groupe_verbal2(Y,Z) --> verbe2, groupe_nominal2(Y,Z). %changement de X par Y et Y par Z pour compréhension
groupe_verbal2(Y,Z) --> verbe2(Y), groupe_nominal2(Z).
nom2(X) --> nomCommun2(X).
nom2(X) --> nomPropre2(X).
determinant2(un)-->[un].
determinant2(le)-->[le].
determinant2(de)-->[de].
determinant2(les)-->[les].
determinant2(des)-->[des].
determinant3(le)
determinant4(de)
lien2(frere)-->[frere].
lien2(pere)-->[pere].
nomPropre2(nicolas) --> [nicolas].
nomPropre2(felix) --> [felix].
nomPropre2(pierre) --> [pierre].
nomPropre2(anne) --> [anne].
nomCommun2(pate) --> [pate].
nomCommun2(croquettes) --> [croquettes].
nomCommun2(chiens) --> [chiens].
nomCommun2(chats) --> [chats].
nomCommun2(chat) --> [chat].
verbe2 --> [est].
verbe2(mange) --> [mange].
verbe2(aime) --> [aime].
verbe2(aime) --> [aime].
verbe2(possede) --> [possède].
 */
%//////////////LANCER INTERFACE//////////////////

lancer :-
    lire(Question),			%% L'utilisateur pose sa question
%     write('Lecture OK\n'),
    formater(Faits,Question,[]), 	%% On formate la question
%     write('Formatage OK\n'),
    repondre(Faits, Reponse),		%% On y repond
%     write('Reponse OK\n'),
    write('Reponse : '),
    ecrire(Reponse),			%% On ecrit la réponse
    lancer
    .

lancer :-
    write('Phrase invalide\n'),
    lancer
    .

%Fonction qui exprime une liste en une phrase.
dire([X|R]) :- write(X), write(' '), dire(R).
dire([]).

% Le prédicat lire/2 lit une chaîne de caractères Chaine entre apostrophes
% et terminée par un point.
% Resultat correspond à la liste des mots contenus dans la phrase.
% Les signes de ponctuation ne sont pas gérés.
lire(Resultat):- 
    write('Question (exit pour quitter) : '), 
    read(Chaine),
    Chaine \= 'exit',
    name(Chaine, Temp), 
    chaine_liste(Temp, Resultat),
    !.

lire(Resultat) :-
    abort.
                       
% Prédicat de transformation de chaîne en liste
chaine_liste([],[]).
chaine_liste(Liste,[Mot|Reste]):- separer(Liste,32,A,B), name(Mot,A),
chaine_liste(B,Reste).

% Sépare une liste par rapport à un élément
separer([],X,[],[]):-!.
separer([X|R],X,[],R):-!.
separer([A|R],X,[A|Av],Ap):- X\==A, !, separer(R,X,Av,Ap).
