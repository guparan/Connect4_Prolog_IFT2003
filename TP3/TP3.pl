% Author:
% Date: 4/17/2013
/*
    La grammaire complète permettant d’analyser les questions:
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
nc( homme ) --> [homme].
v( mordre ) --> [mord].
 */
 
 
 %//////////////// BASE DE CONNAISSANCES ////////////////////
 
 pere(paul,nicolas).
 frere(nicolas, pierre).
 frere(nicolas, anne).
 frere(X,Y):- frere(Y,X).
 frere(X,Y):- frere(X,Z), frere(X,Y).
 est(garfield, chat).
 est(felix, chat).
 possede(anne, garfield).
 aime(anne, chats).
 aime(anne, chiens).
 not(aime(pierre,chien)).
 mange(X, croquettes) :- est(X, chat).
 mange(X, pate):- est(X, chat).
 
 
 
 %/////////////GRAMMAIRE DES QUESTIONS /////////////////////
phrase(Fait) --> groupe_nominal(X), groupe_verbal(Y,Z), {Fait=..[Y,X,Z]}.
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
pronom_interrogatif(x)-->[qui].   %la réponse écrit artificiellement ici
lien(frere)-->[frere].
lien(pere)-->[pere].
nomPropre(nicolas) --> [nicolas].
nomPropre(felix) --> [felix].
nomPropre(pierre) --> [pierre].
nomPropre(anne) --> [anne].
nomCommun(pate) --> [pate].
nomCommun(croquettes) --> [croquettes].
nomCommun(chiens) --> [chiens].
nomCommun(chats) --> [chats].
nomCommun(chat) --> [chat].
verbe --> [est].
verbe(mange) --> [mange-t-il].
verbe(aime) --> [aime-t-il].
verbe(aime) --> [aime-t-elle].
verbe(possede) --> [possède-t-il].