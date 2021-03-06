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



%///////////// GRAMMAIRE DES QUESTIONS /////////////////////

formater(Fait) --> groupe_nominal(X), groupe_verbal(Y,Z), {Fait=..[Y,X,Z]}.

groupe_nominal(X)--> determinant, nom(X).
groupe_nominal(X)--> nom(X).
groupe_nominal(X) --> pronom_interrogatif(X).
groupe_nominal(Y,Z) --> determinant, lien(Y), determinant, nom(Z).

groupe_verbal(Y,Z) --> verbe, groupe_nominal(Y,Z).
groupe_verbal(Y,Z) --> verbe(Y), groupe_nominal(Z).

nom(X) --> nomCommun(X).
nom(X) --> nomPropre(X).

determinant-->[un].
determinant-->[une].
determinant-->[le].
determinant-->[la].
determinant-->[les].
determinant-->[du].
determinant-->[de].
determinant-->[des].

pronom_interrogatif(X)-->[qui].

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



%/////////////////// VALIDATION /////////////////////

repondre(pere(X,Z),Reponse):- nonvar(X), nonvar(Z), pere(X,Z), Reponse = 'true'.
repondre(pere(X,Z),Reponse):- pere(X,Z), Reponse = pere(X,Z).
repondre(frere(X,Z),Reponse):- nonvar(X), nonvar(Z), frere(X,Z), Reponse = 'true'.
repondre(frere(X,Z),Reponse):- frere(X,Z), Reponse = frere(X,Z).
repondre(mange(X,Z),Reponse):- nonvar(X), nonvar(Z), mange(X,Z), Reponse = 'true'.
repondre(mange(X,Z),Reponse):- mange(X,Z), Reponse = mange(X,Z).
repondre(aime(X,Z),Reponse):- nonvar(X), nonvar(Z), aime(X,Z), Reponse = 'true'.
repondre(aime(X,Z),Reponse):- aime(X,Z), Reponse = aime(X,Z).
repondre(possede(X,Z),Reponse):- nonvar(X), nonvar(Z), possede(X,Z), Reponse = 'true'.
repondre(possede(X,Z),Reponse):- possede(X,Z), Reponse = possede(X,Z).
repondre(Faits, Reponse):- Reponse = 'false'.



%//////////////// GRAMMAIRE DES REPONSES /////////////////

% Methode 1  :
ecrire(Faits) :- Faits=='true', dire(['Oui.']), nl.
ecrire(Faits) :- Faits=='false', dire(['Non.']), nl.
ecrire(Faits) :- Faits=..[Y,X,Z], dire([X, Y, Z]), nl.

  /* Methode 2  :
ecrire(Phrase) --> <groupe_nominal2(GN)> <groupe_verbal2(GV)>, {Phrase=[GN,GV]}.

groupe_nominal2(DET,NOM) --> <determinant2(DET)> <nom2(NOM)>.
groupe_nominal2(DET,NOM,DET2,NOM2) --> determinant3(DET), lien2(NOM), determinant4(DET2), nom2(NOM2).

groupe_verbal2(Y,Z) --> verbe2, groupe_nominal2(Y,Z). 
groupe_verbal2(Y,Z) --> verbe2(Y), groupe_nominal2(Z).

nom2(X) --> nomCommun2(X).
nom2(X) --> nomPropre2(X).

determinant2(un)-->[un].
determinant2(le)-->[le].
determinant2(les)-->[les].
determinant2(de)-->[de].
determinant2(des)-->[des].
determinant3(le)-->[le].
determinant4(de)-->[de].

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
verbe2(possede) --> [possède].
 */
 
 

%////////////// LANCER INTERFACE //////////////////

lancer :-
    lire(Question),                     %% L'utilisateur pose sa question
%     write('Lecture OK\n'),
    formater(Faits,Question,[]),        %% On formate la question
%     write('Formatage OK\n'),
    repondre(Faits, Reponse),           %% On y repond
%     write('Reponse OK\n'),
    write('Reponse : '),
    ecrire(Reponse),                    %% On ecrit la réponse
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



%/////////////// QUESTIONS DE TEST //////////////////

test(QuestionChaine) :-
	write('Question : '),
	write(QuestionChaine),
	nl,
	name(QuestionChaine, Temp),
	chaine_liste(Temp, QuestionListe),
	formater(Faits,QuestionListe,[]),  %% On formate la question
	repondre(Faits, Reponse),          %% On y repond
	write('Reponse : '),
	ecrire(Reponse),
	nl
	.

tests :-
	test('qui est le pere de nicolas'),
	test('felix mangetil des croquettes'),
	test('pierre aimetil les chiens'),
	test('anne aimetelle les chats'),
	test('nicolas possedetil un chat'),
	test('qui est le frere de anne')
	.