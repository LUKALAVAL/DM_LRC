autoref(C,C) :- cnamea(C).
autoref(C,C) :- cnamena(C).
autoref(C,D) :- cnamea(C), equiv(D,E), autoref(C,E).
autoref(C,D) :- cnamena(C), equiv(D,E), autoref(C,E).
autoref(C,and(D,_)) :- cnamea(C), autoref(C,D).
autoref(C,and(D,_)) :- cnamena(C), autoref(C,D).
autoref(C,and(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,and(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,or(D,_)) :- cnamea(C), autoref(C,D).
autoref(C,or(D,_)) :- cnamena(C), autoref(C,D).
autoref(C,or(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,or(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,not(D)) :- cnamea(C), autoref(C,D).
autoref(C,not(D)) :- cnamena(C), autoref(C,D).
autoref(C,some(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,some(_,D)) :- cnamena(C), autoref(C,D).
autoref(C,all(_,D)) :- cnamea(C), autoref(C,D).
autoref(C,all(_,D)) :- cnamena(C), autoref(C,D).

nnf(not(and(C1,C2)),or(NC1,NC2)):- nnf(not(C1),NC1),nnf(not(C2),NC2),!.
nnf(not(or(C1,C2)),and(NC1,NC2)):- nnf(not(C1),NC1),nnf(not(C2),NC2),!.
nnf(not(all(R,C)),some(R,NC)) :- nnf(not(C),NC),!.
nnf(not(some(R,C)),all(R,NC)):- nnf(not(C),NC),!.
nnf(not(not(X)),X):- !.
nnf(not(X),not(X)):- !.
nnf(and(C1,C2),and(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(or(C1,C2),or(NC1,NC2)):- nnf(C1,NC1),nnf(C2,NC2),!.
nnf(some(R,C),some(R,NC)):- nnf(C,NC),!.
nnf(all(R,C),all(R,NC)) :- nnf(C,NC),!.
nnf(X,X).

concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
concept(not(C)) :- concept(C), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(some(R,C)) :- rname(R), concept(C), !.
concept(all(R,C)) :- rname(R), concept(C), !.

instance(I) :- iname(I), !.

remplace(C,C) :- cnamea(C).
remplace(not(C),not(CC)) :- remplace(C,CC).
remplace(and(C1,C2),and(CC1,CC2)) :- remplace(C1,CC1), remplace(C2,CC2).
remplace(or(C1,C2),or(CC1,CC2)) :- remplace(C1,CC1), remplace(C2,CC2).
remplace(some(R,C),some(R,CC)) :- remplace(C,CC).
remplace(all(R,C),all(R,CC)) :- remplace(C,CC).
remplace(C,CC) :- equiv(C,D), remplace(D,CC).




# genere(Nom) :- compteur(V),nombre(V,L1),
#   concat([105,110,115,116],L1,L2),
#   V1 is V+1,
#   dynamic(compteur/1),
#   retract(compteur(V)),
#   dynamic(compteur/1),
#   assert(compteur(V1)),nl,nl,nl,
#   name(Nom,L2).
#
# nombre(0,[]).
# nombre(X,L1) :-
#   R is (X mod 10),
#   Q is ((X-R)//10),
#   chiffre_car(R,R1),
#   char_code(R1,R2),
#   nombre(Q,L),
#   concat(L,[R2],L1).
#
# chiffre_car(0,'0').
# chiffre_car(1,'1').
# chiffre_car(2,'2').
# chiffre_car(3,'3').
# chiffre_car(4,'4').
# chiffre_car(5,'5').
# chiffre_car(6,'6').
# chiffre_car(7,'7').
# chiffre_car(8,'8').
# chiffre_car(9,'9').

# semantique(A) :- cnamea(A).
# semantique(A) :- cnamena(A).
# semantique(not(A)) :- semantique(A).
# semantique(and(A,B)) :- semantique(A), semantique(B).
# semantique(or(A,B)) :- semantique(A), semantique(B).
# semantique(all(R,A)) :- rname(R), semantique(A).
# semantique(some(R,A)) :- rname(R), semantique(A).
# semantique(inst(I,C)) :- iname(I), cnamea(C).
# semantique(inst(I,C)) :- iname(I), cnamena(C).
# semantique(instR(I1,I2,R)) :- iname(I1), iname(I2), rname(R).
