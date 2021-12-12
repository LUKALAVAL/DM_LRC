/**** PROGRAMME ****/

programme :-
  premiere_etape(Tbox,Abi,Abr),
  deuxieme_etape(Abi,Abi1,Tbox),
  troisieme_etape(Abi1,Abr).




/**** PREMIERE PARTIE ****/

premiere_etape(Tbox,Abi,Abr) :-
  make_tbox(Tbox),
  make_abi(Abi),
  make_abr(Abr).

make_tbox(Tbox) :- setof((C,D), equiv(C,D), Tbox).
make_abi(Abi) :- setof((I,C), inst(I,C), Abi).
make_abr(Abr) :- setof((I1,I2,R), instR(I1,I2,R), Abr).




/**** DEUXIEME PARTIE ****/

deuxieme_etape(Abi,Abi1,Tbox) :-
  saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox) :-
  nl, write('Entrez le numero du type de proposition que vous demontrez :'),
  nl, write('1 Une instance donnee appartient a un concept donne.'),
  nl, write('2 Deux concepts n"ont pas d"elements en commun(ils ont une intersection vide).'),
  nl, read(R), suite(R,Abi,Abi1,Tbox).

suite(1,Abi,Abi1,Tbox) :-
  acquisition_prop_type1(Abi,Abi1,Tbox),!.
suite(2,Abi,Abi1,Tbox) :-
  acquisition_prop_type2(Abi,Abi1,Tbox),!.
suite(_,Abi,Abi1,Tbox) :-
  nl, write('Cette reponse est incorrecte.'),
  nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

acquisition_prop_type1(Abi,Abi1,Tbox) :-
  nl, write('Entrez une instance :'),
  nl, read(I), instance(I),
  nl, write('Entrez un concept :'),
  nl, read(C), concept(C),
  remplace(C,C_),
  nnf(not(C_),NC_),
  Abi1 = [(I,NC_)|Abi].

acquisition_prop_type2(Abi,Abi1,Tbox) :-
  nl, write('Entrez un concept (1) :'),
  nl, read(C1), concept(C1),
  nl, write('Entrez un concept (2) :'),
  nl, read(C2), concept(C2),
  remplace(and(C1,C2),C_),
  nnf(C_,NC_),
  genere(I),
  Abi1 = [(I,NC_)|Abi].

concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
concept(not(C)) :- concept(C), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(some(R,C)) :- rname(R), concept(C), !.
concept(all(R,C)) :- rname(R), concept(C), !.

instance(I) :- iname(I), !.

remplace(C,C) :- cnamea(C).
remplace(not(C),not(C_)) :- remplace(C,C_).
remplace(and(C1,C2),and(C_1,C_2)) :- remplace(C1,C_1), remplace(C2,C_2).
remplace(or(C1,C2),or(C_1,C_2)) :- remplace(C1,C_1), remplace(C2,C_2).
remplace(some(R,C),some(R,C_)) :- remplace(C,C_).
remplace(all(R,C),all(R,C_)) :- remplace(C,C_).
remplace(C,C_) :- equiv(C,D), remplace(D,C_).





/**** TROISIEME PARTIE ****/

troisieme_etape(Abi,Abr) :-
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
  affiche_evolution_Abox([],[],[],[],[],[],Lie,Lpt,Li,Lu,Ls,Abr), !,
  resolution(Lie,Lpt,Li,Lu,Ls,Abr),
  nl, write('Youpiiiiii, on a demontre la proposition initiale !!!').

tri_Abox([],[],[],[],[],[]).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([E|Abi],Lie,Lpt,Li,Lu,[E|Ls]) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

resolution(_,_,_,_,Ls,_) :- clash(Ls).
resolution(Lie,Lpt,Li,Lu,Ls,Abr) :- complete_some(Lie,Lpt,Li,Lu,Ls,Abr).

complete_some([E|Lie],Lpt,Li,Lu,Ls,Abr) :-
  E = (A,some(R,C)),
  nl, write('CALL SOME'),
  genere(B),
  evolue((B,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
  Abr1 = [(A,B,R)|Abr],
  affiche_evolution_Abox([E|Lie],Lpt,Li,Lu,Ls,Abr,Lie1,Lpt1,Li1,Lu1,Ls1,Abr1), !,
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr1),
  nl, write('EXIT SOME').
complete_some([],Lpt,Li,Lu,Ls,Abr) :-
  transformation_and([],Lpt,Li,Lu,Ls,Abr).

transformation_and(Lie,Lpt,[E|Li],Lu,Ls,Abr) :-
  E = (A,and(C1,C2)),
  nl, write('CALL AND'),
  evolue((A,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
  Abr1 = Abr,
  evolue((A,C2), Lie1, Lpt1, Li1, Lu1, Ls1, Lie2, Lpt2, Li2, Lu2, Ls2),
  Abr2 = Abr1,
  affiche_evolution_Abox(Lie,Lpt,[E|Li],Lu,Ls,Abr,Lie2,Lpt2,Li2,Lu2,Ls2,Abr2), !,
  resolution(Lie2,Lpt2,Li2,Lu2,Ls2,Abr2),
  nl, write('EXIT AND').
transformation_and(Lie,Lpt,[],Lu,Ls,Abr) :-
  deduction_all(Lie,Lpt,[],Lu,Ls,Abr).

deduction_all(Lie,[E1|Lpt],Li,Lu,Ls,Abr) :-
  E1 = (A,all(R,C)),
  E2 = (A,B,R),
  member(E2,Abr),
  nl, write('CALL ALL'),
  evolue((B,C), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
  Abr1 = Abr,
  affiche_evolution_Abox(Lie,[E1|Lpt],Li,Lu,Ls,Abr,Lie1,Lpt1,Li1,Lu1,Ls1,Abr1), !,
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr1),
  nl, write('EXIT ALL').
deduction_all(Lie,[],Li,Lu,Ls,Abr) :-
  transformation_or(Lie,[],Li,Lu,Ls,Abr).

transformation_or(Lie,Lpt,Li,[E|Lu],Ls,Abr) :-
  E = (A,or(C1,C2)),
  nl, write('CALL OR (1)'),
  evolue((A,C1), Lie, Lpt, Li, Lu, Ls, Lie1, Lpt1, Li1, Lu1, Ls1),
  Abr1 = Abr,
  affiche_evolution_Abox(Lie,Lpt,Li,[E|Lu],Ls,Abr,Lie1,Lpt1,Li1,Lu1,Ls1,Abr1), !,
  resolution(Lie1,Lpt1,Li1,Lu1,Ls1,Abr1),
  nl, write('EXIT OR (1)'),
  nl, write('CALL OR (2)'),
  evolue((A,C2), Lie, Lpt, Li, Lu, Ls, Lie2, Lpt2, Li2, Lu2, Ls2),
  Abr2 = Abr,
  affiche_evolution_Abox(Lie,Lpt,Li,[E|Lu],Ls,Abr,Lie2,Lpt2,Li2,Lu2,Ls2,Abr2), !,
  resolution(Lie2,Lpt2,Li2,Lu2,Ls2,Abr2),
  nl, write('EXIT OR (2)').

clash([(I,nothing)|Ls]) :-
  member((I,_),Ls),
  nl, write('CLASH').
clash([(I,C)|Ls]) :-
  nnf(not(C),NC),
  member((I,NC),Ls),
  nl, write('CLASH').
clash([_|Ls]) :- clash(Ls).

evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I,some(R,C))|Lie], Lpt, Li, Lu, Ls).
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls).
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Ls).
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Ls).
evolue(E, Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [E|Ls]).

affiche_evolution_Abox(Lie1, Lpt1, Li1, Lu1, Ls1, Abr1, Lie2,
Lpt2, Li2, Lu2, Ls2, Abr2) :-
  nl, write(' '),
  nl, write('   Lie'),
  affiche(Lie1,Lie2),
  nl, write('   Lpt'),
  affiche(Lpt1,Lpt2),
  nl, write('   Li'),
  affiche(Li1,Li2),
  nl, write('   Lu'),
  affiche(Lu1,Lu2),
  nl, write('   Ls'),
  affiche(Ls1,Ls2),
  nl, write('   Abr'),
  affiche(Abr1,Abr2),
  nl, write(' ').

affiche(L1,L2) :-
  affiche_plus(L1,L2),
  affiche_moins(L1,L2).

affiche_plus(L1,[E|L2]) :-
  member(E,L1),
  affiche_plus(L1,L2).
affiche_plus(L1,[E|L2]) :-
  nl, write('   + '), sublime(E),
  affiche_plus(L1,L2).
affiche_plus(_,[]).

affiche_moins([E|L1],L2) :-
  member(E,L2),
  affiche_moins(L1,L2).
affiche_moins([E|L1],L2) :-
  not(member(E,L2)),
  nl, write('   - '), sublime(E),
  affiche_moins(L1,L2).
affiche_moins([],_).

sublime((A,B,R)) :- write(A), write(' '), write(R), write(' '), write(B).
sublime((A,C)) :- write(A), write(' : '), sublime(C).
sublime(not(C)) :- write('¬('), sublime(C), write(')').
sublime(and(C1,C2)) :- write('('), sublime(C1), write(' ⊓ '), sublime(C2), write(')').
sublime(or(C1,C2)) :- write('('), sublime(C1), write(' ⊔ '), sublime(C2), write(')').
sublime(some(R,C)) :- write('∃'), write(R), write('.'), sublime(C).
sublime(all(R,C)) :- write('∀'), write(R), write('.'), sublime(C).
sublime(anything) :- write('⊤').
sublime(nothing) :- write('⊥').
sublime(C) :- write(C).




/**** CODE SUPPLEMENTAIRE ****/

compteur(1).

autoref(C,C) :- cnamea(C), !.
autoref(C,C) :- cnamena(C), !.
autoref(C,D) :- equiv(D,E), autoref(C,E), !.
autoref(C,and(D,_)) :- autoref(C,D), !.
autoref(C,and(_,D)) :- autoref(C,D), !.
autoref(C,or(D,_)) :- autoref(C,D), !.
autoref(C,or(_,D)) :- autoref(C,D), !.
autoref(C,not(D)) :- autoref(C,D), !.
autoref(C,some(_,D)) :- autoref(C,D), !.
autoref(C,all(_,D)) :- autoref(C,D), !.

nnf(not(anything), nothing).
nnf(not(nothing), anything).
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

enleve(X,[X|L],L) :-!.
enleve(X,[Y|L],[Y|L2]) :- enleve(X,L,L2).

concat([],L1,L1).
concat([X|Y],L1,[X|L2]) :- concat(Y,L1,L2).

genere(Nom) :-
  compteur(V), nombre(V,L1),
  concat([105,110,115,116],L1,L2),
  V1 is V+1,
  dynamic(compteur/1),
  retract(compteur(V)),
  dynamic(compteur/1),
  assert(compteur(V1)),
  name(Nom,L2).

nombre(0,[]).
nombre(X,L1) :-
  R is (X mod 10),
  Q is ((X-R)//10),
  chiffre_car(R,R1),
  char_code(R1,R2),
  nombre(Q,L),
  concat(L,[R2],L1).

chiffre_car(0,'0').
chiffre_car(1,'1').
chiffre_car(2,'2').
chiffre_car(3,'3').
chiffre_car(4,'4').
chiffre_car(5,'5').
chiffre_car(6,'6').
chiffre_car(7,'7').
chiffre_car(8,'8').
chiffre_car(9,'9').

lecture([X|L]):-
  read(X),
  X \= fin, !,
  lecture(L).
lecture([]).
