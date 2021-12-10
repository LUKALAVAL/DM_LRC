:- [box].

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
suite(R,Abi,Abi1,Tbox) :-
  nl, write('Cette reponse est incorrecte.'),
  nl, saisie_et_traitement_prop_a_demontrer(Abi,Abi1,Tbox).

acquisition_prop_type1(Abi,Abi1,Tbox) :-
  nl, write('Entrez une instance :'),
  nl, read(I), instance(I),
  nl, write('Entrez un concept :'),
  nl, read(C), concept(C),
  remplace(C,CC),
  nnf(not(CC),NCC),
  Abi1 = [(I,NCC)|Abi].

acquisition_prop_type2(Abi,Abi1,Tbox) :-
  nl, write('Entrez un concept (1) :'),
  nl, read(C1), concept(C1),
  nl, write('Entrez un concept (2) :'),
  nl, read(C2), concept(C2),
  remplace(and(C1,C2),CC),
  nnf(CC,NCC),
  genere(I),
  Abi1 = [(I,NCC)|Abi].

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





/**** TROISIEME PARTIE ****/

troisieme_etape(Abi,Abr) :-
  tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls),
  resolution(Lie,Lpt,Li,Lu,Ls,Abr),
  nl, write('Youpiiiiii, on a demontre la proposition initiale !!!').

tri_Abox([],[],[],[],[],[]).
tri_Abox([(I,some(R,C))|Abi],[(I,some(R,C))|Lie],Lpt,Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,all(R,C))|Abi],Lie,[(I,all(R,C))|Lpt],Li,Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,and(C1,C2))|Abi],Lie,Lpt,[(I,and(C1,C2))|Li],Lu,Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([(I,or(C1,C2))|Abi],Lie,Lpt,Li,[(I,or(C1,C2))|Lu],Ls) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).
tri_Abox([E|Abi],Lie,Lpt,Li,Lu,[E|Ls]) :- tri_Abox(Abi,Lie,Lpt,Li,Lu,Ls).

resolution(Lie,Lpt,Li,Lu,Ls,Abr) :- clash(Ls), nl, write('clash').
resolution(Lie,Lpt,Li,Lu,Ls,Abr) :- complete_some(Lie,Lpt,Li,Lu,Ls,Abr).

complete_some([(A,some(R,C))|Lie_],Lpt,Li,Lu,Ls,Abr) :-
  genere(B),
  Lie = [(A,some(R,C))|Lie_],
  Ls_ = [(B,C)|Ls],
  Abr_ = [(A,B,R)|Abr],
  affiche_evolution_Abox(Lie,Lpt,Li,Lu,Ls,Abr,Lie_,Lpt,Li,Lu,Ls_,Abr_),
  resolution(Lie_,Lpt,Li,Lu,Ls_,Abr_).
complete_some([],Lpt,Li,Lu,Ls,Abr) :-
  transformation_and([],Lpt,Li,Lu,Ls,Abr).

transformation_and(Lie,Lpt,[(A,and(C1,C2))|Li_],Lu,Ls,Abr) :-
  Li = [(A,and(C1,C2))|Li_],
  Ls_ = [(A,C1)|[(A,C2)|Ls]],
  affiche_evolution_Abox(Lie,Lpt,Li,Lu,Ls,Abr,Lie,Lpt,Li_,Lu,Ls_,Abr),
  resolution(Lie,Lpt,Li_,Lu,Ls_,Abr).
transformation_and(Lie,Lpt,[],Lu,Ls,Abr) :-
  deduction_all(Lie,Lpt,[],Lu,Ls,Abr).

deduction_all(Lie,[(A,all(R,C))|Lpt_],Li,Lu,Ls,[(A,B,R)|Abr_]) :-
  Lpt = [(A,all(R,C))|Lpt_],
  Abr = [(A,B,R)|Abr_],
  Ls_ = [(B,C)|Ls],
  affiche_evolution_Abox(Lie,Lpt,Li,Lu,Ls,Abr,Lie,Lpt,Li_,Lu,Ls_,Abr),
  resolution(Lie,Lpt,Li,Lu,Ls_,Abr).
deduction_all(Lie,[],Li,Lu,Ls,Abr) :-
  transformation_or(Lie,[],Li,Lu,Ls,Abr).

transformation_or(Lie,Lpt,Li,[(A,or(C1,C2))|Lu_],Ls,Abr) :-
  Lu = [(A,or(C1,C2))|Lu_],
  Ls_1 = [(A,C1)|Ls],
  affiche_evolution_Abox(Lie,Lpt,Li,Lu,Ls,Abr,Lie,Lpt,Li,Lu_,Ls_1,Abr),
  resolution(Lie,Lpt,Li,Lu_,Ls_1,Abr),
  Ls_2 = [(A,C2)|Ls],
  affiche_evolution_Abox(Lie,Lpt,Li,Lu,Ls,Abr,Lie,Lpt,Li,Lu_,Ls_2,Abr),
  resolution(Lie,Lpt,Li,Lu_,Ls_2,Abr).

clash([(I,C)|Ls]) :- nnf(not(C),NC), member((I,NC),Ls).
clash([(I,C)|Ls]) :- clash(Ls).

affiche_evolution_Abox(Ls1, Lie1, Lpt1, Li1, Lu1, Abr1, Ls2, Lie2,
Lpt2, Li2, Lu2, Abr2) :-
  nl, write(" "),
  nl, write("Ls"),
  affiche(Ls1,Ls2),
  nl, write("Lpt"),
  affiche(Lpt1,Lpt2),
  nl, write("Li"),
  affiche(Li1,Li2),
  nl, write("Lu"),
  affiche(Lu1,Lu2),
  nl, write("Abr"),
  affiche(Abr1,Abr2).

affiche(L1,L2) :-
  affiche_plus(L1,L2),
  affiche_moins(L1,L2).

affiche_plus(L1,[E|L2]) :-
  member(E,L1),
  affiche_plus(L1,L2).
affiche_plus(L1,[E|L2]) :-
  nl, write('+ '), write(E),
  affiche_plus(L1,L2).
affiche_plus(_,[]).

affiche_moins([E|L1],L2) :-
  member(E,L2),
  affiche_moins(L1,L2).
affiche_moins([E|L1],L2) :-
  not(member(E,L2)),
  nl, write('- '), write(E),
  affiche_moins(L1,L2).
affiche_moins([],_).






/**** CODE SUPPLEMENTAIRE ****/

compteur(1).

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
  assert(compteur(V1)),nl,nl,nl,
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

evolue((I,some(R,C)), Lie, Lpt, Li, Lu, Ls, [(I,some(R,C))|Lie], Lpt, Li, Lu, Ls).
evolue((I,all(R,C)), Lie, Lpt, Li, Lu, Ls, Lie, [(I,all(R,C))|Lpt], Li, Lu, Ls).
evolue((I,and(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, [(I,and(C1,C2))|Li], Lu, Ls).
evolue((I,or(C1,C2)), Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, [(I,or(C1,C2))|Lu], Ls).
evolue(E, Lie, Lpt, Li, Lu, Ls, Lie, Lpt, Li, Lu, [E|Ls]).
