:- [pred].
compteur(1).

programme :-
  premiere_etape(Tbox,Abi,Abr),
  deuxieme_etape(Abi,Abi1,Tbox),
  troisieme_etape(Abi1,Abr).



premiere_etape(Tbox,Abi,Abr) :-
  make_tbox(Tbox),
  make_abi(Abi),
  make_abr(Abr).

make_tbox(Tbox) :- setof((C,D), equiv(C,D), Tbox).
make_abi(Abi) :- setof((I,C), inst(I,C), Abi).
make_abr(Abr) :- setof((I1,I2,R), instR(I1,I2,R), Abr).




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
