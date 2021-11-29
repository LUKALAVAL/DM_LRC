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

semantique(A) :- cnamea(A).
semantique(A) :- cnamena(A).
semantique(not(A)) :- semantique(A).
semantique(and(A,B)) :- semantique(A), semantique(B).
semantique(or(A,B)) :- semantique(A), semantique(B).
semantique(all(R,A)) :- rname(R), semantique(A).
semantique(some(R,A)) :- rname(R), semantique(A).
semantique(inst(I,C)) :- iname(I), cnamea(C).
semantique(inst(I,C)) :- iname(I), cnamena(C).
semantique(instR(I1,I2,R)) :- iname(I1), iname(I2), rname(R).

concept(C) :- cnamea(C), !.
concept(C) :- cnamena(C), !.
concept(not(C)) :- concept(C), !.
concept(or(C1,C2)) :- concept(C1), concept(C2), !.
concept(and(C1,C2)) :- concept(C1), concept(C2), !.
concept(some(_,C)) :- concept(C), !.
concept(all(_,C)) :- concept(C), !.
