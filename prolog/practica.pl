% --------------------------------------------- %
% ---------------- Ejercicio 1 ---------------- %
% --------------------------------------------- %
padre(juan,carlos).
padre(juan,luis).
padre(carlos,daniel).
padre(carlos,diego).
padre(luis,pablo).
padre(luis,manuel).
padre(luis,ramiro).

%abuelo(?X,?Y)
abuelo(X,Y) :- padre(X,Z), padre(Z,Y).

%hijo(?X,?Y)
hijo(X,Y) :- padre(Y,X).

%hermano(?X,?Y)
hermano(X,Y) :- padre(Z,X), padre(Z,Y), X \= Y.

%descendiente(?X,?Y)
descendiente(X,Y) :- hijo(X,Y).
descendiente(X,Y) :- hijo(X,Z), descendiente(Z,Y).

%ancestro(X,X).
ancestro(X,Y) :- padre(X,Y).
%ancestro(X,Y) :- padre(Z,Y), padre(X,Z).
ancestro(X,Y) :-  padre(X,Z), ancestro(Z,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 2 ---------------- %
% --------------------------------------------- %
vecino(X,Y,[_|Ls]) :- vecino(X,Y,Ls).
vecino(X,Y, [X|[Y|_]]).

% --------------------------------------------- %
% ---------------- Ejercicio 3 ---------------- %
% --------------------------------------------- %
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X).
menorOIgual(X,suc(Y)) :- menorOIgual(X,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 4 ---------------- %
% --------------------------------------------- %
%concatenar(?Lista1,?Lista2,?Lista3)
concatenar([],Xs,Xs).
%concatenar(Xs,[],Xs).
concatenar([X|Xs],Ys,[X|Ls]) :- concatenar(Xs,Ys,Ls).

% --------------------------------------------- %
% ---------------- Ejercicio 5 ---------------- %
% --------------------------------------------- %
%I)
%last(?L,?U)
%last([U],U).
%last([_|Xs],U) :- last(Xs,U).
last(L,U) :- concatenar(_,[U],L).

%II)
%reverse(+L1,-L2)
reverse([],[]).
reverse([X|Xs],Ls) :- reverse(Xs,Ys), concatenar(Ys,[X],Ls).

%III)
%maxList(+L,-M)
maxList([X],X).
maxList([X|Xs],M) :- maxList(Xs,X1), M is max(X,X1).
%minList(+L,-M)
minList([X],X).
minList([X|Xs],M) :- minList(Xs,X1), M is min(X,X1).

%IV)
%prefijo(?P,+L)
prefijo(Xs,L) :- concatenar(Xs,_,L), Xs \= [].

%V)
%sufijo(?P,+L)
sufijo(Xs,L) :- concatenar(_,Xs,L), Xs \= [].

%VI)
%sublista(?P,+L)
sublista([],[]).
sublista(L,Xs) :- sufijo(P,Xs), prefijo(L,P), Xs \= [].

%VI)
pertenece(X,L) :- sublista([X],L).

% --------------------------------------------- %
% ---------------- Ejercicio 6 ---------------- %
% --------------------------------------------- %
%aplanar(+Xs,-Yx)
aplanar([],[]).
aplanar([[]|Xs],Ys) :- aplanar(Xs,Ys).
%aplanar([X],[X]) :- X \= [_|_].
aplanar([X|Xs],Ys) :- X \=[] , X \= [_|_], aplanar(Xs,Y), concatenar([X],Y,Ys).
aplanar([X|Xs],Ys) :- X = [_|_], aplanar(X,Y), aplanar(Xs,Ls), concatenar(Y,Ls,Ys).

% --------------------------------------------- %
% ---------------- Ejercicio 7 ---------------- %
% --------------------------------------------- %
%palindromo(+L1,-L2)
palindromo(L1,Res) :- reverse(L1,L2), concatenar(L1,L2,Res).

%doble(?L1,?L2)
doble([],[]).
doble([X|Xs],Ys) :- Ys = [X,X|L], doble(Xs,L).

%iesimo(?I, +L, -X)
iesimo(1,[X|_],X).
iesimo(N,[_|Xs],Y) :- N > 1, B is N - 1, iesimo(B,Xs,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 8 ---------------- %
% --------------------------------------------- %
desde(X,X).
desde(X,Y) :- nonvar(Y), Y > X.
desde(X,Y) :- var(Y), N is X + 1, desde(N,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 9 ---------------- %
% --------------------------------------------- %
%interseccion(+L1, +L2, -L3)
interseccion(_,[],[]).
interseccion([],_,[]).
interseccion([X|Xs], L, [X|Ys]) :- pertenece(X,L), borrar(L,X,L2), borrar(Xs,X,L3), interseccion(L3,L2,Ys).
interseccion([X|Xs], L, Ys) :- not(pertenece(X,L)), borrar(Xs,X,L3), interseccion(L3,L,Ys).

%split(N, L, L1, L2)
%split(0,L,[],L).
split(N,L,L1,L2) :- length(L,Len), Len >= N, length(L1,N), concatenar(L1,L2,L).
    
%borrar(+L, +E, -L2)
borrar([],_,[]).
borrar([X|Xs], X, Ys) :- borrar(Xs,X,Ys).
borrar([X|Xs], E, [X|Ys]) :- X \= E, borrar(Xs,E,Ys).

%sacarDuplicados+L1, -L2)
sacarDuplicados([],[]).
sacarDuplicados([X|Xs],[X|Ys]) :- borrar(Xs,X,Ls), sacarDuplicados(Ls,Ys).

%reparto(+L, +N, -Llistas)
reparto(Xs, 1, [Xs]).
%reparto([],N,[[]|Ys]) :- N > 1, N1 is N - 1, reparto([],N1,Ys). 
reparto(L, N,[L1|Ys]) :- N > 1, append(L1,L2,L), N1 is N - 1, reparto(L2,N1,Ys).

%repartoSinVacias(+L, -Llistas)
repartoSinVacias([], [[]]).
repartoSinVacias(Xs,[L1|Ls]) :- length(Xs,N), N > 0, append(L1,L2,Xs), L1 \= [], repartoSinVacias(L2,Ys), borrar(Ys,[],Ls).

% --------------------------------------------- %
% ---------------- Ejercicio 10 --------------- %
% --------------------------------------------- %
%intercalar(L1, L2, L3)
intercalar([],L2,L2).
intercalar(L1,[],L1) :- L1 \= [].
intercalar([X|Xs],[Y|Ys], [X,Y|Ls]) :- intercalar(Xs,Ys,Ls).

% --------------------------------------------- %
% ---------------- Ejercicio 11 --------------- %
% --------------------------------------------- %

% PREGUNTAR

% --------------------------------------------- %
% ---------------- Ejercicio 12 --------------- %
% --------------------------------------------- %
vacio(nill).

raiz(bin(_,R,_),R).

altura(nill,0).
altura(bin(I,_,D), Res) :- altura(I,N1), altura(D,N2), Res is 1 + max(N1,N2).

cantidadDeNodos(nill,0).
cantidadDeNodos(bin(I,_,D), Res) :- cantidadDeNodos(I,N1), cantidadDeNodos(D,N2), Res is 1 + N1 + N2.

% --------------------------------------------- %
% ---------------- Ejercicio 13 --------------- %
% --------------------------------------------- %
%inorder(+AB, -Lista)
inorder(nill,[]).
inorder(bin(I,R,D), Res) :- inorder(I,Is), inorder(D,Ds), concatenar(Is,[R],L), concatenar(L,Ds,Res).

% --------------------------------------------- %
% ---------------- Ejercicio 14 --------------- %
% --------------------------------------------- %
coprimos(X,Y) :- desde(2,N), between(2,N,X), Y is N - X, Y \= 1, 1 is gcd(X,Y).

% --------------------------------------------- %
% ---------------- Ejercicio 15 --------------- %
% --------------------------------------------- %
listaHasta(_,[]).
listaHasta(Cota, [N|Ls]) :- between(0,Cota,N), listaHasta(Cota, Ls).

cumplen([],_,_).
cumplen([X|Xs],I,Lon) :- length(X,Lon), listaHasta(I,X), sumlist(X,I), cumplen(Xs,I,Lon).

cuadradoSemiLatino(N,L) :- N > 0, desde(0,I), length(L,N), cumplen(L,I,N). 

% --------------------------------------------- %
% ---------------- Ejercicio 16 --------------- %
% --------------------------------------------- %
%I)
esCompatible(A,B,C) :- A > 0, A < B + C, Diferencia is B - C, A > abs(Diferencia).

esTrianulo(tri(A,B,C)) :- esCompatible(A,B,C), esCompatible(B,A,C), esCompatible(C,A,B).

%II)
%perimetro(?T,?P)
perimetro(tri(A,B,C),P) :- desde(3,P), between(1,P,A), between(1,P,B), between(1,P,C), esTrianulo(tri(A,B,C)), P is A + B + C.

perimetroSinCongruentes(tri(A,B,C),P) :- desde(3,P), between(1,P,A), between(1,A,B), between(1,B,C), esTrianulo(tri(A,B,C)), P is A + B + C.

%III)
%triangulo(-T)
triangulo(T) :- desde(0,P), perimetro(T,P).

trianguloSinConcruentes(T) :- desde(0,P), perimetroSinCongruentes(T,P).

% --------------------------------------------- %
% ---------------- Ejercicio 17 --------------- %
% --------------------------------------------- %
diferenciaSimetrica([],L2,L2).
diferenciaSimetrica(L1,[],L1) :- L1 \= [].
diferenciaSimetrica([X|Xs], L2, L3) :- member(X,L2), borrar(L2,X,Ls), diferenciaSimetrica(Xs,Ls,L3).
diferenciaSimetrica([X|Xs], L2, [X|L3]) :- not(member(X,L2)), diferenciaSimetrica(Xs,L2,L3).

% --------------------------------------------- %
% ---------------- Ejercicio 18 --------------- %
% --------------------------------------------- %
%I) 
%II)
%III)

% --------------------------------------------- %
% ---------------- Ejercicio 19 --------------- %
% --------------------------------------------- %

diff(L1,L2,D) :- sumlist(L1,N1), sumlist(L2,N2), D is abs(N1 - N2).

corteMasParejo(L1, L2, L3) :- append(L2,L3,L1), diff(L2,L3,Dif),  not((append(L4,L5,L1), diff(L4,L5,Dif2), Dif2 < Dif)).

% --------------------------------------------- %
% ---------------- Ejercicio 21 --------------- %
% --------------------------------------------- %


% --------------------------------------------- %
% ---------------- Ejercicio 22 --------------- %
% --------------------------------------------- %


% --------------------------------------------- %
% ---------------- Ejercicio 23 --------------- %
% --------------------------------------------- %



