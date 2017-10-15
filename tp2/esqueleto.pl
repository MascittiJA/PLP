%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Funciones auxiliares:------------------%

%direcciones
direccion(vertical).
direccion(horizontal).

%elementos
elemento(o).
elemento(~).

%dameElEnesimo(Arrego, N, X)
dameElEnesimo([X|_], 1, X).
dameElEnesimo([_|Xs], N, Res) :- N > 0, length([_|Xs], Long), N =< Long, N2 is N - 1, dameElEnesimo(Xs, N2, Res).

%colocarBarco(+Barco,+Direccion,+?Tablero,+Fila,+Columna,-TableroNuevo)
colocarBarco(0,_,_,_,_).
colocarBarco(B,D,T,F,C) :- B > 0, direccion(D), vertical = D, B1 is B - 1, F1 is F + 1, contenido(T,F,C,X1), X1 = o, colocarBarco(B1,D,T,F1,C).
colocarBarco(B,D,T,F,C) :- B > 0, direccion(D), horizontal = D, B1 is B - 1, C1 is C + 1,contenido(T,F,C,X1), X1 = o, colocarBarco(B1,D,T,F,C1).

%recorrerTablero(+?Tablero,?Fila,?Columna)
recorrerTablero(T,F,C) :- length(T,Filas), nth1(1,T,Fila), length(Fila,Columnas), N is Filas + Columnas, between(2,N,Suma), between(1,Suma,F), C is Suma - F, C \= 0, enRango(T,F,C).

%agua(?X,?Y)
agua(X,Y) :- var(X), (Y = ~).
agua(X,Y) :- nonvar(X), Y = X.


copiarCelda(T,F,C,Tnew) :- contenido(T,F,C,X) = contenido(Tnew,F,C,X).

copiarTablero(T,Tnew) :- recorrerTablero(T,F1,C1), copiarCelda(T,F1,C1,Tnew).
%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
%contenido(T,F,C,X) :- enRango(T,F,C), dameElEnesimo(T,F,Fila), dameElEnesimo(Fila,C,X).
contenido(T,F,C,X) :- enRango(T,F,C), nth1(F,T,Fila), nth1(C,Fila,X).

%disponible(+Tablero, ?Fila, ?Columna)
%disponible(T,F,C) :- contenido(T,F,C,X1), var(X1), adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2), var(X2).
%disponible(T,F,C) :- contenido(T,F,C,X1), var(X1), setof(var(X2), (adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2)), Set), length(Set, Long), Long = 1.
%disponible(T,F,C,N) :- contenido(T,F,C,X1), var(X1), setof(var(X2), (adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2)), Set), length(Set, Long), Long = 1, N = Set.
disponible(T,F,C) :- contenido(T,F,C,X1), var(X1), not(not(not((adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2), nonvar(X2))))).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(0,_,_,_,_).
puedoColocar(N,D,T,F,C) :- N  > 0, direccion(D), vertical = D, disponible(T,F,C), N1 is N - 1, F1 is F + 1, puedoColocar(N1,D,T,F1,C).
puedoColocar(N,D,T,F,C) :- N  > 0, direccion(D), horizontal = D, disponible(T,F,C), N1 is N - 1, C1 is C + 1, puedoColocar(N1,D,T,F,C1).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([],_).
ubicarBarcos([Bcantidad|Bs],T) :- recorrerTablero(T,F,C), puedoColocar(Bcantidad,horizontal,T,F,C), colocarBarco(Bcantidad,horizontal,T,F,C), ubicarBarcos(Bs,T) .
ubicarBarcos([Bcantidad|Bs],T) :- recorrerTablero(T,F,C), puedoColocar(Bcantidad,vertical,T,F,C), not(puedoColocar(Bcantidad,horizontal,T,F,C)), colocarBarco(Bcantidad,vertical,T,F,C), ubicarBarcos(Bs,T) .

%completarConAgua(+?Tablero)
%completarConAgua(T) :- not(not((recorrerTablero(T,F1,C1), contenido(T,F1,C1,X1), var(X1)))), recorrerTablero(T,F,C), contenido(T,F,C,~), completarConAgua(T).
completarConAgua(T) :- maplist(maplist(agua),T,T).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T,F,C,T) :- enRango(T,F,C), contenido(T,F,C,~).
%golpear(T,F,C,Tnew) :- enRango(T,F,C), contenido(Tnew,F,C,~), recorrerTablero(T,F1,C1), (F,C) \= (F1,C1), contenido(T,F1,C1,Copiar), contenido(Tnew,F1,C1,Copiar).
golpear(T,F,C,Tnew) :- enRango(T,F,C), contenido(Tnew,F,C,~), forall((recorrerTablero(T,F1,C1), (F,C) \= (F1,C1)), (contenido(T,F1,C1,Copiar), contenido(Tnew,F1,C1,Copiar))).



% Completar instanciación soportada y justificar.
% ----------------------------------------------------------------------------------------------------------- %
% El predicado golpear necesita Tablre, Fila, Columna instanciados, con lo cual este tambien pues los utiliza
% Luego Resultado siempre está bien definido, con lo cual puede venir instanciado o no.
% ----------------------------------------------------------------------------------------------------------- %
%atacar(+Tablero, +Fila, +Columna, ?Resultado, -NuevoTab)
atacar(T,F,C,Res,Tnew) :- contenido(T,F,C,~), golpear(T,F,C,Tnew), Res = agua.
atacar(T,F,C,Res,Tnew) :- contenido(T,F,C,o), golpear(T,F,C,Tnew), not(not(adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,o))), Res = tocado.
atacar(T,F,C,Res,Tnew) :- contenido(T,F,C,o), golpear(T,F,C,Tnew), not(not(not(adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,o)))), Res = hundido.

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :- matriz(M,3,3), contenido(M,1,1,o), not(disponible(M,1,1)).
test(4) :- matriz(M,3,3), contenido(M,1,1,o), not(disponible(M,1,2)).
test(5) :- matriz(M,3,3), contenido(M,1,1,o), not(disponible(M,2,1)).
test(6) :- matriz(M,3,3), contenido(M,1,1,o), not(disponible(M,2,2)).
test(7) :- matriz(M,3,3), contenido(M,1,1,o), disponible(M,1,3).
test(8) :- matriz(M,3,3), contenido(M,1,1,o), disponible(M,2,3).
test(9) :- matriz(M,3,3), contenido(M,1,1,o), disponible(M,3,3).
test(10) :- matriz(M,3,3), contenido(M,1,1,o), disponible(M,3,2).
test(11) :- matriz(M,3,3), contenido(M,1,1,o), disponible(M,3,1).
test(12) :- matriz(M,2,2), puedoColocar(2,horizontal,M,1,1).
test(13) :- matriz(M,2,2), puedoColocar(2,horizontal,M,2,1).
test(14) :- matriz(M,2,2), not(puedoColocar(2,horizontal,M,2,2)).
test(15) :- matriz(M,2,2), not(puedoColocar(2,horizontal,M,1,2)).
test(16) :- matriz(M,3,2), setof(M, (ubicarBarcos([2,1],M), completarConAgua(M)), Set),
    Set = [[[o, o], [~, ~], [o, ~]], [[o, o], [~, ~], [~, o]], [[o, ~], [~, ~], [o, o]], [[~, o], [~, ~], [o, o]]].
test(17) :- matriz(M,3,3), completarConAgua(M), 
    M = [[~, ~, ~], [~, ~, ~], [~, ~, ~]].
test(18) :- matriz(M,3,3), contenido(M,1,1,o), contenido(M,2,2,o), contenido(M,3,3,o), completarConAgua(M),
    M = [[o, ~, ~], [~, o, ~], [~, ~, o]].
tests :- forall(between(1,18,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
