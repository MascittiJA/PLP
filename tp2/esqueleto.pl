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

%colocar(+?Tablero,+Elemento,+Fila,+Columna,?Tablero)
%colocar(T,E,F,C,T1) :- elemento(E), disponible(T,F,C), .

%colocarBarco(+Barco,+Direccion,+?Tablero,+Fila,+Columna)
%colocarBarco(0,_,_,_,_).
%colocarBarco(B,D,T,F,C,T1) :- B > 0, direccion(D), vertical = D, contenido(T,F,C,X), X = o, N is B - 1, F1 is F + 1, colocarBarco(N,D,T1,F1,C).
%colocarBarco(B,D,T,F,C,T1) :- B > 0, direccion(D), horizontal = D, contenido(T,F,C,X), X = o, N is B - 1, C1 is C + 1, colocarBarco(N,D,T1,F,C1).

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T,F,C,X) :- enRango(T,F,C), dameElEnesimo(T,F,Fila), dameElEnesimo(Fila,C,X).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T,F,C) :- contenido(T,F,C,X1), X1 \= o.%, adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2), X2 \= o.
%disponible(T,F,C) :- contenido(T,F,C,X1), X1 \= o, setof(X2 \= o, (adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2)), AdyacentesLimpios), not(member(false, AdyacentesLimpios)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(N,D,T,F,C) :- N  > 0, direccion(D), vertical = D, setof(disponible(T,F,C)), N1 is N - 1, F1 is F + 1, puedoColocar(N1,D,T,F1,C).
puedoColocar(N,D,T,F,C) :- N  > 0, direccion(D), horizontal = D, disponible(T,F,C), N1 is N - 1, C1 is C + 1, puedoColocar(N1,D,T,F,C1).

%ubicarBarcos(+Barcos, +?Tablero)
%ubicarBarcos([],T).
%ubicarBarcos([Bcantidad|Bs],T) :- puedoColocar(Bcantidad,vertical,T,F1,C1), ).

%completarConAgua(+?Tablero)

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
