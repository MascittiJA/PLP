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

%------------------Predicados auxiliares:------------------%

%restarYsumarUno(N,B,N1,B1)	
restarYsumarUno(N,B,N1,B1) :- N > 0, N1 is N - 1, B1 is B + 1.

%colocarBarco(+Barco,+Direccion,+?Tablero,+Fila,+Columna,-TableroNuevo)
colocarBarco(0,_,_,_,_).
colocarBarco(B,D,T,F,C) :-
	D = vertical,
	restarYsumarUno(B,F,B1,F1),
	contenido(T,F,C,o),
	colocarBarco(B1,D,T,F1,C).
colocarBarco(B,D,T,F,C) :-
	D = horizontal,
	restarYsumarUno(B,C,B1,C1),
	contenido(T,F,C,o),
	colocarBarco(B1,D,T,F,C1).

%recorrerTablero(+?Tablero,?Fila,?Columna)
recorrerTablero(T,F,C) :-
	length(T,Filas),
	nth1(1,T,Fila),
	length(Fila,Columnas),
	N is Filas + Columnas,
	between(2,N,Suma),
	between(1,Suma,F),
	C is Suma - F,
	C \= 0,
	enRango(T,F,C).


%aguaSiEstoyLibre(?X,?Y)
aguaSiEstoyLibre(X,Y) :- var(X), (Y = ~).
aguaSiEstoyLibre(X,X) :- nonvar(X).


%igualSalvoIesimo(+Xs,+I,-Ys)
igualSalvoIesimo(Xs,I,Ys) :-
	append(Principio, [_|Ultimos], Xs),
	append(Principio, [_|Ultimos], Ys),
	length(Principio, N),
	N is I - 1.

%------------------Predicados a definir:------------------%

%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T,F,C,X) :- nth1(F,T,Fila), nth1(C,Fila,X).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T,F,C) :-
	contenido(T,F,C,X1),
	var(X1),
	forall((adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,X2)), var(X2)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(0,_,_,_,_).
puedoColocar(N,vertical,T,F,C) :- 
	disponible(T,F,C),
	restarYsumarUno(N,F,N1,F1),
	puedoColocar(N1,vertical,T,F1,C).
puedoColocar(N,horizontal,T,F,C) :-
	disponible(T,F,C),
	restarYsumarUno(N,C,N1,C1),
	puedoColocar(N1,horizontal,T,F,C1).

colocarBarcosSiPuedo(Bcantidad,T,Direccion,F,C) :-
%	recorrerTablero(T,F,C),
	puedoColocar(Bcantidad,Direccion,T,F,C),
	colocarBarco(Bcantidad,Direccion,T,F,C).
%	ubicarBarcos(Bs,T).
    
	
%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([],_).
ubicarBarcos([Bcantidad|Bs],T) :-
	recorrerTablero(T,F,C),
    colocarBarcosSiPuedo(Bcantidad,T,horizontal,F,C),
	ubicarBarcos(Bs,T) .
ubicarBarcos([Bcantidad|Bs],T) :-
	recorrerTablero(T,F,C),
	not(puedoColocar(Bcantidad,horizontal,T,F,C)),
    colocarBarcosSiPuedo(Bcantidad,T,vertical,F,C),
	ubicarBarcos(Bs,T) .

%completarConAgua(+?Tablero)
completarConAgua(T) :- maplist(maplist(aguaSiEstoyLibre),T,T).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T,F,C,T) :- contenido(T,F,C,~).
golpear(T,F,C,Tnew) :-
	not(contenido(T,F,C,~)),
	igualSalvoIesimo(T,F,Tnew),
	nth1(F,T,FilaVieja),
	nth1(F,Tnew,FilaNueva),
	igualSalvoIesimo(FilaVieja,C,FilaNueva),
	nth1(C,FilaNueva,~).


% Completar instanciación soportada y justificar.
% ----------------------------------------------------------------------------------------------------------- %
% El predicado contenido(+?T, ?F, ?C, ?E) necesita un tablero parcialemente instanciado, pero luego todos sus parametros son reversibles. Luego de verificar contenido, T, F y C quedan instanciadas. Por lo tanto el resto del predicado tambien tiene sus parametros correctamente instanciados. Tanto golpear(+T, +F, +C, -T1), como adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2) estan correctamente instanciados a la hora de evaluarse.
% Por lo tanto este predicado es reversible en todos sus parámetros, pero necesita un tablero parcialmente instanciado para no quedar colgado instanciando todos los tableros posibles. 
% ----------------------------------------------------------------------------------------------------------- %
%atacar(+?Tablero, ?Fila, ?Columna, ?Resultado, ?NuevoTab)
atacar(T,F,C,agua,T) :- contenido(T,F,C,~).
atacar(T,F,C,tocado,Tnew) :-
	contenido(T,F,C,o),
	not(forall(adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,~))),
	golpear(T,F,C,Tnew).
atacar(T,F,C,hundido,Tnew) :-
	contenido(T,F,C,o),
	forall(adyacenteEnRango(T,F,C,F1,C1), contenido(T,F1,C1,~)),
	golpear(T,F,C,Tnew).

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(22) :- matriz(M,3,3), forall(recorrerTablero(M,F,C), disponible(M,F,C)).
test(23) :- matriz(M,3,3), contenido(M,2,2,o), forall(recorrerTablero(M,F,C), not(disponible(M,F,C))).
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
test(24) :- Tablero = [[o, o], [_, _], [_, o]], completarConAgua(Tablero),
    Tablero = [[o, o], [~, ~], [~, o]].
test(19) :- atacar([[o, o, ~], [~, ~, ~], [~, ~, o]],1,1,Res,T), T = [[~, o, ~], [~, ~, ~], [~, ~, o]], Res = tocado.
test(20) :- atacar([[o, o, ~], [~, ~, ~], [~, ~, o]],2,2,Res,T), T = [[o, o, ~], [~, ~, ~], [~, ~, o]], Res = agua.
test(21) :- atacar([[o, o, ~], [~, ~, ~], [~, ~, o]],3,3,Res,T), T = [[o, o, ~], [~, ~, ~], [~, ~, ~]], Res = hundido.
tests :- forall(between(1,24,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.


