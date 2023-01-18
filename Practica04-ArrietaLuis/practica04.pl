% Arrieta Mancera Luis Sebastian 318174116
% Gongora Ramírez Dania Paula  318128274
% Martínez Hernández Zuriel Enrique 318056423
% --------------------------------------------------%
%          Practica 4 - Introdución a Prolog        %
% --------------------------------------------------%

% --------------------------------------------------%
%      Sintáxis básica y bases de conocimiento      %
% --------------------------------------------------%

% A continuación tenemos algunos hechos sobre animales

es_perro(blacky).  %blacky es un perro
es_perro(rizzo).
es_perro(rufus).
es_gato(masapan).  
es_gato(waffle).
es_gato(curie).
es_gato(crepita).
es_raton(boris).
es_raton(matilda).

mas_grande(elefante, caballo).
mas_grande(caballo, perro).
mas_grande(perro, raton).
mas_grande(raton, hormiga).

% Aquí hay un ejemplo de función recursiva en prolog
% El caso base 
mucho_mas_grande(X,Y) :- mas_grande(X, Y).
mucho_mas_grande(X,Y) :- mas_grande(X, Z), mucho_mas_grande(Z, Y).

% --------------------------------------------------%
%          1-Parentesco, árbol sld y grafos         %
% --------------------------------------------------%
progenitorde(martin,luis).
progenitorde(luis,jose).
progenitorde(luis, pedro).
progenitorde(ursula, aureliano).
progenitorde(arcadio, aureliano).
progenitorde(ursula, remedios).
progenitorde(arcadio,remedios).
progenitorde(ursula, amaranta).
progenitorde(arcadio,amaranta).
progenitorde(aureliano, mauricio).
progenitorde(fernanda, mauricio).
progenitorde(aureliano, renata).
progenitorde(fernanda, renata).

% EJERCICIO 1.1
eshijode(A,B)   :- progenitorde(B,A).
% EJERCICIO 1.2
esabuelode(A,B) :- eshijode(B,Z),progenitorde(A,Z).
% EJERCICIO 1.3
eshermanode(A,B) :- progenitorde(Z,A), progenitorde(Z,B).
% EJERCICIO 1.4
familiarde(A,B) :- progenitorde(A,B);eshijode(A,B);esabuelode(A,B);esabuelode(B,A);eshermanode(A,B).

% Ejercicio 2.1 (Extra)
% suma de los primeros n naturales
suma(0,0).
suma(X,Res) :- Y is X - 1, suma(Y,S), Res is X + S.

% Investigacion de arboles SLD
% La resolucion-SLD: Resolucion Lineal con funcion de Seleccion para clausulas Definidas.
% Es un caso particular de la resolucion general donde:
% Los resolventes son siempre objetivos
% Los programas son conjuntos de clausulas de Horn, es decir, son hechos y reglas
% Hay que seleccionar un atomo al que aplicar la resolucion.
% --------------------------------------------------------
% Desarrollamos el  arbol para la consulta suma(2,R):
%
% suma(2,R) :- Y is 2 -1, suma(Y,S), R is 2 + S
%                   |
% suma(1,S) :- K is 1-1, suma(K,P), S is 1 + P
%                   |
%                   | P is 0
%                   |
%               suma(0,0).
%                   |
%                   []
% Haciendo la sustitucion tenemos:
% P = 0
% S = 1 + P
% R = 2 + S 
% => R = 2 + (1 + P) => R = 2 + (1 + (0)) => R = 2 + (1) => R = 3
%
% Referencia:
% https://www.cs.us.es/cursos/lp/temas/tema-09.pdf
% :)

% Ejercicio 3.1
arista(d,i,15).
arista(i,f,11).
arista(f,a,8).
arista(a,b,7).
arista(f,g,10).
arista(g,c,7).
arista(d,h,4).
arista(h,f,9).

% Ejercicio 3.2
camino(X,X).
camino(X,Y) :- arista(X,Y,R).
camino(X,Y) :- arista(X,Z,R), camino(Z,Y).

% Ejercicio 3.3
costo(X,X,R) :- R is 0.
costo(X,Y,R) :- arista(X,Y,Z), R is Z,!. 
costo(X,Y,R) :- arista(X,Z,W), costo(Z,Y,V), R is W + V.

% Una vez que se llega el resultado el arbol hace backtracking buscando otra clausula
% eso genera otra rama en el arbol, el signo de admiracion es un corte de esa rama 
% extra que se genero.

% --------------------------------------------------%
%                2-Números naturales                %
% --------------------------------------------------%

esNatural(c).
esNatural(s(X)) :- esNatural(X).

% SUMA
suma(c,X,X) :- esNatural(X).
suma(s(X),Y,s(Z)) :- suma(X,Y,Z).


% Sumar n veces x. Solo es un ejemplo en el que nos basamos
% para poder realizar el ejercicio de la multiplicacion.
sum(1,X,X).
sum(X,Y,Z) :- W is X - 1, sum(W,Y,K), Z is K + Y.

% MULTIPLICACION
multiplicacion(s(c),X,X).
multiplicacion(s(X),Y,Z) :- multiplicacion(X,Y,K), suma(K,Y,Z).

% ADECIMAL
aDecimal(c,0).
aDecimal(s(X),Z) :- aDecimal(X,K), Z is K + 1.


% --------------------------------------------------%
%                      3-Listas                     %
% --------------------------------------------------%

% Ejemplo
listar(L) :- Xs = [2,3,4,5], H = 1, L = [H|Xs].

% Funcion que regresa la cabeza de una lista.
cabeza([C|L], C).
cabeza([C|L],Cabeza) :-  Cabeza is C .

% EJERCICIO 1
% Funcion que regresa una lista sin su cabeza
sinCabeza([C],[]).
sinCabeza([C|L], L) :- sinCabeza(L,K).

% EJERCICIO 2 
% Hechos
ancestro(pedro, [ana, ramon]).
ancestro(ana, [ramon, pepe, juan]).

% Funcion que determina si X es el primer descendiente de Y.
descendienteP(X,Y) :- ancestro(Y,[X|L]).

% EJERCICIO 3 (EXTRA)
% Hechos
ancestro(pedro, [ana, ramon, pedro, javier, vilma, nicolas]).
ancestro(juan, [ben, pepe, josue, jesica, pavel, keith, kyle]).

% Hijo descendiente de P
descendiente(Hijo,P) :- ancestro(P,L), buscar(Hijo,L).

% Funcion auxiliar que busca un elemento dentro de una lista
buscar(_, []) :- !, fail. 	
buscar(Hijo, [C|_]) :- Hijo == C.
buscar(Hijo, [Hijo |L]) :- !, true.
buscar(Hijo, [_|L]) :- buscar(Hijo,L).



