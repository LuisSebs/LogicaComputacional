% Arrieta Mancera Luis Sebastian 318174116
% Gongora Ramírez Dania Paula  318128274
% Martínez Hernández Zuriel Enrique 318056423
% Practica 05
% EJERCICIO 1
pura_a([]).
pura_a([a]).
pura_a([a|X]) :- pura_a(X).

% EJERCICIO 2
reemplaza_a_b_c([],[]).
reemplaza_a_b_c([a|X],[b|Y]) :- reemplaza_a_b_c(X,Y).
reemplaza_a_b_c([b|X],[c|Y]) :- reemplaza_a_b_c(X,Y).
reemplaza_a_b_c([c|X],[a|Y]) :- reemplaza_a_b_c(X,Y).
reemplaza_a_b_c([X|XS],[X|YS]) :- X \= a, X \= b, X \= c, reemplaza_a_b_c(XS,YS).

% EJERCICIO 3
longitud([],0).
longitud([X|XS],Y) :- longitud(XS,K), Y is 1 + K.

% EJERCICIO 4
sumaUno([],[]).
sumaUno([X|XS],[Y|YS]) :- Y is X + 1, sumaUno(XS,YS).

% EJERCICIO 5
contiene_0([0]).
contiene_0([X|YS]) :- X is 0 ; contiene_0(YS).

% EJERCICIO 6
multEscalar(X,[],[]).
multEscalar(X,[Y|YS],[R|RS]) :- R is X*Y, multEscalar(X,YS,RS).

% EJERCICIO 7
prodPunto([],[],0).
prodPunto([X|XS],[Y|YS],R) :- prodPunto(XS,YS,K), R is X*Y + K.

% EJERCICIO 8
max([X],X).
max([X|XS],Y) :- max(XS,Y_TAIL), Y_TAIL > X, Y is Y_TAIL.
max([X|XS],Y) :- max(XS,Y_TAIL), X > Y_TAIL, Y is X.

% EJERCICIO 9
aplana([],[]).
aplana([X|XS],R) :-
    aplana(X,Y), 
    aplana(XS,YS),
    append(Y,YS,R).
aplana([X|XS],[X|YS]) :- X \= [], X \= [_|_], aplana(XS,YS).

% EJERCICIO 10
elementoN(1,[X|_],X) :- !.
elementoN(N,[X|XS],R) :- M is N-1, elementoN(M,XS,R).

