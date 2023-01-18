% --------------------------------------------------%
%  Actividad de laboratorio - Acumuladores y cortes %
% --------------------------------------------------%

% --------------------------------------------------%
% Observemos las siguientes predicados que regresan %
%            la longitud de una lista               %
% --------------------------------------------------%

longitud([], 0).
longitud([H|T], R) :- longitud(T, Rt), R is Rt + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

longitud2(L, R) :- longitudAcc(L, 0, R).

longitudAcc([], R, R).
longitudAcc([_|T], Acc, R) :- Acc2 is Acc + 1, longitudAcc(T, Acc2, R).


% --------------------------------------------------%
% Ejercicio 1 : Versión con acumulador de maximo    %
% --------------------------------------------------%


max(L,M) :- maxAc(L,0,M).


maxAc([], M, M).
maxAc([H|T], A, M) :- H >= A, NuevoA is H, maxAc(T, NuevoA, M).
maxAc([H|T], A, M) :- H < A, maxAc(T, A, M).

% --------------------------------------------------%
% Ejercicio 2 : Define un predicado que toma una    %
%               lista y regresa volteada sin usar   %
%               append.                             %
% --------------------------------------------------%

volteada(L,R) :- voltAc(L,[],R).

voltAc([],R,R).
voltAc([H|T], Ac,R) :-  voltAc(T, [H|Ac], R).

% --------------------------------------------------%
% Ejercicio 3 : Define una versión con acumulador   %
%               de "aplana" sin usar append.        %
% --------------------------------------------------%

aplana(L, F) :- aplanaAcc(L, [], F).

aplanaAcc([], F, F).
aplanaAcc([H|T], Acc, F) :- aplanaAcc(T, Acc, FT), aplanaAcc(H, FT, F).
aplanaAcc(L, Acc, [L|Acc]) :- L \= [], L \= [_|_].


% ---------------------------------------------------%
% Ejercicio 4 : Practica usando el corte: !          %
%               Define un predicado que remueva la   % 
%               primer aparición de un elemento dado %
% ---------------------------------------------------%

%quitaX(X,Li,Lo) :- quitaXAcc(X,Li,[],Lo).

quitaXAcc(_, [], R,R).
quitaXAcc(X, [X|T], Acc, R) :- append(Acc, T, R),!.
quitaXAcc(X, [H|T], Acc, R) :- append(Acc, [H], Acc2), quitaXAcc(X, T, Acc2, R). 




quitaX(X, [], []).
quitaX(X, [X|T], T) :- !.
quitaX(X,[Hi|Ti], [Hi|To]) :- quitaX(X, T, To).


