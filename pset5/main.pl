% Agregar declaración discontiguous para suprimir advertencias
:- discontiguous juega_basquetbol/1.
:- discontiguous vive_misma_casa/2.
:- discontiguous deportista/1.
:- discontiguous britanico/1.
:- discontiguous puede_ser_multado/1.
:- discontiguous ciudadano_mexicano/1.

% Hechos para probar Ejercicio 1a - Misma casa
direccion(juan, 'Calle 123').
direccion(maria, 'Calle 123').
direccion(pedro, 'Calle 456').

% Hechos para probar Ejercicio 1b - Deportistas
juega_basquetbol(ana).
juega_futbol_americano(carlos).
juega_basquetbol(luis).

% Hechos para probar Ejercicio 1c - Británicos
gales(dylan).
ingles(john).
escoces(william).
irlandes(sean).

% Hechos para probar Ejercicio 1d - Multa de auto
tiene_auto(roberto).
tiene_auto(sofia).
pago_tenencia(sofia).

% Hechos para probar Ejercicio 1e - Ciudadanía mexicana
nacionalidad_mexicana(miguel).
nacionalidad_mexicana(laura).
mayor_de_edad(miguel).
mayor_de_edad(laura).
honesto(miguel).

% Ejercicio 1a - Misma casa
vive_misma_casa(Persona1, Persona2) :- 
    direccion(Persona1, Dir),
    direccion(Persona2, Dir),
    Persona1 \= Persona2.

% Ejercicio 1b - Deportistas
deportista(X) :- juega_basquetbol(X).
deportista(X) :- juega_futbol_americano(X).

% Ejercicio 1c - Británicos
britanico(X) :- gales(X).
britanico(X) :- ingles(X).
britanico(X) :- escoces(X).
britanico(X) :- irlandes(X).

% Ejercicio 1d - Multa de auto
puede_ser_multado(X) :- 
    tiene_auto(X),
    \+ pago_tenencia(X).

% Ejercicio 1e - Ciudadanía mexicana
ciudadano_mexicano(X) :-
    nacionalidad_mexicana(X),
    mayor_de_edad(X),
    honesto(X).

% Ejercicio 2 - Máximo de una lista
maximo([X], X).
maximo([X|Xs], Max) :-
    maximo(Xs, MaxRest),
    Max is max(X, MaxRest).

% Ejercicio 3 - Contar números distintos
numeros_distintos(Lista, Cantidad) :-
    sort(Lista, ListaSinDuplicados),
    length(ListaSinDuplicados, Cantidad).

% Ejercicio 4 - Fibonacci
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(2, 1).
fibonacci(N, F) :-
    N > 2,
    N1 is N - 1,
    N2 is N - 2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.