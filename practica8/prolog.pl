% Ej 1
padre(juan, carlos).
padre(juan, luis).
padre(carlos, daniel).
padre(carlos, diego).
padre(luis, pablo).
padre(luis, manuel).
padre(luis, ramiro).
abuelo(X, Y) :- padre(X,Z), padre(Z, Y).

hijo(X, Y) :- padre(Y, X).
hermano(X, Y) :- padre(Z, X), padre(Z, Y), X \= Y.
descendiente(X, Y) :- hijo(X, Y).
descendiente(X, Y) :- hijo(X, Z), descendiente(Z, Y).




% Ej 3
natural(0).
natural(suc(X)) :- natural(X).
menorOIgual(X,X) :- natural(X).
menorOIgual(X, suc(Y)) :- menorOIgual(X, Y).

% Ej 4
% juntar(?L1, ?L2, ?L3)
juntar([], L, L).
juntar([H1 | T1], L2, [H1 | L3]) :- juntar(T1, L2, L3).


% Ej 5

%last(?L, ?U)
last(L, U) :- append(_, [U], L).

%reverse(+L, ?R)
reverse1([], []).
reverse1(L, [U | R]) :- last(L, U), append(T, U, L), reverse(T, R).

reverse2([], []).
reverse2([H | T], R) :- reverse(T, P), append(P, [H], R).

%prefijo(?P, +L)
prefijo(P, L) :- append(P, _, L).

%sufijo(?S, +L)
sufijo(S, L) :- append(_, S, L).

%sublista(?S, +L)
sublista([], _).
sublista([X|Xs], L) :- prefijo(P, L), sufijo([X|Xs], P).


%pertenece(?X, +L)
pertenece(X, [X]).
pertenece(X, L) :- sublista([X], L).


% Ej 6

%aplanar(+XS, -YS)
aplanar([], []).

% Caso en la cabeza hay una lista
aplanar([X | XS], YS) :- is_list(X), aplanar(X, Y), aplanar(XS, APLREC), append(Y, APLREC, YS).

% Caso en la cabeza hay un atomo
aplanar([X | XS], YS) :- not(is_list(X)), aplanar(XS, APLREC), append([X], APLREC , YS).


% Ej 7 
% Usando member y/o append

%borrar(+ListaOriginal, +X, -ListaSinXs)
borrar([], _, []).
borrar([H | T], X, [H | ListaSinXs]) :- H \= X, borrar(T, X, ListaSinXs).
borrar([X | T], X, ListaSinXs) :- borrar(T, X, ListaSinXs).


%sacarDuplicados(+L1, -L2)
sacarDuplicados([], []).
sacarDuplicados([H1 | T1], [H1 |L2]) :- borrar(T1, H1, T1SinH1), sacarDuplicados(T1SinH1, L2).

%partir(tamaño de L1, L original, L1, L2 (resto))
partir(0, L, [], L).
partir(N, [X|L], [X|L1], L2) :- partir(N2, L, L1, L2), N is N2+1.


insertar(X, XS, YS) :- append(P, S, XS), append(P, [X|S], YS).

%permutacion(+L1, ?L2)
permutacion([], []).
permutacion([H1 | T1], L2) :- permutacion(T1, L2P), insertar(H1, L2P, L2).

%reparto(+L, +N, -LListas)
reparto([], 0, []).
reparto(L, N, [X | Listas]) :- N > 0, append(X, XS, L), N1 is N-1, reparto(XS, N1, Listas).

%repartoSinVacias(+L, -LListas)
%El segundo parametro es una lista de listas tal que al concatenarlas te dan la lista del primer parametro
repartoSinVacias([], []).
repartoSinVacias(L, LListas) :- length(L, N), between(1, N, K), reparto(L, K, LListas), not((member(X, LListas), length(X, 0))).

sumaK([], 0).
sumaK([H | T], K) :- K > 0, K1 is K - H, sumaK(T, K1). 

%Ejercicio 8
%parteQueSuma(+L, +S, -P)
parteQueSuma(_, 0, []).

% Caso donde agrego la Head
parteQueSuma([H | T], S, [H | P]) :- S > 0, S1 is S - H, parteQueSuma(T, S1, P).

% Caso donde no agrego la Head
parteQueSuma([_ | T], S, P) :- S > 0, parteQueSuma(T, S, P).


%Ej 9
desde(X, X).
desde(X, Y) :- N is X+1, desde(N, Y).

%ambos instanciados: se cuelga
%El primero instanciado: funciona bien, unificando el segundo parametro con valores desde el primero
%El segundo instanciado: funciona bien una sola vez, luego da error de instanciacion
%Ninguno instanciado: igual que arriba
desdeReversible(X, X).
desdeReversible(X, Y) :- var(Y), N is X+1, desdeReversible(N, Y).
desdeReversible(X, Y) :- nonvar(Y), Y >= X.

%Ej 11 %(bin(izq, v, der))

arbol_ejemplo(
    bin(
        bin(
            bin(nil, 1, nil),  % subárbol izquierdo del 2
            2,
            bin(nil, 3, nil)   % subárbol derecho del 2
        ),
        4,
        bin(
            nil,
            6,
            bin(nil, 7, nil)   % subárbol derecho del 6
        )
    )
).

% Ejemplo de árbol:
%        4
%       / \
%      2   6
%     / \   \
%    1   3   7


arbol_busc(
    bin(
        bin(
            bin(nil, 2, nil),
            3,
            bin(nil, 4, nil)
        ),
        5,
        bin(
            nil,
            8,
            bin(nil, 10, nil)
        )
    )
).

%        5
%       / \
%      3   8
%     / \   \
%    2   4   10

%vacio
vacio(nil).

%raiz
raiz(bin(_, R, _), R).

%altura
altura(nil, 0).
altura(bin(Izq, _, Der), Altura) :- altura(Izq, I), altura(Der, D), Altura is max(I, D) + 1.

%cantidadDeNodos
cantidadDeNodos(nil, 0).
cantidadDeNodos(bin(Izq, _, Der), Cantidad) :- cantidadDeNodos(Izq, I), cantidadDeNodos(Der, D), Cantidad is I + D + 1.


% Ej 12
%inorder(+AB, -Lista)
inorder(nil, []).
inorder(bin(Izq, X, Der), Nodos) :- inorder(Izq, I), inorder(Der, D), append(I, [X| D], Nodos).

%arbolConInorder(+Lista, -AB)
arbolConInorder([], nil).
arbolConInorder(Lista, bin(I, X, D)) :- append(L1, [X| L2], Lista), arbolConInorder(L1, I), arbolConInorder(L2, D).

%aBB(+T)
aBB(nil).
aBB(bin(I, V, D)) :- aBB(I), aBB(D), inorder(I, LI), max_list([V|LI], V), inorder(D, LD), min_list([V|LD], V).

%aBBInsertar(+X, +T1, -T2)
%insertar en el arbol vacio
aBBInsertar(X, nil, bin(nil, X, nil)).

aBBInsertar(X, bin(I, X, D), bin(I, X, D)).     % aBB no tiene valores repetidos.

%Caso donde el X va hacia la derecha (X=7)
aBBInsertar(X, bin(I1, R1, D1), bin(I1, R1, D2)) :- X > R1, aBBInsertar(X, D1, D2).

aBBInsertar(X, bin(I1, R1, D1), bin(I2, R1, D1)) :- X < R1, aBBInsertar(X, I1, I2).

%        5
%       / \
%      3   8
%     / \   \
%    2   4   10




%Ej 13
generarPar(X, Y) :- desde(2, S), S2 is S - 1, between(1, S2, X), Y is S - X  .

esCoprimo(X, Y) :- 1 is gcd(X,Y).

%coprimos(-X, -Y)
coprimos(X, Y):- generarPar(X, Y), esCoprimo(X, Y).


% Ej 15
generarTriplas(A, B, C) :- desde(1, Inf), between(1, Inf, A), between(1, Inf, B), between(1, Inf, C).

esTriangulo(tri(A, B, C)) :- A < B + C, B < A + C, C < A + B.

%Instanciado el triangulo
perimetro(tri(A,B,C), P) :- ground(tri(A,B,C)), esTriangulo(tri(A,B,C)),  P is A + B + C.

%Triangulo no instanciado
perimetro(tri(A,B,C), P) :- not(ground(tri(A,B,C))), generarTriplas(A, B, C), esTriangulo(tri(A,B,C)),  P is A + B + C.
