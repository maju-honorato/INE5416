:- use_module(library(clpfd)).

% predicado principal - ordem para resolver
resolver_kakuro(N) :-
    grupos(N, NumCelulas, Grupos),
    length(Celulas, NumCelulas),
    Celulas ins 1..9,
    regras(Grupos, Celulas),
    label(Celulas),
    tabuleiro(N, Tabuleiro),
    pegar_linhas(Tabuleiro, Celulas).

% coloca as regras de soma e valor unico para os grupos
regras([], _).
regras([grupo(Soma, Indices) | Resto], Celulas) :-
    pegar_vars(Indices, Celulas, Vars),
    sum(Vars, #=, Soma),   % soma igual
    all_distinct(Vars),   % todos diferentes
    regras(Resto, Celulas).

% monta uma lista com as variaveis dos indices procurados
pegar_vars([], _, []).
pegar_vars([X | Resto], Celulas, [V | Vars]) :-
    achar_numeros(X, Celulas, V),
    pegar_vars(Resto, Celulas, Vars).

% busca a celula correspondente ao indice
achar_numeros(1, [X|_], X) :- !.
achar_numeros(I, [_|Resto], X) :-
    Y is I - 1,
    achar_numeros(Y, Resto, X).

% processa as linhas do tabulerio
pegar_linhas([], _).
pegar_linhas([Linha|Resto], Celulas) :-
    print_linha(Linha, Celulas),
    nl,
    pegar_linhas(Resto, Celulas).

% processa os elementos de cada linha e printa na tela
print_linha([], _).
print_linha([b|Resto], Celulas) :-
    write('. '),
    print_linha(Resto, Celulas).
print_linha([Indice|Resto], Celulas) :-
    achar_numeros(Indice, Celulas, Valor),
    write(Valor),
    write(' '),
    print_linha(Resto, Celulas).

% fato que aramazena id, total de celulas brancas e os grupos (soma e variaveis)
grupos(57, 55, [
    grupo(3, [1,2]),
    grupo(9, [3,4]),
    grupo(13, [5,6,7,8]),
    grupo(13, [9,10,11,12]),
    grupo(16, [13,14]),
    grupo(19, [15,16,17]),
    grupo(17, [18,19]),
    grupo(4, [20,21]),
    grupo(29, [22,23,24,25]),
    grupo(16, [26,27,28,29,30]),
    grupo(10, [31,32,33,34]),
    grupo(16, [35,36]),
    grupo(16, [37,38]),
    grupo(16, [39,40,41]),
    grupo(10, [42,43]),
    grupo(14, [44,45,46,47]),
    grupo(12, [48,49,50,51]),
    grupo(17, [52,53]),
    grupo(17, [54,55]),
    grupo(13, [1,6,14,20]),
    grupo(4, [2,7]),
    grupo(3, [3,10]),
    grupo(30, [4,11,18,25]),
    grupo(11, [5,13]),
    grupo(16, [8,15]),
    grupo(11, [9,17,23,29]),
    grupo(13, [12,19]),
    grupo(34, [16,22,28,34,40]),
    grupo(7, [21,26,32]),
    grupo(19, [24,30,35]),
    grupo(10, [27,33,39,47]),
    grupo(18, [31,38,45,52]),
    grupo(29, [36,42,50,55]),
    grupo(13, [37,44]),
    grupo(4, [41,48]),
    grupo(3, [43,51]),
    grupo(16, [46,53]),
    grupo(13, [49,54])
]).

% fato que armazena a matriz usada pra o print
tabuleiro(57,
[[b,b,b,b,b,b,b,b,b,b],
[b,b,1,2,b,b,b,3,4,b],
[b,5,6,7,8,b,9,10,11,12],
[b,13,14,b,15,16,17,b,18,19],
[b,b,20,21,b,22,23,24,25,b],
[b,b,b,26,27,28,29,30,b,b],
[b,b,31,32,33,34,b,35,36,b],
[b,37,38,b,39,40,41,b,42,43],
[b,44,45,46,47,b,48,49,50,51],
[b,b,52,53,b,b,b,54,55,b]
]).