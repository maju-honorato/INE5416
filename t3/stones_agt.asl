// Agent sample_agent in project stones

/*   
a ideia principal é fazer com que o adversario sobre com um numero impar
de torres com uma pedra, dessa forma se ele nao conseguir add nenhuma pedra
e so puder retirar de alguma torre, os dois agentes tiram uma pedra de cada ate
que sobre uma torre de uma pedra (e o agente que começar tiradno 'um' de um numero impar
de torres, perde)
*/

/* Initial beliefs and rules */

maior_torre(T, S) :- tower(T, S) & not (tower(T2, S2) & T2 \== T & S2 > S).

/* Initial goals */

!start.

/* Plans */

+!start : true
    <- .print("hello world.");
       .date(Y,M,D); .time(H,Min,Sec,MilSec); // get current date & time
       +started(Y,M,D,H,Min,Sec).             // add a new belief

/* Percepts */

+start: .my_name(Me) & .term2string(Me, MeStr) & player(MeStr) <- 
    .print("Percebi que o jogo iniciou e sou um jogador.").

+round(N, WhoPlays): .my_name(Me) & .term2string(Me, WhoPlays) <- /*I'm the current player*/
    .print("It's round ", N, " I'm the player");

    .findall(X, possibleNumber(X), ListaNum);
    .findall(T, (tower(T, S) & S >= 1), ListaTodasTorres);

    .findall(T, (tower(T, S) & S > 1), ListaTorresMultiplasPedras);
    .length(ListaTorresMultiplasPedras, QtdeTorresMultiplasPedras);

    .findall(T, (tower(T, S) & S == 1), ListaTorresUmaPedra);
    .length(ListaTorresUmaPedra, QtdeTorresUmaPedra);

    !jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres).

// se so tiver torres com uma pedra
+!jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres): QtdeTorresMultiplasPedras == 0 <-
    if ((QtdeTorresUmaPedra mod 2) == 0) {
        if (adicao_disponivel(K) & (K div 2) > 0) {
                .nth(0, ListaTorresUmaPedra, T);
                play(T, -1);
                .abolish(adicao_disponivel(K));
        } 
        else {
            .nth(0, ListaTorresUmaPedra, T);
            play(T, 1);
            .abolish(adicao_disponivel(_));
        }
    }
    else {
        if (adicao_disponivel(K) & (K div 2) > 0) {
            .nth(0, ListaTorresUmaPedra, T);
            play(T, -1);
            .abolish(adicao_disponivel(K));
        }
        else {
            .nth(0, ListaTorresUmaPedra, T);
            play(T, 1);
            .abolish(adicao_disponivel(_));
        }
    }.

// se tiver mais de uma torre c/ multiplas pedras, tira o max de pedras possiveis da maior torre
+!jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres): QtdeTorresMultiplasPedras > 1 <-
    ?maior_torre(T, S);
    .findall(N, (.member(N, ListaNum) & N <= S), ListaNumValidos);
    .max(ListaNumValidos, MaxNum);
    play(T, MaxNum);

    if (MaxNum > 1) {
        .abolish(adicao_disponivel(_));
        +adicao_disponivel(MaxNum);
    }
    else {
        .abolish(adicao_disponivel(_));
    }.

// se tiver uma torre c/ multiplas pedras e nenhuma torre c/ uma pedra
+!jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres): QtdeTorresMultiplasPedras == 1 & QtdeTorresUmaPedra == 0 <-
    .nth(0, ListaTorresMultiplasPedras, T);
    ?tower(T, S);
    SobraUm = S - 1;

    // tenta deixar torre com uma pedra
    if (.member(SobraUm, ListaNum)) {
        play(T, SobraUm);

        if (SobraUm > 1) {
            .abolish(adicao_disponivel(_));
            +adicao_disponivel(SobraUm);
        }
        else {
            .abolish(adicao_disponivel(_));
        }
    }

    // se nao der pra deixar uma pedra, tira o menor numero possivel
    else {
        .findall(N, (.member(N, ListaNum) & N <= S), ListaNumValidos);
        .min(ListaNumValidos, MinNum);
        play(T, MinNum);

        if (MinNum > 1) {
            .abolish(adicao_disponivel(_));
            +adicao_disponivel(MinNum);
        }
        else {
            .abolish(adicao_disponivel(_));
        }
    }.

// se tiver uma torre c/ multiplas pedras e pelo menos uma torre c/ uma pedra
+!jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres): QtdeTorresMultiplasPedras == 1 & QtdeTorresUmaPedra > 0 <-
    .nth(0, ListaTorresMultiplasPedras, T);
    ?tower(T, S);
    .findall(N, (.member(N, ListaNum) & N <= S), ListaNumValidos);

    // quantidade de torres c/ uma pedra eh impar
    if ((QtdeTorresUmaPedra mod 2) \== 0) {
        // zera a tore grande, se possivel
        if (.member(S, ListaNumValidos)) {
            play(T, S);

            if (S > 1) {
                .abolish(adicao_disponivel(_));
                +adicao_disponivel(S);
            }
            else {
                .abolish(adicao_disponivel(_));
            }
        } 
        // se nao der pra zerar, tira o menor numero possivel
        else {
            .min(ListaNumValidos, MinNum);
            play(T, MinNum);

            if (MinNum > 1) {
                .abolish(adicao_disponivel(_));
                +adicao_disponivel(MinNum);
            }
            else {
                .abolish(adicao_disponivel(_));
            }
        }

    // quantidade de torres c/ uma pedra eh par
    } 
    else {
        SobraUm = S - 1;
        // tenta deixar torre maior com uma pedra
        if (.member(SobraUm, ListaNumValidos)) {
            play(T, SobraUm);

            if (SobraUm > 1) {
                .abolish(adicao_disponivel(_));
                +adicao_disponivel(SobraUm);
            }
            else {
                .abolish(adicao_disponivel(_));
            }
        } 
        // se nao der pra deixar uma pedra, tira o menor numero possivel
        else {
            .min(ListaNumValidos, MinNum);
            play(T, MinNum);

            if (MinNum > 1) {
                .abolish(adicao_disponivel(_));
                +adicao_disponivel(MinNum);
            }
            else {
                .abolish(adicao_disponivel(_));
            }
        }
    }.

// se tudo der errado, tira uma pedra da primeira disponivel
+!jogar(QtdeTorresMultiplasPedras, QtdeTorresUmaPedra, ListaNum, ListaTorresMultiplasPedras, ListaTorresUmaPedra, ListaTodasTorres): true <-
    .nth(0, ListaTodasTorres, T);
    play(T, 1).

+round(N, WhoPlays): .my_name(Me) & .term2string(Me, MeStr) & MeStr \== WhoPlays. /*I'm NOT the current player*/

// garante q a adicao disponivel nao va pra proxima partida
+winner[source(judge)] <-
    .abolish(adicao_disponivel(_)).

{ include("$jacamo/templates/common-cartago.asl") }
{ include("$jacamo/templates/common-moise.asl") }

// uncomment the include below to have an agent compliant with its organisation
//{ include("$moise/asl/org-obedient.asl") }