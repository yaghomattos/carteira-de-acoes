/*
  Trabalho Prático 1 - Linguagens de Programação

  Autores: Yagho Mattos da Rocha - Matrícula: 201765565AC 
           Thiago de Almeida Lopes  - Matrícula: 201765556AC
*/

%-----------------------------------------------------------------------------------------
/* Seção de importação de bibliotecas, inicialização e menu */

:- use_module(library(aggregate)).

:- initialization(init, program).

init :- (exists_file('operacoes.txt') -> carrega('operacoes.txt'); true), write('Trabalho 1 - Linguagens de Programação\n'),menu.

menu :- 
  repeat,
  nl, write('Menu'),nl,nl,
  write('1.Cadastro de Operações'),nl,
  write('2.Operações Cadastradas'),nl,
  write('3.Carteira de Ações'),nl,
  write('4.Lucro/Prejuízo no Ano'),nl,
  write('5.Operações Realizadas por Ação'),nl,
  write('6.Finalizar Programa!'),nl,nl,
  write('Escolha uma Opção seguida pelo "." (ponto final) :'),nl,
  read(Choice),
  run_opt(Choice).

run_opt(1) :- write("\nUsar o formato: cadastro_operacao('AÇÃO', 'Operação', preço, quantidade)\n\n").
run_opt(2) :- write('\n______________________________Operações Cadastradas:______________________________\n'),operacoes, menu.
run_opt(3) :- write('\n_________________Carteira de Ações:_________________\n'), carteira, menu.
run_opt(4) :- write("\nUsar o formato: calcula_lucro(Ano)\n\n").
run_opt(5) :- write("\nUsar o formato: operacao_acao('Ação', ano, operação)\n\n").
run_opt(6) :- write('Finalizado...\n'), halt.
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
/* Estruturas de persistência dos dados, manipulação de lista, truncagem de números */

salva(Predicado,Arquivo) :-
 tell(Arquivo),
 listing(Predicado),
 told.

carrega(A) :-
 exists_file(A),
 consult(A).

/*funções de manipulação de lista*/
elemento(X, [X|[]]).
elemento(X,[X|_]).
elemento(X,[_|L]) :- elemento(X,L).

remove_primeiro([_|X], X). 

/*truncagem para números positivos*/
trunca(X,Y) :- X >= 0, Y is floor(10^2 * X)/10^2, !.

/*truncagem para números negativos*/
trunca(X,Y) :- X < 0, Y is ceil(10^2 * X)/10^2, !. 
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
/* Estrutura de cadastro de operações, salvamento e listagem de operações cadastradas */

:- dynamic operacao/7.

data_hoje(D,M,Ano) :- get_time(T), stamp_date_time(T, date(Ano, M, D, _, _, _, _, _, _), 'UTC').

salva_operacoes :- salva(operacao(_,_,_,_,_,_,_), 'operacoes.txt').

cadastro_operacao(A, 'Compra', P, N) :- 
  T is N * P * 0.00025 + N * P * 0.00005,
  trunca(T,T_Truncada),
  C is P * N + T, 
  trunca(C,C_Truncado),
  data_hoje(D,M,Ano),
  assertz(operacao(data(D,M,Ano), A, 'Compra', P, N, T_Truncada, C_Truncado)),
  cabecalho_operacao,
  imprime_operacao(data(D,M,Ano), A, 'Compra', P, N, T_Truncada, C_Truncado),
  salva_operacoes, menu. 

cadastro_operacao(A, 'Venda', P, N) :- 
  T is N * P * 0.00025 + N * P * 0.00005,
  trunca(T,T_Truncada),
  N_Negativa is N * (-1),
  C is P * N_Negativa + T, 
  trunca(C,C_Truncado),
  data_hoje(D,M,Ano),
  assertz(operacao(data(D,M,Ano), A, 'Venda', P, N_Negativa, T_Truncada, C_Truncado)),
  cabecalho_operacao,
  imprime_operacao(data(D,M,Ano), A, 'Venda', P, N_Negativa, T_Truncada, C_Truncado),
  salva_operacoes, menu. 

cabecalho_operacao :- 
  (write('   Data    | Ticket | Operacao | Preço Unitário | Quantidade | Taxas | Custo Total\n'), write('----------------------------------------------------------------------------------\n')).

imprime_operacao(data(D,M,Ano),A,O,P,N,T,C) :- 
  format('~|~`0t~d~2+', [D]),write('/'),format('~|~`0t~d~2+', [M]),write('/'),
  write(Ano),write(' | '),write(A),write('  |  '),(O == 'Compra' -> write(O); (write(O), write(' '))),
  write('  |      '),write(P),write('     |     '),write(N),write('    | '),write(T),
  write('  |   '),write(C),nl.

operacoes :- 
  carrega('operacoes.txt'),
  nl, cabecalho_operacao,
  forall(operacao(data(D,M,Ano),A,O,P,N,T,C), (imprime_operacao(data(D,M,Ano),A,O,P,N,T,C))),
  write('----------------------------------------------------------------------------------\n\n').
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
/* Estruturas d */

:- dynamic acao/4.
:- dynamic nomes/1.
:- dynamic lista_nomes/1.
:- dynamic acao_quantidade/2.
:- dynamic acao_taxa/2.
:- dynamic acao_preco_medio/2.
:- dynamic taxa/2.
:- dynamic preco/2.
:- dynamic quantidade/2.
:- dynamic preco_medio/3.

separa_nomes :-
  retractall(nomes(_)),
  forall(operacao(data(_,_,_),A,_,_,_,_,_), assert(nomes(A))). 

nomes :- setof(X, nomes(X), L), assert(lista_nomes(L)).

separa_quantidade :- forall(operacao(data(_,_,_),A,_,_,N,_,_), assert(acao_quantidade(A,N))).

lista_quantidade :- forall(bagof(N, acao_quantidade(A,N), L), assert(quantidade(A, L))).

separa_taxa :- forall(operacao(data(_,_,_),A,_,_,_,T,_), assert(acao_taxa(A,T))).

lista_taxa :- forall(bagof(T, acao_taxa(A,T), L), assert(taxa(A, L))).

separa_preco :- forall(operacao(data(_,_,_),A,_,P,_,_,_), assert(acao_preco_medio(A,P))).

lista_preco :- forall(bagof(P, acao_preco_medio(A,P), L), assert(preco(A, L))).

separa_estruturas :-   
  separa_quantidade,
  lista_quantidade,
  separa_taxa,
  lista_taxa,
  separa_preco,
  lista_preco,
  separa_nomes,
  nomes.

calcula_carteira :- 
  separa_estruturas,
  lista_nomes(Y),
  forall(elemento(A, Y), (preco(A, P), quantidade(A, N), taxa(A, T), assert(acao(A, P, N, T)))),
  aux_preco_medio.

calcula_pm(A, [P|[]], [N|[]], [T|[]],PM1, NA) :- 
  N2 is NA + N,
  (N2 =< 0 -> PMA is 0; PMA is ((NA*PM1 + (N * P + T))/N2)),
  assert(preco_medio(A,PMA,N2)).
calcula_pm(A, PL, NL, TL, PM1, NA) :- 
  elemento(N,NL),
  N2 is NA + N,
  remove_primeiro(PL, PT),
  remove_primeiro(NL, NT),
  remove_primeiro(TL, T_Truncada),
  calcula_pm(A, PT, NT, T_Truncada,PM1,N2).

aux_preco_medio :- 
  forall(acao(A,PL,NL,TL), (elemento(P,PL), elemento(N,NL),elemento(T,TL), 
  PM is N * (P + T)/N, calcula_pm(A,PL,NL,TL,PM,0))).  

imprime_preco_medio :- 
  (write('\nAção'),write('  | '),write('Preço Médio'),write(' | '),
  write(' Quantidade '),write(' | '),write('Valor Investido'),
  write('\n-----------------------------------------------------\n')),
  forall(preco_medio(A,PM,N), (
    N =< 0 -> ! ;
    trunca(PM, PMT),
    Total is PM * N,
    trunca(Total,TotalT),
    (PM >= 10 -> write(A),write(' |    '),write(PMT),write('    |      '),
    write(N),write('     | '),write(TotalT),nl;
    write(A),write(' |    0'),write(PMT),write('    |      '),write(N),
    write('     | '),write(TotalT),nl
    ))),
  write('-----------------------------------------------------\n').

carteira :- 
  calcula_carteira, 
  imprime_preco_medio.
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
:- dynamic lucro/2.
:- dynamic custo/2.
:- dynamic preco/1.
:- dynamic lucro_total/1.

separa(Ano) :- 
  retractall(custo(_,_)), 
  forall(operacao(data(_,_,Ano),A,_,_,_,_,C), assert(custo(A,C))).

somatorio :- 
  retractall(lucro(_,_)),
  forall((aggregate(sum(C), custo(A,C), Total)), assert(lucro(A, Total))).

separa_lucro :- 
  retractall(preco(_)),
  forall(lucro(_,C), assert(preco(C))).

calcula_total :- 
  separa_lucro, aggregate(sum(C), preco(C), Total), assert(lucro_total(Total)).

calcula_lucro(Ano) :- 
  separa(Ano),
  somatorio,
  calcula_total,
  lucro_total(T),
  (write('\nAção  | Lucro\n'), write('--------------------\n')),
  forall(lucro(X,Y), (write(X), write(' | ') ,write(Y), nl)),
  (write('--------------------\n'), write('Lucro Total: '), write(T), write('\n--------------------\n\n')), 
  menu.
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
operacao_acao(Acao, Ano, Operacao) :-  
  (write('\nData      | Ticket | Operacao | Preço Unitário | Quantidade | Taxas | Custo Total\n'), write('----------------------------------------------------------------------------------\n')),
  forall(operacao(data(D,M,Ano),Acao,Operacao,P,N,T,C), (write(D),
  write('/'),write(M),write('/'),write(Ano),write(' | '),write(Acao),write('  | '),
  write(Operacao),write('   | '),write(P),write('  | '), write(N),write('  | '),write(T),
  write('  | '),write(C),nl)), 
  menu.
%-----------------------------------------------------------------------------------------
