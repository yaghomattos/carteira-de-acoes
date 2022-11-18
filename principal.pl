%-----------------------------------------------------------------------------------------
/*:- use_module(library(persistency)).

:- persistent operacao(fact1:any, fact2:any, fact3:any, fact4:any).
:- persistent compra(fact1:any, fact2:any, fact3:any, fact4:any, fact5:any, fact6:any, fact7:any).
:- persistent venda(fact1:any, fact2:any, fact3:any, fact4:any, fact5:any, fact6:any, fact7:any).

:- initialization(init).

init:-
  absolute_file_name('carteira.pl', File, [access(write)]),
  db_attach(File, []).*/

:- use_module(library(aggregate)).

salva(Predicado,Arquivo) :-
 tell(Arquivo),
 listing(Predicado),
 told.

carrega(A) :-
 exists_file(A),
 consult(A).

:- initialization(init).

init :- carrega('operacoes.bd').

data_hoje(D,M,Y) :- get_time(T), stamp_date_time(T, date(Y, M, D, _, _, _, _, _, _), 'UTC').

%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%empresa(Ticket, Preço Unitário)
empresa("PETR4", 15.34).
empresa("ITSA4", 8.51).
empresa("BBAS3", 28.51).
empresa("MOVI3", 8.50).
empresa("VIIA3", 4.03).

:- dynamic empresa/2.

cadastro(X,Y) :- assertz(empresa(X,Y)).

empresas :- listing(empresa/2).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%cadastro de operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)

:- dynamic operacao/7.

salva_operacoes :- salva(operacao(_,_,_,_,_,_,_), 'operacoes.bd').

cadastro_operacao(A, O, P, N) :- 
  T is N * P * 0.00025 + N * P * 0.00005,
  C is P * N + T, 
  data_hoje(D,M,Y),
  assertz(operacao(data(D,M,Y), A, O, P, N, T, C)),
  salva_operacoes. 

%operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)
/*operacao(11/03/2020, 'PETR4', 'Compra', 15.34, 200, 0.92, 3068.92).
operacao(12/03/2020, 'PETR4', 'Compra', 12.68, 300, 1.14, 3805.14).
operacao(18/03/2020, 'PETR4', 'Compra', 10.98, 100, 0.32, 1098.32).
operacao(20/03/2020, 'ITSA4', 'Compra', 8.51, 100, 0.25, 851.25).
operacao(31/03/2020, 'BBAS3', 'Compra', 28.51, 100, 0.85, 2850.85).
operacao(31/03/2020, 'MOVI3', 'Compra', 8.50, 100, 0.25, 850.25).
operacao(31/03/2020, 'MOVI3', 'Compra', 8.00, 100, 0.24, 800.24).
operacao(01/04/2020, 'AMAR3', 'Compra', 3.98, 200, 0.23, 796.23).
operacao(02/04/2020, 'AMAR3', 'Compra', 3.79, 100, 0.11, 379.11).
operacao(03/04/2020, 'VIIA3', 'Compra', 4.03, 100, 0.12, 403.12).
operacao(03/04/2020, 'BBSE3', 'Compra', 22.81, 100, 0.68, 2281.68).
operacao(03/04/2020, 'CMIG4', 'Compra', 7.99, 100, 0.23, 799.23).
operacao(24/04/2020, 'BBAS3', 'Compra', 24.99, 200, 1.49, 4999.49).
operacao(07/05/2020, 'CIEL3', 'Compra', 3.62, 200, 0.21, 362.21).
operacao(19/03/2020, 'PETR4', 'Venda', 12.60, -100, 0.37, -1259.63).
operacao(02/04/2020, 'PETR4', 'Venda', 16.00, -200, 0.96, -3199.04).
operacao(07/04/2020, 'ITSA4', 'Venda', 9.10, -100, 0.27, -909.73).
operacao(24/03/2020, 'PETR4', 'Venda', 13.29, -300, 1.19, -3985.81).
operacao(07/04/2020, 'BBAS3', 'Venda', 30.11, -100, 0.90, -3010.10). 
operacao(08/04/2020, 'MOVI3', 'Venda', 9.18, -200, 0.55, -1835.45).
operacao(09/04/2020, 'VIIA3', 'Venda', 5.41, -100, 0.16, -540.84).
operacao(09/04/2020, 'CMIG4', 'Venda', 9.42, -100, 0.28, -941.72).
operacao(14/04/2020, 'BBSE3', 'Venda', 27.00, -100, 0.81, -2699.19).
operacao(15/04/2020, 'AMAR3', 'Venda', 5.54, -300, 0.49, -1661.51).*/

operacoes :- 
  carrega('operacoes.bd'),
  listing(operacao/7).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%carteira de ações(Ação, Preço Médio, Quantidade, Valor Investido)

:- dynamic acao/4.

calcula_carteira :- 
  retractall(acao(A,B,C,D)), 
  forall(operacao(_,A,_,B,C,_,D), assert(acao(A,B,C,D))). 

carteira :- 
  calcula_carteira, 
  listing(acao/4).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%lucro / prejuízo

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
  forall(lucro(X,Y), (write(X), write(' | ') ,write(Y), write('\n'))),
  (write('--------------------\n'), write('Lucro Total: '), write(T), write('\n--------------------\n\n')), !.
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%operações por ação
/*O programa deve ter uma funcionalidade para listar todas as operacoes realizadas de uma dada acao.
Deve ser possivel exibir as operacoes filtradas por ano e tipo da operacao (compra ou venda).*/

operacao_acao(A, Ano, O) :-  forall(operacao(data(_,_,Ano),A,O,_,_,_,_), ).
%-----------------------------------------------------------------------------------------
