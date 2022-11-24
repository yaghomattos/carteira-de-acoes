%-----------------------------------------------------------------------------------------
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
%acao(Ticket, Preço Unitário)

:- dynamic acao/2.
:- dynamic lista_precoU/2.

acao('PETR4', 15.34).
acao('PETR4', 15.4).
acao('PETR4', 15.3).
acao('PETR4', 1.34).
acao('ITSA4', 8.51).
acao('BBAS3', 28.51).
acao('MOVI3', 8.50).
acao('VIIA3', 4.03).

cadastro_acao(X,Y) :- assertz(acao(X,Y)).

precos_acao(A, L) :- bagof(X, acao(A,X), L), assert(lista_precoU(A, L)).

last(X,[X]).
last(X,[_|T]) :- last(X,T).

member(X,[X|_]).
member(X,[_|L]) :- member(X,L).

ultimo_preco(A, X) :- 
  lista_precoU(A, L),
  last(X,L), !.

acoes :- 
  (write('\nTicket | Preço Unitário\n'), write('-----------------------\n')),
  forall(acao(T,P), (write(T),write('  | '),write(P), write('\n'))),
  write('-----------------------\n\n').
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%cadastro de operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)

:- dynamic operacao/7.

salva_operacoes :- salva(operacao(_,_,_,_,_,_,_), 'operacoes.bd').

%truncagem para números positivos
trunca(X,Y) :- X >= 0, Y is floor(10^2 * X)/10^2, !.

%truncagem para números negativos
trunca(X,Y) :- X <0, Y is ceil(10^2 * X)/10^2, !. 

cadastro_operacao(A, 'Compra', P, N) :- 
  T is N * P * 0.00025 + N * P * 0.00005,
  trunca(T,TT),
  C is P * N + T, 
  trunca(C,CT),
  data_hoje(D,M,Y),
  assertz(operacao(data(D,M,Y), A, 'Compra', P, N, TT, CT)),
  salva_operacoes, !. 

cadastro_operacao(A, 'Venda', P, N) :- 
  T is N * P * 0.00025 + N * P * 0.00005,
  trunca(T,TT),
  C is P * N - T, 
  trunca(C,CT),
  data_hoje(D,M,Y),
  assertz(operacao(data(D,M,Y), A, 'Venda', P, N, TT, CT)),
  salva_operacoes, !. 

%operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)

operacoes :- 
  carrega('operacoes.bd'),
  (write('\nData      | Ticket | Operacao | Preço Unitário | Quantidade | Taxas | Custo Total\n'), write('----------------------------------------------------------------------------------\n')),
  forall(operacao(data(D,M,Y),A,O,P,N,T,C), (write(D),write('/'),write(M),write('/'),
  write(Y),write(' | '),write(A),write('  | '),write(O),write('   | '),write(P),write('  | '),
  write(N),write('  | '),write(T),write('  | '),write(C),write('\n'))),
  write('----------------------------------------------------------------------------------\n\n').
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%carteira de ações(Ação, Preço Médio, Quantidade, Valor Investido)

:- dynamic acao/4.
:- dynamic nomes/1.
:- dynamic lista_nomes/1.
:- dynamic teste/2.
:- dynamic acao_quantidade/2.
:- dynamic acao_taxa/2.
:- dynamic acao_preco_medio/2.
:- dynamic taxa/2.
:- dynamic preco/2.
:- dynamic quantidade/2.

/*funções de manipulação de lista*/
elemento(X,[X|_]).
elemento(X,[_|L]) :- elemento(X,L).

ultimo(X,[X]).
ultimo(X,[_|T]) :- ultimo(X,T).

remove_primeiro([_|X], X). 
/*funções de manipulação de lista*/

separa_nomes :-
  retractall(nomes(_)),
  forall(operacao(data(_,_,_),A,_,_,_,_,_), assert(nomes(A))). 

nomes :- setof(X, nomes(X), L), assert(lista_nomes(L)).

separa_quantidade :- forall(operacao(data(_,_,_),A,_,_,N,_,_), assert(acao_quantidade(A,N))).

lista_quantidade :- forall(bagof(N, acao_quantidade(A,N), L), assert(quantidade(A, L))), 
  listing(quantidade/2).

soma_quantidade :- forall(quantidade(A,_), 
  (quantidade(A,L), (aggregate(sum(X), elemento(X,L), Total)), assert(teste(A,Total)))),
  listing(teste/2).

separa_taxa :- forall(operacao(data(_,_,_),A,_,_,_,T,_), assert(acao_taxa(A,T))).

lista_taxa :- forall(bagof(T, acao_taxa(A,T), L), assert(taxa(A, L))), 
  listing(taxa/2).

separa_preco :- forall(operacao(data(_,_,_),A,_,P,_,_,_), assert(acao_preco_medio(A,P))).

lista_preco :- forall(bagof(P, acao_preco_medio(A,P), L), assert(preco(A, L))), 
  listing(preco/2).

separa :-   
  separa_quantidade,
  lista_quantidade,
  separa_taxa,
  lista_taxa,
  separa_preco,
  lista_preco,
  separa_nomes,
  nomes.

recursao(A, LP, LN, LT) :- 
  acao(A, LP, LN, LT),
  (elemento(P, LP),
  elemento(N, LN),  
  elemento(T, LT)),
  (write(A),write(', '),write(P),write(', '),write(N),write(', '),write(T),write('\n')),
  remove_primeiro(LP, X), remove_primeiro(LN, Y), remove_primeiro(LT, Z),
  assert(acao(A,P,N,T)),
  recursao(A, X, Y, Z).

calcula_preco_medio :- 
  separa,
  lista_nomes(Y),
  forall(elemento(A, Y), (preco(A, P), quantidade(A, N), taxa(A, T), assert(acao(A, P, N, T)))).

calcula_carteira :- 
  calcula_preco_medio.

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

operacao_acao(Acao, Ano, Operacao) :-  
  (write('\nData      | Ticket | Operacao | Preço Unitário | Quantidade | Taxas | Custo Total\n'), write('----------------------------------------------------------------------------------\n')),
  forall(operacao(data(D,M,Ano),Acao,Operacao,P,N,T,C), (write(D),
  write('/'),write(M),write('/'),write(Ano),write(' | '),write(Acao),write('  | '),
  write(Operacao),write('   | '),write(P),write('  | '), write(N),write('  | '),write(T),
  write('  | '),write(C),write('\n'))).
%-----------------------------------------------------------------------------------------
