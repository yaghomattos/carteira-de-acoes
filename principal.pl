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
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
/*funções de manipulação de lista*/
elemento(X, [X|[]]).
elemento(X,[X|_]).
elemento(X,[_|L]) :- elemento(X,L).

ultimo(X,[X]).
ultimo(X,[_|T]) :- ultimo(X,T).

remove_primeiro([_|X], X). 
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%cadastro de operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)

:- dynamic operacao/7.

data_hoje(D,M,Y) :- get_time(T), stamp_date_time(T, date(Y, M, D, _, _, _, _, _, _), 'UTC').

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
  remove_primeiro(TL, TT),
  calcula_pm(A, PT, NT, TT,PM1,N2).

aux_preco_medio :- 
  forall(acao(A,PL,NL,TL), (elemento(P,PL), elemento(N,NL),elemento(T,TL), 
  PM is N * (P + T)/N, calcula_pm(A,PL,NL,TL,PM,0))).  

imprime_preco_medio :- 
  (write('Ação'),write(' | '),write('Preço Médio'),write(' | '),
  write(' Quantidade '),write(' | '),write('Valor Investido'),write('\n')),
  forall(preco_medio(A,PM,N), (
    N =< 0 -> ! ;
    trunca(PM, PMT),
    Total is PM * N,
    trunca(Total,TotalT),
    (PM >= 10 -> write(A),write('|    '),write(PMT),write('    |      '),
    write(N),write('     | '),write(TotalT),write('\n');
    write(A),write('|    '),write(0),write(PMT),write('    |      '),write(N),
    write('     | '),write(TotalT),write('\n')
    ))).

carteira :- 
  calcula_carteira, 
  imprime_preco_medio.
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
