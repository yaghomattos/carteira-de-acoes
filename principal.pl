%-----------------------------------------------------------------------------------------
/*:- use_module(library(persistency)).

:- persistent operacao(fact1:any, fact2:any, fact3:any, fact4:any).
:- persistent compra(fact1:any, fact2:any, fact3:any, fact4:any, fact5:any, fact6:any, fact7:any).
:- persistent venda(fact1:any, fact2:any, fact3:any, fact4:any, fact5:any, fact6:any, fact7:any).

:- initialization(init).

init:-
  absolute_file_name('carteira.pl', File, [access(write)]),
  db_attach(File, []).*/

salva(Predicado,Arquivo) :-
 tell(Arquivo),
 listing(Predicado),
 told.

%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%empresa(Ticket, Preço Unitário)
empresa("PETR4", 15.34).
empresa("ITSA4", 8.51).
empresa("BBAS3", 28.51).
empresa("MOVI3", 8.50).
empresa("VIIA3", 4.03).

empresas() :- listing(empresa/2).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%cadastro de operações(Data, Ticket, Operacao, Preço Unitário, Quantidade, Taxas, Custo Total)

%operacao(Data, Ticket, "Compra", Preço Unitário, Quantidade, Taxas, CustoTotal)
operacao(11/03/2020, "PETR4", "Compra", 15.34, 200, 0.92, 3068.92).
operacao(12/03/2020, "PETR4", "Compra", 12.68, 300, 1.14, 3805.14).
operacao(18/03/2020, "PETR4", "Compra", 10.98, 100, 0.32, 1098.32).
operacao(20/03/2020, "ITSA4", "Compra", 8.51, 100, 0.25, 851.25).
operacao(31/03/2020, "BBAS3", "Compra", 28.51, 100, 0.85, 2850.85).
operacao(31/03/2020, "MOVI3", "Compra", 8.50, 100, 0.25, 850.25).
operacao(31/03/2020, "MOVI3", "Compra", 8.00, 100, 0.24, 800.24).
operacao(01/04/2020, "AMAR3", "Compra", 3.98, 200, 0.23, 796.23).
operacao(02/04/2020, "AMAR3", "Compra", 3.79, 100, 0.11, 379.11).
operacao(03/04/2020, "VIIA3", "Compra", 4.03, 100, 0.12, 403.12).
operacao(03/04/2020, "BBSE3", "Compra", 22.81, 100, 0.68, 2281.68).
operacao(03/04/2020, "CMIG4", "Compra", 7.99, 100, 0.23, 799.23).
operacao(24/04/2020, "BBAS3", "Compra", 24.99, 200, 1.49, 4999.49).
operacao(07/05/2020, "CIEL3", "Compra", 3.62, 200, 0.21, 362.21).


%venda(Data, Ticket, "Venda", Preço Unitário, Quantidade, Taxas, Custo Total)
operacao(19/03/2020, "PETR4", "Venda", 12.60, -100, 0.37, -1259.63).
operacao(02/04/2020, "PETR4", "Venda", 16.00, -200, 0.96, -3199.04).
operacao(07/04/2020, "ITSA4", "Venda", 9.10, -100, 0.27, -909.73).
operacao(24/03/2020, "PETR4", "Venda", 13.29, -300, 1.19, -3985.81).
operacao(07/04/2020, "BBAS3", "Venda", 30.11, -100, 0.90, -3010.10). 
operacao(08/04/2020, "MOVI3", "Venda", 9.18, -200, 0.55, -1835.45).
operacao(09/04/2020, "VIIA3", "Venda", 5.41, -100, 0.16, -540.84).
operacao(09/04/2020, "CMIG4", "Venda", 9.42, -100, 0.28, -941.72).
operacao(14/04/2020, "BBSE3", "Venda", 27.00, -100, 0.81, -2699.19).
operacao(15/04/2020, "AMAR3", "Venda", 5.54, -300, 0.49, -1661.51).

operacoes() :- salva(operacao(_,_,_,_,_,_,_),'operacoes.pl'), listing(operacao/7).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%carteira de ações(Ação, Preço Médio, Quantidade, Valor Investido)

:- dynamic acao/4.

calcula() :- retractall(acao(A,B,C,D)), forall(operacao(_,A,_,B,C,_,D), assert(acao(A,B,C,D))), salva(acao(_,_,_,_),'carteira.pl').

carteira() :- calcula(), listing(acao/4).
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%lucro / prejuízo
%-----------------------------------------------------------------------------------------


%-----------------------------------------------------------------------------------------
%operações por ação
%-----------------------------------------------------------------------------------------
