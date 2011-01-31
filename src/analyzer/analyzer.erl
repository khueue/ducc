-module(analyzer).
-export([analyze/1]).

analyze(Node = {{_,program}, _})         -> analyze_program(Node);
analyze(Node = {{_,scalardec}, _})       -> analyze_scalardec(Node);
analyze(Node = {{_,arraydec}, _})        -> analyze_arraydec(Node);
analyze(Node = {{_,fundec}, _})          -> analyze_fundec(Node);
analyze(Node = {{_,fundef}, _})          -> analyze_fundef(Node);
analyze(Node = {{_,formal_arraydec}, _}) -> analyze_formal_arraydec(Node);
analyze(Node = {{_,'if'}, _})            -> analyze_if(Node);
analyze(Node = {{_,while}, _})           -> analyze_while(Node);
analyze(Node = {{_,return}, _})          -> analyze_return(Node);
analyze(Node = {{_,funcall}, _})         -> analyze_funcall(Node);
analyze(Node = {{_,arrelem}, _})         -> analyze_arrelem(Node);
analyze(Node = {{_,binop}, _})           -> analyze_binop(Node);
analyze(Node = {{_,ident}, _})           -> analyze_ident(Node);
analyze(Node = {{_,intconst}, _})        -> analyze_intconst(Node);
analyze(Node = {{_,charconst}, _})       -> analyze_charconst(Node);
analyze(Node = {{_,unop}, _})            -> analyze_unop(Node);
analyze([]) -> ok;
analyze([X|Xs]) ->
    analyze(X),
    analyze(Xs);
analyze(U) ->
    io:format('~p unhandled~n', [U]),
    ok.

process(X) ->
    io:format('~p~n', [X]).

analyze_program({Meta, {_File, Topdecs}}) ->
    process(Meta),
    analyze(Topdecs).

analyze_scalardec({Meta, {_Type, _Name}}) ->
    process(Meta).

analyze_arraydec({Meta, {_Type, _Name, _Size}}) ->
    process(Meta).

analyze_fundec({Meta, {_Type, _Name, Formals}}) ->
    process(Meta),
    analyze(Formals).

analyze_fundef({Meta, {_Type, _Name, Formals, Locals, Stmts}}) ->
    process(Meta),
    analyze(Formals),
    analyze(Locals),
    analyze(Stmts).

analyze_formal_arraydec({Meta, {_Type, _Name}}) ->
    process(Meta).

analyze_if({Meta, {Cond, Then, Else}}) ->
    process(Meta),
    analyze(Cond),
    analyze(Then),
    analyze(Else).

analyze_while({Meta, {Cond, Stmt}}) ->
    process(Meta),
    analyze(Cond),
    analyze(Stmt).

analyze_return({Meta, {Expr}}) ->
    process(Meta),
    analyze(Expr).

analyze_funcall({Meta, {_Name, Actuals}}) ->
    process(Meta),
    analyze(Actuals).

analyze_arrelem({Meta, {_Name, Index}}) ->
    process(Meta),
    analyze(Index).

analyze_binop({Meta, {Lhs, _Op, Rhs}}) ->
    process(Meta),
    analyze(Lhs),
    analyze(Rhs).

analyze_ident({Meta, {_Name}}) ->
    process(Meta).

analyze_intconst({Meta, {_Value}}) ->
    process(Meta).

analyze_charconst({Meta, {_Char}}) ->
    process(Meta).

analyze_unop({Meta, {_Op, Rhs}}) ->
    process(Meta),
    analyze(Rhs).
