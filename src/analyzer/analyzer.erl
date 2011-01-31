-module(analyzer).
-export([analyze/1]).

analyze([]) -> ok;
analyze([X|Xs]) ->
    analyze(X),
    analyze(Xs);
analyze(Node) when erlang:is_tuple(Node) ->
    {_Line, Tag} = erlang:element(1, Node),
    analyze_node(Tag, Node);
analyze(U) ->
    io:format('~p unhandled~n', [U]),
    ok.

analyze_node(program, Node)         -> analyze_program(Node);
analyze_node(scalardec, Node)       -> analyze_scalardec(Node);
analyze_node(arraydec, Node)        -> analyze_arraydec(Node);
analyze_node(fundec, Node)          -> analyze_fundec(Node);
analyze_node(fundef, Node)          -> analyze_fundef(Node);
analyze_node(formal_arraydec, Node) -> analyze_formal_arraydec(Node);
analyze_node('if', Node)            -> analyze_if(Node);
analyze_node(while, Node)           -> analyze_while(Node);
analyze_node(return, Node)          -> analyze_return(Node);
analyze_node(funcall, Node)         -> analyze_funcall(Node);
analyze_node(arrelem, Node)         -> analyze_arrelem(Node);
analyze_node(binop, Node)           -> analyze_binop(Node);
analyze_node(ident, Node)           -> analyze_ident(Node);
analyze_node(intconst, Node)        -> analyze_intconst(Node);
analyze_node(charconst, Node)       -> analyze_charconst(Node);
analyze_node(unop, Node)            -> analyze_unop(Node);
analyze_node(Tag, _) ->
    io:format('Unhandled tag: ~p~n', [Tag]).

process(X) ->
    io:format('~p~n', [X]).

analyze_program({Meta, _File, Topdecs}) ->
    process(Meta),
    analyze(Topdecs).

analyze_scalardec({Meta, _Type, _Name}) ->
    process(Meta).

analyze_arraydec({Meta, _Type, _Name, _Size}) ->
    process(Meta).

analyze_fundec({Meta, _Type, _Name, Formals}) ->
    process(Meta),
    analyze(Formals).

analyze_fundef({Meta, _Type, _Name, Formals, Locals, Stmts}) ->
    process(Meta),
    analyze(Formals),
    analyze(Locals),
    analyze(Stmts).

analyze_formal_arraydec({Meta, _Type, _Name}) ->
    process(Meta).

analyze_if({Meta, Cond, Then, Else}) ->
    process(Meta),
    analyze(Cond),
    analyze(Then),
    analyze(Else).

analyze_while({Meta, Cond, Stmt}) ->
    process(Meta),
    analyze(Cond),
    analyze(Stmt).

analyze_return({Meta, Expr}) ->
    process(Meta),
    analyze(Expr).

analyze_funcall({Meta, _Name, Actuals}) ->
    process(Meta),
    analyze(Actuals).

analyze_arrelem({Meta, _Name, Index}) ->
    process(Meta),
    analyze(Index).

analyze_binop({Meta, Lhs, _Op, Rhs}) ->
    process(Meta),
    analyze(Lhs),
    analyze(Rhs).

analyze_ident({Meta, _Name}) ->
    process(Meta).

analyze_intconst({Meta, _Value}) ->
    process(Meta).

analyze_charconst({Meta, _Char}) ->
    process(Meta).

analyze_unop({Meta, _Op, Rhs}) ->
    process(Meta),
    analyze(Rhs).
