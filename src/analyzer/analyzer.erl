-module(analyzer).
-export([analyze/1]).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

get_line(Node) ->
    {Line, _Tag} = get_meta(Node),
    Line.

get_meta(Node) ->
    erlang:element(1, Node).

analyze(ParseTree) ->
    SymTab = dict:new(),
    Env = {SymTab},
    {SymTab1} = analyze(ParseTree, Env),
    List = dict:to_list(SymTab1),
    io:format('~p~n', [List]),
    ok.

analyze([], Env) ->
    Env;
analyze([X|Xs], Env) ->
    Env1 = analyze(X, Env),
    Env2 = analyze(Xs, Env1),
    Env2;
analyze(Node, Env) when erlang:is_tuple(Node) ->
    Tag = get_tag(Node),
    Env1 = analyze_node(Tag, Node, Env),
    Env1;
analyze(Unhandled, Env) ->
    io:format('Unhandled: ~p~n', [Unhandled]),
    Env.

analyze_node(program, Node, Env)         -> analyze_program(Node, Env);
analyze_node(scalardec, Node, Env)       -> analyze_scalardec(Node, Env);
analyze_node(arraydec, Node, Env)        -> analyze_arraydec(Node, Env);
analyze_node(fundec, Node, Env)          -> analyze_fundec(Node, Env);
analyze_node(fundef, Node, Env)          -> analyze_fundef(Node, Env);
analyze_node(formal_arraydec, Node, Env) -> analyze_formal_arraydec(Node, Env);
analyze_node('if', Node, Env)            -> analyze_if(Node, Env);
analyze_node(while, Node, Env)           -> analyze_while(Node, Env);
analyze_node(return, Node, Env)          -> analyze_return(Node, Env);
analyze_node(funcall, Node, Env)         -> analyze_funcall(Node, Env);
analyze_node(arrelem, Node, Env)         -> analyze_arrelem(Node, Env);
analyze_node(binop, Node, Env)           -> analyze_binop(Node, Env);
analyze_node(ident, Node, Env)           -> analyze_ident(Node, Env);
analyze_node(intconst, Node, Env)        -> analyze_intconst(Node, Env);
analyze_node(charconst, Node, Env)       -> analyze_charconst(Node, Env);
analyze_node(unop, Node, Env)            -> analyze_unop(Node, Env);
analyze_node(Tag, _, Env) ->
    io:format('Unhandled tag: ~p~n', [Tag]),
    Env.

process(X) ->
    io:format('~p~n', [X]).

analyze_program(Node = {Meta, _File, Topdecs}, Env) ->
    process(Meta),
    _Env1 = analyze(Topdecs, Env).

analyze_scalardec({Meta, _Type, Name}, Env) ->
    process(Meta),
    Env1 = store_key(Name, Meta, Env),
    Env1.

store_key(Key, Value, {SymTab}) ->
    SymTab1 = dict:store(Key, Value, SymTab),
    {SymTab1}.

analyze_arraydec({Meta, _Type, _Name, _Size}, Env) ->
    process(Meta),
    Env.

analyze_fundec({Meta, _Type, _Name, Formals}, Env) ->
    process(Meta),
    analyze(Formals, Env).

analyze_fundef({Meta, _Type, _Name, Formals, Locals, Stmts}, Env) ->
    process(Meta),
    analyze(Formals, Env),
    analyze(Locals, Env),
    analyze(Stmts, Env).

analyze_formal_arraydec({Meta, _Type, _Name}, Env) ->
    process(Meta),
    Env.

analyze_if({Meta, Cond, Then, Else}, Env) ->
    process(Meta),
    analyze(Cond, Env),
    analyze(Then, Env),
    analyze(Else, Env).

analyze_while({Meta, Cond, Stmt}, Env) ->
    process(Meta),
    analyze(Cond, Env),
    analyze(Stmt, Env).

analyze_return({Meta, Expr}, Env) ->
    process(Meta),
    analyze(Expr, Env).

analyze_funcall({Meta, _Name, Actuals}, Env) ->
    process(Meta),
    analyze(Actuals, Env).

analyze_arrelem({Meta, _Name, Index}, Env) ->
    process(Meta),
    analyze(Index, Env).

analyze_binop({Meta, Lhs, _Op, Rhs}, Env) ->
    process(Meta),
    analyze(Lhs, Env),
    analyze(Rhs, Env).

analyze_ident({Meta, _Name}, Env) ->
    process(Meta),
    Env.

analyze_intconst({Meta, _Value}, Env) ->
    process(Meta),
    Env.

analyze_charconst({Meta, _Char}, Env) ->
    process(Meta),
    Env.

analyze_unop({Meta, _Op, Rhs}, Env) ->
    process(Meta),
    analyze(Rhs, Env).
