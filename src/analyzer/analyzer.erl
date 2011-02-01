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
    Env = {[SymTab]},
    {_SymTabs1} = analyze(ParseTree, Env),
    %List = dict:to_list(SymTab1),
    %io:format('~p~n', [List]),
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

push_symtab(SymTabNew, {SymTabs}) ->
    {[SymTabNew|SymTabs]}.

peek_symtab({[]}) ->
    throw(oooooooooooooooooooj);
peek_symtab({[SymTab|SymTabs]}) ->
    {SymTab, SymTabs}.

process(X) ->
    io:format('~p~n', [X]).

add_key(Key, Value, {SymTabs}) ->
    {CurrentSymTab, Rest} = peek_symtab({SymTabs}),
    CurrentSymTab1 = dict:store(Key, Value, CurrentSymTab),
    {[CurrentSymTab1|Rest]}.

analyze_program({Meta, _File, Topdecs}, Env) ->
    process(Meta),
    Env1 = analyze(Topdecs, Env),
    Env1.

analyze_scalardec(Node = {Meta, _Type, Name}, Env) ->
    process(Meta),
    {CurrentSymTab, _Others} = peek_symtab(Env),
    Defined = dict:is_key(Name, CurrentSymTab),
    Env1 = case Defined of
        true  -> throw({analyzer_exception, {get_line(Node), 'already defined'}});
        false -> add_key(Name, Meta, Env)
    end,
    Env1.

analyze_arraydec(Node = {Meta, _Type, Name, Size}, Env) ->
    process(Meta),
    {CurrentSymTab, _Rest} = peek_symtab(Env),
    Defined = dict:is_key(Name, CurrentSymTab),
    Env1 = case Defined of
        true -> throw({analyzer_exception, {get_line(Node), 'already defined'}});
        false -> Env
    end,
    Env2 = case erlang:is_integer(Size) andalso Size >= 0 of
        true -> add_key(Name, Meta, Env1);
        false -> throw({analyzer_exception, {get_line(Node), 'invalid size'}})
    end,
    Env2.

analyze_fundec({Meta, _Type, _Name, Formals}, Env) ->
    process(Meta),
    Env1 = push_symtab(dict:new(), Env),
    _Env2 = analyze(Formals, Env1),
    Env. % Updates to the environment are local to the function!

analyze_fundef({Meta, _Type, _Name, Formals, Locals, Stmts}, Env) ->
    process(Meta),
    Env1 = push_symtab(dict:new(), Env),
    Env2 = analyze(Formals, Env1),
    Env3 = analyze(Locals, Env2),
    _Env4 = analyze(Stmts, Env3),
    Env. % Updates to the environment are local to the function!

analyze_formal_arraydec({Meta, _Type, _Name}, Env) ->
    process(Meta),
    Env.

analyze_if({Meta, Cond, Then, Else}, Env) ->
    process(Meta),
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Then, Env1),
    Env3 = analyze(Else, Env2),
    Env3.

analyze_while({Meta, Cond, Stmt}, Env) ->
    process(Meta),
    Env1 = analyze(Cond, Env),
    Env2 = analyze(Stmt, Env1),
    Env2.

analyze_return({Meta, Expr}, Env) ->
    process(Meta),
    Env1 = analyze(Expr, Env),
    Env1.

analyze_funcall({Meta, _Name, Actuals}, Env) ->
    process(Meta),
    Env1 = analyze(Actuals, Env),
    Env1.

analyze_arrelem({Meta, _Name, Index}, Env) ->
    process(Meta),
    Env1 = analyze(Index, Env),
    Env1.

analyze_binop({Meta, Lhs, _Op, Rhs}, Env) ->
    process(Meta),
    Env1 = analyze(Lhs, Env),
    Env2 = analyze(Rhs, Env1),
    Env2.

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
    Env1 = analyze(Rhs, Env),
    Env1.
