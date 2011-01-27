Nonterminals
program topdec_list topdec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formal_list formaldec formal_arraydec locals
stmts stmt condition expr unop actuals expr_list else_part
rval lval or and comp ineq math term factor function_call array_element
op_eq op_ineq op_term op_mult.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'integer' 'identifier' 'character'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

program          -> topdec_list : make_program('$1').

topdec_list      -> '$empty' : [].
topdec_list      -> topdec topdec_list : ['$1'|'$2'].

topdec           -> vardec ';' : '$1'.
topdec           -> fundef : '$1'.

fundef           -> funtypeandname '(' formals ')' funbody :
    make_fundef('$1', '$3', '$5').

funtypeandname   -> typename 'identifier' :
    make_funtypeandname(type_of('$1'), value_of('$2')).
funtypeandname   ->   'void' 'identifier' :
    make_funtypeandname(type_of('$1'), value_of('$2')).

vardec           -> scalardec : '$1'.
vardec           -> arraydec : '$1'.

scalardec        -> typename 'identifier' :
    make_scalardec(line_of('$1'), type_of('$1'), value_of('$2'), nil).

arraydec         -> typename 'identifier' '[' 'integer' ']' :
    make_arraydec(line_of('$1'), type_of('$1'), value_of('$2'), value_of('$4')).

typename         -> 'int' : '$1'.
typename         -> 'char' : '$1'.

funbody          -> ';' : make_funbody(nil, nil).
funbody          -> '{' locals stmts '}' : make_funbody('$2', '$3').

formals          -> 'void' : [].
formals          -> formal_list : '$1'.

formal_list      -> formaldec : ['$1'].
formal_list      -> formaldec ',' formal_list : ['$1'|'$3'].

formaldec        -> scalardec : '$1'.
formaldec        -> formal_arraydec : '$1'.

formal_arraydec  -> typename 'identifier' '[' ']' :
    make_formal_arraydec(line_of('$1'), type_of('$1'), value_of('$2'), nil).

locals           -> '$empty' : [].
locals           -> vardec ';' locals : ['$1'|'$3'].

stmts            -> '$empty' : [].
stmts            -> stmt stmts : ['$1'|'$2'].

stmt             -> expr ';' : '$1'.
stmt             -> 'return' expr ';' : make_return(type_of('$1'), '$2').
stmt             -> 'return' ';' : make_return(type_of('$1'), nil).
stmt             -> 'while' condition stmt : make_while(type_of('$1'), '$2', '$3').
stmt             -> 'if' condition stmt else_part :
    make_if(type_of('$1'), '$2', '$3', '$4').
stmt             -> '{' stmts '}' : '$2'.
stmt             -> ';' : [].

else_part        -> '$empty' : nil.
else_part        -> 'else' stmt : '$2'.

condition        -> '(' expr ')' : '$2'.

expr             -> rval : '$1'.

array_element    -> 'identifier' '[' expr ']' : make_array_element(value_of('$1'), '$3').

function_call    -> 'identifier' '(' actuals ')' :
    make_function_call(value_of('$1'), '$3').

rval             -> lval '=' rval : make_binop(type_of('$2'), '$1', '$3').
rval             -> or : '$1'.

lval             -> 'identifier' : value_of('$1').
lval             -> array_element : '$1'.

or               -> or '||' and : make_binop(type_of('$2'), '$1', '$3').
or               -> and : '$1'.

and              -> and '&&' comp : make_binop(type_of('$2'), '$1', '$3').
and              -> comp : '$1'.

comp             -> comp op_eq ineq : make_binop('$2', '$1', '$3').
comp             -> ineq : '$1'.
op_eq            -> '==' : type_of('$1').
op_eq            -> '!=' : type_of('$1').

ineq             -> ineq op_ineq math : make_binop('$2', '$1', '$3').
ineq             -> math : '$1'.
op_ineq          -> '<'  : type_of('$1').
op_ineq          -> '>'  : type_of('$1').
op_ineq          -> '<=' : type_of('$1').
op_ineq          -> '>=' : type_of('$1').

math             -> math op_term term : make_binop('$2', '$1', '$3').
math             -> term : '$1'.
op_term          -> '+' : type_of('$1').
op_term          -> '-' : type_of('$1').

term             -> term op_mult factor : make_binop('$2', '$1', '$3').
term             -> factor : '$1'.
op_mult          -> '*' : type_of('$1').
op_mult          -> '/' : type_of('$1').

factor           -> 'identifier' : value_of('$1').
factor           -> 'integer' : value_of('$1').
factor           -> 'character' : value_of('$1').
factor           -> array_element : '$1'.
factor           -> function_call : '$1'.
factor           -> '(' expr ')' : '$2'.
factor           -> unop factor : make_unop('$1', '$2').
unop             -> '-' : type_of('$1').
unop             -> '!' : type_of('$1').

actuals          -> '$empty' : [].
actuals          -> expr_list : '$1'.

expr_list        -> expr : ['$1'].
expr_list        -> expr ',' expr_list : ['$1'|'$3'].

Erlang code.

type_of(Token) ->
    erlang:element(1, Token).

line_of(Token) ->
    erlang:element(2, Token).

value_of(Token) ->
    erlang:element(3, Token).

make_program(Topdec) ->
    {program, Topdec}.

make_fundef(FunTypeName, Formals, FunBody) ->
    {fundef, FunTypeName, Formals, FunBody}.

make_funtypeandname(Type, Ident) ->
   {Type, Ident}.

make_scalardec(Line, Type, Value, Size) ->
    {Line, Type, Value, Size}.

make_arraydec(Line, Type, IdentValue, IntValue) ->
    {Line, Type, IdentValue, IntValue}.

make_funbody(nil, nil) ->
    nil;
make_funbody(Locals, Stmts) ->
    {Locals, Stmts}.

make_formal_arraydec(Line, Type, Ident, Size) ->
    {Line, Type, Ident, Size}.

make_if(Keyword, Cond, Then, Else) ->
    {Keyword, Cond, Then, Else}.

make_while(Keyword, Cond, Stmt) ->
    {Keyword, Cond, Stmt}.

make_return(Keyword, Expr) ->
    {Keyword, Expr}.

make_function_call(Ident, Actuals) ->
    {funcall, Ident, Actuals}.

make_array_element(Ident, Index) ->
    {Ident, Index}.

make_binop(Op, Lhs, Rhs) ->
    {binop, Op, Lhs, Rhs}.

make_unop(Op, Rhs) ->
    {unop, Op, Rhs}.
