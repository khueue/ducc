Nonterminals
program topdec_list topdec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formal_list formaldec formal_arraydec locals
stmts stmt condition expr unop actuals expr_list else_part
rval lval or and comp ineq primary term factor function_call array_element
op_ineq op_primary op_term op_factor.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'intconst' 'ident' 'charconst'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

program          -> topdec_list : make_program(line_of('$1'), '$1').

topdec_list      -> '$empty' : [].
topdec_list      -> topdec topdec_list : ['$1'|'$2'].

topdec           -> vardec ';' : '$1'.
topdec           -> fundef : '$1'.

fundef           -> funtypeandname '(' formals ')' funbody :
    make_fundef(line_of('$1'), '$1', '$3', '$5').

funtypeandname   -> typename 'ident' :
    make_funtypeandname(line_of('$1'), type_of('$1'), value_of('$2')).
funtypeandname   ->   'void' 'ident' :
    make_funtypeandname(line_of('$1'), type_of('$1'), value_of('$2')).

vardec           -> scalardec : '$1'.
vardec           -> arraydec : '$1'.

scalardec        -> typename 'ident' :
    make_scalardec(line_of('$1'), type_of('$1'), value_of('$2'), nil).

arraydec         -> typename 'ident' '[' 'intconst' ']' :
    make_arraydec(line_of('$1'), type_of('$1'), value_of('$2'), value_of('$4')).

typename         -> 'int' : '$1'.
typename         -> 'char' : '$1'.

funbody          -> ';' :
    make_funbody(line_of('$1'), nil, nil).
funbody          -> '{' locals stmts '}' :
    make_funbody(line_of('$1'), '$2', '$3').

formals          -> 'void' : [].
formals          -> formal_list : '$1'.

formal_list      -> formaldec : ['$1'].
formal_list      -> formaldec ',' formal_list : ['$1'|'$3'].

formaldec        -> scalardec : '$1'.
formaldec        -> formal_arraydec : '$1'.

formal_arraydec  -> typename 'ident' '[' ']' :
    make_formal_arraydec(line_of('$1'), type_of('$1'), value_of('$2'), nil).

locals           -> '$empty' : [].
locals           -> vardec ';' locals : ['$1'|'$3'].

stmts            -> '$empty' : [].
stmts            -> stmt stmts : ['$1'|'$2'].

stmt             -> expr ';' : '$1'.
stmt             -> 'return' expr ';' :
    make_return(line_of('$1'), type_of('$1'), '$2').
stmt             -> 'return' ';' :
    make_return(line_of('$1'), type_of('$1'), nil).
stmt             -> 'while' condition stmt :
    make_while(line_of('$1'), type_of('$1'), '$2', '$3').
stmt             -> 'if' condition stmt else_part :
    make_if(line_of('$1'), type_of('$1'), '$2', '$3', '$4').
stmt             -> '{' stmts '}' : '$2'.
stmt             -> ';' : [].

else_part        -> '$empty' : nil.
else_part        -> 'else' stmt : '$2'.

condition        -> '(' expr ')' : '$2'.

expr             -> rval : '$1'.

array_element    -> 'ident' '[' expr ']' :
    make_array_element(line_of('$1'), value_of('$1'), '$3').

function_call    -> 'ident' '(' actuals ')' :
    make_function_call(line_of('$1'), value_of('$1'), '$3').

rval             -> lval '=' rval :
    make_binop(line_of('$1'), type_of('$2'), '$1', '$3').
rval             -> or : '$1'.

lval             -> 'ident' : value_of('$1').
lval             -> array_element : '$1'.

or               -> or '||' and :
    make_binop(line_of('$1'), type_of('$2'), '$1', '$3').
or               -> and : '$1'.

and              -> and '&&' comp :
    make_binop(line_of('$1'), type_of('$2'), '$1', '$3').
and              -> comp : '$1'.

comp             -> comp op_ineq ineq :
    make_binop(line_of('$1'), '$2', '$1', '$3').
comp             -> ineq : '$1'.
op_ineq          -> '==' : type_of('$1').
op_ineq          -> '!=' : type_of('$1').

ineq             -> ineq op_primary primary :
    make_binop(line_of('$1'), '$2', '$1', '$3').
ineq             -> primary : '$1'.
op_primary       -> '<'  : type_of('$1').
op_primary       -> '>'  : type_of('$1').
op_primary       -> '<=' : type_of('$1').
op_primary       -> '>=' : type_of('$1').

primary          -> primary op_term term :
    make_binop(line_of('$1'), '$2', '$1', '$3').
primary          -> term : '$1'.
op_term          -> '+' : type_of('$1').
op_term          -> '-' : type_of('$1').

term             -> term op_factor factor :
    make_binop(line_of('$1'), '$2', '$1', '$3').
term             -> factor : '$1'.
op_factor        -> '*' : type_of('$1').
op_factor        -> '/' : type_of('$1').

factor           -> 'ident' :
    make_ident(line_of('$1'), type_of('$1'), value_of('$1')).
factor           -> 'intconst' :
    make_intconst(line_of('$1'), type_of('$1'), value_of('$1')).
factor           -> 'charconst' :
    make_charconst(line_of('$1'), type_of('$1'), value_of('$1')).
factor           -> array_element : '$1'.
factor           -> function_call : '$1'.
factor           -> '(' expr ')' : '$2'.
factor           -> unop factor : make_unop(line_of('$1'), '$1', '$2').
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

make_meta(Line, Tag) ->
    {Line, Tag}.

make_program(Line, Topdec) ->
    {make_meta(Line, program), Topdec}.

make_fundef(Line, FunTypeName, Formals, FunBody) ->
    {make_meta(Line, fundef), FunTypeName, Formals, FunBody}.

make_funtypeandname(Line, Type, Ident) ->
   {make_meta(Line, Type), Ident}.

make_scalardec(Line, Type, Value, Size) ->
    {make_meta(Line, Type), Value, Size}.

make_arraydec(Line, Type, IdentValue, IntValue) ->
    {make_meta(Line, Type), IdentValue, IntValue}.

make_funbody(Line, nil, nil) ->
    {make_meta(Line, funbody), nil};
make_funbody(Line, Locals, Stmts) ->
    {make_meta(Line, funbody), Locals, Stmts}.

make_formal_arraydec(Line, Type, Ident, Size) ->
    {make_meta(Line, Type), Ident, Size}.

make_if(Line, Keyword, Cond, Then, Else) ->
    {make_meta(Line, Keyword), Cond, Then, Else}.

make_while(Line, Keyword, Cond, Stmt) ->
    {make_meta(Line, Keyword), Cond, Stmt}.

make_return(Line, Keyword, Expr) ->
    {make_meta(Line, Keyword), Expr}.

make_function_call(Line, Ident, Actuals) ->
    {make_meta(Line, funcall), Ident, Actuals}.

make_array_element(Line, Ident, Index) ->
    {make_meta(Line, arrelem), Ident, Index}.

make_binop(Line, Op, Lhs, Rhs) ->
    {make_meta(Line, binop), Op, Lhs, Rhs}.

make_ident(Line, Type, Value) ->
    {make_meta(Line, Type), Value}.

make_intconst(Line, Type, Value) ->
    {make_meta(Line, Type), Value}.

make_charconst(Line, Type, Value) ->
    {make_meta(Line, Type), Value}.

make_unop(Line, Op, Rhs) ->
    {make_meta(Line, unop), Op, Rhs}.
