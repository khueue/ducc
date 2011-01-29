Nonterminals
program topdec_list topdec fundec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formal_list formaldec formal_arraydec locals
stmts stmt condition expr op_unary actuals expr_list else_part
rval lval or and comp ineq primary term factor function_call array_element
op_ineq op_primary op_term op_factor.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'intconst' 'ident' 'charconst'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

program          -> topdec_list : make_program('$1').

topdec_list      -> '$empty' : [].
topdec_list      -> topdec topdec_list : ['$1'|'$2'].

topdec           -> vardec ';' : '$1'.
topdec           -> fundec ';' : '$1'.
topdec           -> fundef : '$1'.

fundec           -> funtypeandname '(' formals ')' : make_fundec('$1', '$3').

fundef           -> funtypeandname '(' formals ')' funbody : make_fundef('$1', '$3', '$5').

funtypeandname   -> typename 'ident' : make_funtypeandname('$1', '$2').
funtypeandname   ->   'void' 'ident' : make_funtypeandname('$1', '$2').

vardec           -> scalardec : '$1'.
vardec           -> arraydec : '$1'.

scalardec        -> typename 'ident' : make_scalardec('$1', '$2').

arraydec         -> typename 'ident' '[' 'intconst' ']' : make_arraydec('$1', '$2', '$4').

typename         -> 'int' : '$1'.
typename         -> 'char' : '$1'.

funbody          -> '{' locals stmts '}' : make_funbody('$2', '$3').

formals          -> 'void' : [].
formals          -> formal_list : '$1'.

formal_list      -> formaldec : ['$1'].
formal_list      -> formaldec ',' formal_list : ['$1'|'$3'].

formaldec        -> scalardec : '$1'.
formaldec        -> formal_arraydec : '$1'.

formal_arraydec  -> typename 'ident' '[' ']' : make_formal_arraydec('$1', '$2').

locals           -> '$empty' : [].
locals           -> vardec ';' locals : ['$1'|'$3'].

stmts            -> '$empty' : [].
stmts            -> stmt stmts : ['$1'|'$2'].

stmt             -> expr ';' : '$1'.
stmt             -> 'return' expr ';' : make_return('$1', '$2').
stmt             -> 'return' ';' : make_empty_return('$1').
stmt             -> 'while' condition stmt : make_while('$1', '$2', '$3').
stmt             -> 'if' condition stmt else_part : make_if('$1', '$2', '$3', '$4').
stmt             -> '{' stmts '}' : '$2'.
stmt             -> ';' : nil.

else_part        -> '$empty' : nil.
else_part        -> 'else' stmt : '$2'.

condition        -> '(' expr ')' : '$2'.

expr             -> rval : '$1'.

array_element    -> 'ident' '[' expr ']' : make_array_element('$1', '$3').

function_call    -> 'ident' '(' actuals ')' : make_function_call('$1', '$3').

rval             -> lval '=' rval : make_binop('$1', '$2', '$3').
rval             -> or : '$1'.

lval             -> 'ident' : make_ident('$1').
lval             -> array_element : '$1'.

or               -> or '||' and : make_binop('$1', '$2', '$3').
or               -> and : '$1'.

and              -> and '&&' comp : make_binop('$1', '$2', '$3').
and              -> comp : '$1'.

comp             -> comp op_ineq ineq : make_binop('$1', '$2', '$3').
comp             -> ineq : '$1'.
op_ineq          -> '==' : '$1'.
op_ineq          -> '!=' : '$1'.

ineq             -> ineq op_primary primary : make_binop('$1', '$2', '$3').
ineq             -> primary : '$1'.
op_primary       -> '<'  : '$1'.
op_primary       -> '>'  : '$1'.
op_primary       -> '<=' : '$1'.
op_primary       -> '>=' : '$1'.

primary          -> primary op_term term : make_binop('$1', '$2', '$3').
primary          -> term : '$1'.
op_term          -> '+' : '$1'.
op_term          -> '-' : '$1'.

term             -> term op_factor factor : make_binop('$1', '$2', '$3').
term             -> factor : '$1'.
op_factor        -> '*' : '$1'.
op_factor        -> '/' : '$1'.

factor           -> 'ident' : make_ident('$1').
factor           -> 'intconst' : make_intconst('$1').
factor           -> 'charconst' : make_charconst('$1').
factor           -> array_element : '$1'.
factor           -> function_call : '$1'.
factor           -> '(' expr ')' : '$2'.
factor           -> op_unary factor : make_unop('$1', '$2').
op_unary         -> '-' : '$1'.
op_unary         -> '!' : '$1'.

actuals          -> '$empty' : [].
actuals          -> expr_list : '$1'.

expr_list        -> expr : ['$1'].
expr_list        -> expr ',' expr_list : ['$1'|'$3'].

Erlang code.

type(Token) ->
    erlang:element(1, Token).

line(Int) when is_integer(Int) ->
    Int;
line([]) ->
    0; % Probably only appears on an empty file.
line([X|_]) ->
    line(X);
line(_Meta  = {Line,_}) when is_integer(Line) ->
    Line;
line(_Token = {_,Line}) when is_integer(Line) ->
    Line;
line(_Token = {Type,Line,_}) when not(is_tuple(Type)) ->
    Line;
line(Tuple) ->
    First = erlang:element(1, Tuple),
    line(First).

value(Token) ->
    erlang:element(3, Token).

meta(Line, Tag) ->
    {Line, Tag}.

make_program(Topdecs) ->
    {meta(line(Topdecs), program), Topdecs}.

make_fundec(TypeAndName, Formals) ->
    {_Meta, Type, Name} = TypeAndName,
    {meta(line(TypeAndName), fundec), Type, Name, Formals}.

make_fundef(TypeAndName, Formals, Body) ->
    {_Meta, Type, Name} = TypeAndName,
    {meta(line(TypeAndName), fundef), Type, Name, Formals, Body}.

make_funtypeandname(Type, Ident) ->
   {meta(line(Type), funtypeandname), type(Type), value(Ident)}.

make_scalardec(Type, Value) ->
    {meta(line(Type), scalardec), type(Type), value(Value)}.

make_arraydec(Type, Ident, Size) ->
    {meta(line(Type), arraydec), type(Type), value(Ident), value(Size)}.

make_funbody(Locals, Stmts) ->
    {Locals, Stmts}.

make_formal_arraydec(Type, Ident) ->
    {meta(line(Type), formal_arraydec), type(Type), value(Ident)}.

make_if(Keyword, Cond, Then, Else) ->
    {meta(line(Keyword), 'if'), Cond, Then, Else}.

make_while(Keyword, Cond, Stmt) ->
    {meta(line(Keyword), while), Cond, Stmt}.

make_empty_return(Keyword) ->
    {meta(line(Keyword), return)}.

make_return(Keyword, Expr) ->
    {meta(line(Keyword), return), Expr}.

make_function_call(Ident, Actuals) ->
    {meta(line(Ident), funcall), value(Ident), Actuals}.

make_array_element(Ident, Index) ->
    {meta(line(Ident), arrelem), value(Ident), Index}.

make_binop(Lhs, Op, Rhs) ->
    {meta(line(Op), binop), Lhs, type(Op), Rhs}.

make_ident(Ident) ->
    {meta(line(Ident), ident), value(Ident)}.

make_intconst(Intconst) ->
    {meta(line(Intconst), intconst), value(Intconst)}.

make_charconst(Charconst) ->
    {meta(line(Charconst), charconst), value(Charconst)}.

make_unop(Op, Rhs) ->
    {meta(line(Op), unop), type(Op), Rhs}.
