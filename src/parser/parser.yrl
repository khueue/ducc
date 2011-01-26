Nonterminals
program topdec_list topdec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formal_list formaldec formal_arraydec locals
stmts stmt condition expr operation unop actuals expr_list else_part
rval lval or and comp ineq math term factor function_call array_element.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'integer' 'identifier' 'character'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

program          -> topdec_list :
    {program, '$1'}.

topdec_list      -> '$empty' :
    [].
topdec_list      -> topdec topdec_list :
    ['$1' | '$2'].

topdec           -> vardec ';' :
    '$1'.
topdec           -> fundef :
    '$1'.

fundef           -> funtypeandname '(' formals ')' funbody :
    {fundef, '$1', '$3', '$5'}.

funtypeandname   -> typename 'identifier' :
    {type_of('$1'), value_of('$2')}.
funtypeandname   -> void 'identifier' :
    {type_of('$1'), value_of('$2')}.

vardec           -> scalardec :
    '$1'.
vardec           -> arraydec :
    '$1'.

scalardec        -> typename 'identifier' :
    {line_of('$1'), type_of('$1'), value_of('$2'), nil}.

arraydec         -> typename 'identifier' '[' 'integer' ']' :
    {line_of('$1'), type_of('$1'), value_of('$2'), value_of('$4')}.

typename         -> 'int' :
    '$1'.
typename         -> 'char' :
    '$1'.

funbody          -> '{' locals stmts '}' :
    {'$2', '$3'}.
funbody          -> ';' :
    nil.

formals          -> 'void' :
    [].
formals          -> formal_list :
    '$1'.

formal_list      -> formaldec :
    ['$1'].
formal_list      -> formaldec ',' formal_list :
    ['$1' | '$3'].

formaldec        -> scalardec :
    '$1'.
formaldec        -> formal_arraydec :
    '$1'.

formal_arraydec  -> typename 'identifier' '[' ']' :
    {line_of('$1'), type_of('$1'), value_of('$2'), nil}.

locals           -> '$empty' :
    [].
locals           -> vardec ';' locals :
    ['$1' | '$3'].

stmts            -> '$empty' :
    [].
stmts            -> stmt stmts :
    ['$1' | '$2'].

stmt             -> expr ';' :
    '$1'.
stmt             -> 'return' expr ';' :
    {type_of('$1'), '$2'}.
stmt             -> 'return' ';' :
    {type_of('$1'), nil}.
stmt             -> 'while' condition stmt :
    {type_of('$1'), '$2', '$3'}.
stmt             -> 'if' condition stmt else_part :
    {type_of('$1'), '$2', '$3', '$4'}.
stmt             -> '{' stmts '}' :
    '$2'.
stmt             -> ';' :
    [].

else_part          -> '$empty' :
    nil.
else_part          -> 'else' stmt :
    '$2'.

condition        -> '(' expr ')' :
    '$2'.

expr             -> array_element :
    '$1'.
expr             -> operation :
    '$1'.
expr             -> function_call :
    '$1'.

array_element    -> 'identifier' '[' expr ']' :
    {value_of('$1'), '$3'}.

function_call    -> 'identifier' '(' actuals ')' :
    {value_of('$1'), '$3'}.

operation        -> rval :
    '$1'.

rval             -> lval '=' rval :
    {binop, type_of('$2'), '$1', '$3'}.
rval             -> or :
    '$1'.

lval             -> 'identifier' :
    value_of('$1').
lval             -> array_element :
    '$1'.

or               -> or '||' and :
    {binop, type_of('$2'), '$1', '$3'}.
or               -> and :
    '$1'.

and              -> and '&&' comp :
    {binop, type_of('$2'), '$1', '$3'}.
and              -> comp :
    '$1'.

comp             -> comp '==' ineq :
    {binop, type_of('$2'), '$1', '$3'}.
comp             -> comp '!=' ineq :
    {binop, type_of('$2'), '$1', '$3'}.
comp             -> ineq :
    '$1'.

ineq             -> ineq '<' math :
    {binop, type_of('$2'), '$1', '$3'}.
ineq             -> ineq '>' math :
    {binop, type_of('$2'), '$1', '$3'}.
ineq             -> ineq '<=' math :
    {binop, type_of('$2'), '$1', '$3'}.
ineq             -> ineq '>=' math :
    {binop, type_of('$2'), '$1', '$3'}.
ineq             -> math :
    '$1'.

math             -> math '+' term :
    {binop, type_of('$2'), '$1', '$3'}.
math             -> math '-' term :
    {binop, type_of('$2'), '$1', '$3'}.
math             -> term :
    '$1'.

term             -> term '*' factor :
    {binop, type_of('$2'), '$1', '$3'}.
term             -> term '/' factor :
    {binop, type_of('$2'), '$1', '$3'}.
term             -> factor :
    '$1'.

factor           -> unop factor :
    {unop, '$1', '$2'}.
factor           -> 'identifier' :
    value_of('$1').
factor           -> 'integer' :
    value_of('$1').
factor           -> 'character' :
    value_of('$1').
factor           -> '(' expr ')' :
    '$2'.

unop             -> '-' :
    type_of('$1').
unop             -> '!' :
    type_of('$1').

actuals          -> '$empty' :
    [].
actuals          -> expr_list :
    '$1'.

expr_list        -> expr :
    ['$1'].
expr_list        -> expr ',' expr_list :
    ['$1' | '$3'].

Erlang code.

type_of(Token) ->
    erlang:element(1, Token).

line_of(Token) ->
    erlang:element(2, Token).

value_of(Token) ->
    erlang:element(3, Token).
