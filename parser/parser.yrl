Nonterminals
program topdec_list topdec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formal_list formaldec formal_arraydec locals
stmts stmt condition expr operation unop actuals expr_list if_stmt mif uif
rval lval or and comp ineq math term factor function_call array_element.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'integer' 'identifier'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

% Unary 140 '-' '!'.

% Left 130 '*' '/'.
% Left 120 '+' '-'.
% Left 100 '<' '>' '<=' '>='.
% Left 90 '==' '!='.
% Left 50 '&&'.
% Left 40 '||'.

% Right 20 '='.

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
    {'$1', '$3', '$5'}.

funtypeandname   -> typename 'identifier' :
    {value_of('$1'), value_of('$2')}.
funtypeandname   -> void 'identifier' :
    {value_of('$1'), value_of('$2')}.

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
    {line_of('$2'), '$2', '$3'}.
funbody          -> ';'.

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
    {line_of('$1'), '$1'}.
stmt             -> 'return' expr ';' :
    {line_of('$1'), '$1', '$2'}.
stmt             -> 'return' ';' :
    {line_of('$1'), '$1', nil}.
stmt             -> 'while' condition stmt :
    {line_of('$1'), '$1', '$2', '$3'}.
stmt             -> if_stmt :
    {line_of('$1'), $1}.
stmt             -> '{' stmts '}' :
    '$2'.
stmt             -> ';'.

if_stmt          -> mif :
    '$1'.
if_stmt          -> uif :
    '$1'.

mif              -> 'if' condition mif 'else' mif :
    {'$1', '$2', '$3', '$5'}.

uif              -> 'if' condition stmt :
    {'$1', '$2', '$3', nil}.
uif              -> 'if' condition mif 'else' uif :
    {'$1', '$2', '$3', '$5'}.

condition        -> '(' expr ')' :
    '$1'.

%expr             -> 'integer'.
%expr             -> 'identifier'.
expr             ->  array_element :
    '$1'.
%expr             -> unop expr.
%expr             -> expr binop expr.
expr             -> operation :
    '$1'.
expr             -> function_call :
    '$1'.
%expr             -> '(' expr ')'.

array_element    -> 'identifier' '[' expr ']' :
    {value_of('$1'), '$3'}.

function_call    -> 'identifier' '(' actuals ')' :
    {value_of('$1'), '$3'}.

operation        -> rval :
    '$1'.

rval             -> lval '=' rval :
    {binop, '$2', '$1', '$3'}.
rval             -> or :
    '$1'.

lval             -> 'identifier' :
    '$1'.
lval             -> array_element :
    '$1'.

or               -> or '||' and :
    {binop, '$2', '$1', '$3'}.
or               -> and :
    '$1'.

and              -> and '&&' comp.
and              -> comp.

comp             -> comp '==' ineq.
comp             -> comp '!=' ineq.
comp             -> ineq.

ineq             -> ineq '<' math.
ineq             -> ineq '>' math.
ineq             -> ineq '<=' math.
ineq             -> ineq '>=' math.
ineq             -> math.

math             -> math '+' term.
math             -> math '-' term.
math             -> term.

term             -> term '*' factor.
term             -> term '/' factor.
term             -> factor.

factor           -> unop.
factor           -> 'identifier'.
factor           -> 'integer'.
factor           -> '(' expr ')'.

%unop             -> '-'.
%unop             -> '!'.
unop             -> '-' factor.
unop             -> '!' factor.

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
    element(1, Token).

line_of(Token) ->
    element(2, Token).

value_of(Token) ->
    element(3, Token).
