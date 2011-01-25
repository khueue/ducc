Nonterminals
program topdec_list topdec vardec scalardec arraydec typename funtypeandname
funbody formals formal_list formaldec locals stmts stmt condition
expr unop binop actuals expr_list if_stmt mif uif.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'integer' 'identifier' 'character'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

% Unary 140 '-' '!'.

Left 130 '*' '/'.
Left 120 '+' '-'.
Left 100 '<' '>' '<=' '>='.
Left 90 '==' '!='.
Left 50 '&&'.
Left 40 '||'.

Right 20 '='.

program          -> topdec_list :
    {program, '$1'}.

topdec_list      -> '$empty'.
topdec_list      -> topdec topdec_list :
    {topdec_list, '$1', '$2'}.

topdec           -> vardec ';' :
    {topdec, '$1'}.
topdec           -> funtypeandname '(' formals ')' funbody.

funtypeandname   -> typename 'identifier'.
funtypeandname   -> void 'identifier'.

vardec           -> scalardec :
    {vardec, '$1'}.
vardec           -> arraydec.

scalardec        -> typename 'identifier' :
    {scalardec, '$1', '$2'}.

arraydec         -> typename 'identifier' '[' 'integer' ']'.

typename         -> 'int' :
    '$1'.
typename         -> 'char' :
    '$1'.

funbody          -> '{' locals stmts '}'.
funbody          -> ';'.

formals          -> 'void'.
formals          -> formal_list.

formal_list      -> formaldec.
formal_list      -> formaldec ',' formal_list.

formaldec        -> scalardec.
formaldec        -> typename identifier '[' ']'.

locals           -> '$empty'.
locals           -> vardec ';' locals.

stmts            -> '$empty'.
stmts            -> stmt stmts.

stmt             -> expr ';'.
stmt             -> 'return' expr ';'.
stmt             -> 'return' ';'.
stmt             -> 'while' condition stmt.
stmt             -> if_stmt.
stmt             -> '{' stmts '}'.
stmt             -> ';'.

if_stmt          -> mif.
if_stmt          -> uif.

mif              -> 'if' condition mif 'else' mif.

uif              -> 'if' condition stmt.
uif              -> 'if' condition mif 'else' uif.

condition        -> '(' expr ')'.

expr             -> integer.
expr             -> identifier.
expr             -> identifier '[' expr ']'.
expr             -> unop expr.
expr             -> expr binop expr.
expr             -> identifier '(' actuals ')'.
expr             -> '(' expr ')'.

unop             -> '-'.
unop             -> '!'.

binop            -> '+'.
binop            -> '-'.
binop            -> '*'.
binop            -> '/'.
binop            -> '<'.
binop            -> '>'.
binop            -> '<='.
binop            -> '>='.
binop            -> '!='.
binop            -> '=='.
binop            -> '&&'.
binop            -> '||'.
binop            -> '='.

actuals          -> '$empty'.
actuals          -> expr_list.

expr_list        -> expr.
expr_list        -> expr ',' expr_list.
