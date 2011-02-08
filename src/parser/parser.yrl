Nonterminals
program topdecs topdec fundec fundef vardec scalardec arraydec typename
funtypeandname funbody formals formals_list formaldec farraydec locals
stmts stmt condition expr op_unary actuals actuals_list else_part
expr_rval expr_lval expr_or expr_and expr_comp expr_ineq expr_primary
expr_term expr_factor function_call array_element
op_ineq op_primary op_term op_factor
stmt_flow stmt_return stmt_while stmt_if.

Terminals '&&' '||' '!'
'<' '>' '<=' '>=' '==' '!='
']' '(' ')' '[' '}' '{' '/' ';' ',' '*' '+' '=' '-'
'intconst' 'ident' 'charconst'
'char' 'else' 'if' 'int' 'return' 'void' 'while'.

Rootsymbol program.

program        -> topdecs : make_program('$1').

topdecs        -> '$empty' : [].
topdecs        -> topdec topdecs : ['$1'|'$2'].

topdec         -> vardec ';' : '$1'.
topdec         -> fundec ';' : '$1'.
topdec         -> fundef : '$1'.

fundec         -> funtypeandname '(' formals ')' : make_fundec('$1', '$3').

fundef         -> funtypeandname '(' formals ')' funbody : make_fundef('$1', '$3', '$5').

funtypeandname -> typename 'ident' : make_funtypeandname('$1', '$2').
funtypeandname -> 'void' 'ident' : make_funtypeandname('$1', '$2').

vardec         -> scalardec : '$1'.
vardec         -> arraydec : '$1'.

scalardec      -> typename 'ident' : make_scalardec('$1', '$2').

arraydec       -> typename 'ident' '[' 'intconst' ']' : make_arraydec('$1', '$2', '$4').

typename       -> 'int' : '$1'.
typename       -> 'char' : '$1'.

funbody        -> '{' locals stmts '}' : make_funbody('$2', '$3').

formals        -> 'void' : [].
formals        -> formals_list : '$1'.

formals_list   -> formaldec : ['$1'].
formals_list   -> formaldec ',' formals_list : ['$1'|'$3'].

formaldec      -> scalardec : '$1'.
formaldec      -> farraydec : '$1'.

farraydec      -> typename 'ident' '[' ']' : make_farraydec('$1', '$2').

locals         -> '$empty' : [].
locals         -> vardec ';' locals : ['$1'|'$3'].

stmts          -> '$empty' : make_stmts([]).
stmts          -> stmt stmts : make_stmts(['$1'|'$2']).

stmt           -> expr ';' : '$1'.
stmt           -> stmt_flow : '$1'.
stmt           -> '{' stmts '}' : '$2'.
stmt           -> ';' : nil.

stmt_flow      -> stmt_return ';' : '$1'.
stmt_flow      -> stmt_while : '$1'.
stmt_flow      -> stmt_if : '$1'.

stmt_return    -> 'return' expr : make_return('$1', '$2').
stmt_return    -> 'return' : make_return('$1').

stmt_while     -> 'while' condition stmt : make_while('$1', '$2', '$3').

stmt_if        -> 'if' condition stmt else_part : make_if('$1', '$2', '$3', '$4').

else_part      -> '$empty' : nil.
else_part      -> 'else' stmt : '$2'.

condition      -> '(' expr ')' : '$2'.

expr           -> expr_rval : '$1'.

expr_rval      -> expr_lval '=' expr_rval : make_binop('$1', '$2', '$3').
expr_rval      -> expr_or : '$1'.

expr_lval      -> 'ident' : make_ident('$1').
expr_lval      -> array_element : '$1'.

expr_or        -> expr_or '||' expr_and : make_binop('$1', '$2', '$3').
expr_or        -> expr_and : '$1'.

expr_and       -> expr_and '&&' expr_comp : make_binop('$1', '$2', '$3').
expr_and       -> expr_comp : '$1'.

expr_comp      -> expr_comp op_ineq expr_ineq : make_binop('$1', '$2', '$3').
expr_comp      -> expr_ineq : '$1'.
op_ineq        -> '==' : '$1'.
op_ineq        -> '!=' : '$1'.

expr_ineq      -> expr_ineq op_primary expr_primary : make_binop('$1', '$2', '$3').
expr_ineq      -> expr_primary : '$1'.
op_primary     -> '<'  : '$1'.
op_primary     -> '>'  : '$1'.
op_primary     -> '<=' : '$1'.
op_primary     -> '>=' : '$1'.

expr_primary   -> expr_primary op_term expr_term : make_binop('$1', '$2', '$3').
expr_primary   -> expr_term : '$1'.
op_term        -> '+' : '$1'.
op_term        -> '-' : '$1'.

expr_term      -> expr_term op_factor expr_factor : make_binop('$1', '$2', '$3').
expr_term      -> expr_factor : '$1'.
op_factor      -> '*' : '$1'.
op_factor      -> '/' : '$1'.

expr_factor    -> 'ident' : make_ident('$1').
expr_factor    -> 'intconst' : make_intconst('$1').
expr_factor    -> 'charconst' : make_charconst('$1').
expr_factor    -> array_element : '$1'.
expr_factor    -> function_call : '$1'.
expr_factor    -> '(' expr ')' : '$2'.
expr_factor    -> op_unary expr_factor : make_unop('$1', '$2').
op_unary       -> '-' : '$1'.
op_unary       -> '!' : '$1'.

array_element  -> 'ident' '[' expr ']' : make_array_element('$1', '$3').

function_call  -> 'ident' '(' actuals ')' : make_function_call('$1', '$3').

actuals        -> '$empty' : [].
actuals        -> actuals_list : '$1'.

actuals_list   -> expr : ['$1'].
actuals_list   -> expr ',' actuals_list : ['$1'|'$3'].

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
line(_Token = {Type,Line,_}) when not is_tuple(Type) ->
    Line;
line(Tuple) ->
    First = erlang:element(1, Tuple),
    line(First).

value(Token) ->
    erlang:element(3, Token).

meta(Line, Tag) ->
    {Line, Tag}.

make_program(Topdecs) ->
    Topdecs.

make_fundec(TypeAndName, Formals) ->
    {_Meta, Type, Name} = TypeAndName,
    {meta(line(TypeAndName), fundec), Type, Name, Formals}.

make_fundef(TypeAndName, Formals, Body) ->
    {_Meta, Type, Name} = TypeAndName,
    {Locals, Stmts} = Body,
    {meta(line(TypeAndName), fundef), Type, Name, Formals, Locals, Stmts}.

make_funtypeandname(Type, Ident) ->
   {meta(line(Type), funtypeandname), type(Type), value(Ident)}.

make_stmts([]) -> [];
make_stmts([nil|Stmts]) ->
    % Skip empty statement.
    Stmts;
make_stmts(Stmts) ->
    Stmts.

make_scalardec(Type, Value) ->
    {meta(line(Type), scalardec), type(Type), value(Value)}.

make_arraydec(Type, Ident, Size) ->
    {meta(line(Type), arraydec), type(Type), value(Ident), value(Size)}.

make_funbody(Locals, Stmts) ->
    {Locals, Stmts}.

make_farraydec(Type, Ident) ->
    {meta(line(Type), farraydec), type(Type), value(Ident)}.

make_if(Keyword, Cond, Then, Else) ->
    {meta(line(Keyword), 'if'), Cond, Then, Else}.

make_while(Keyword, Cond, Stmt) ->
    {meta(line(Keyword), while), Cond, Stmt}.

make_return(Keyword) ->
    {meta(line(Keyword), return), nil}.
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
