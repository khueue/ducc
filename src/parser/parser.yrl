Nonterminals
program topdec_list topdec fundef vardec scalardec arraydec typename
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
topdec           -> fundef : '$1'.

fundef           -> funtypeandname '(' formals ')' funbody : make_fundef('$1', '$3', '$5').

funtypeandname   -> typename 'ident' : make_funtypeandname('$1', '$2').
funtypeandname   ->   'void' 'ident' : make_funtypeandname('$1', '$2').

vardec           -> scalardec : '$1'.
vardec           -> arraydec : '$1'.

scalardec        -> typename 'ident' : make_scalardec('$1', '$2').

arraydec         -> typename 'ident' '[' 'intconst' ']' : make_arraydec('$1', '$2', '$4').

typename         -> 'int' : '$1'.
typename         -> 'char' : '$1'.

funbody          -> ';' : make_funbody().
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
stmt             -> ';' : [].

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

type_of(Token) ->
    erlang:element(1, Token).

line_of(_) -> 0;
line_of([X|_]) ->
    %io:format('token: ~p~n', [Line]),
    line_of(X);
line_of([]) ->
    %io:format('token: ~p~n', [Line]),
    0; % Error?
line_of(Token) when is_integer(Token) ->
    %io:format('token1: ~p~n', [Token]),
    Token;
line_of({Ett,Tva}) when is_integer(Ett) ->
    %io:format('token2: ~p~n', [{Ett,Tva}]),
    Ett;
line_of({Ett,Tva}) when is_integer(Tva) ->
    %io:format('token3: ~p~n', [{Ett,Tva}]),
    Tva;
line_of({Ett,Tva,Tre}) ->
    io:format('token4: ~p~n', [{Ett,Tva,Tre}]),
    line_of(Tva); % Tva?
line_of({Ett,Tva,Tre,Fyra}) ->
    %io:format('token5: ~p~n', [{Ett,Tva,Tre,Fyra}]),
    line_of(Ett);
line_of(Token) ->
    %io:format('token6: ~p~n', [Token]),
    line_of(erlang:element(1, Token)).

value_of(Token) ->
    erlang:element(3, Token).

make_meta(Line, Tag) ->
    {Line, Tag}.

make_program(Topdecs) ->
    {make_meta(line_of(Topdecs), program), Topdecs}.

make_fundef(FunTypeName, Formals, FunBody) ->
    {make_meta(line_of(FunTypeName), fundef), FunTypeName, Formals, FunBody}.

make_funtypeandname(Type, Ident) ->
   {make_meta(line_of(Type), funtypeandname), type_of(Type), value_of(Ident)}.

make_scalardec(Type, Value) ->
    {make_meta(line_of(Type), scalardec), type_of(Type), value_of(Value)}.

make_arraydec(Type, Ident, Size) ->
    {make_meta(line_of(Type), arraydec), type_of(Type), value_of(Ident), value_of(Size)}.

make_funbody() ->
    nil.
make_funbody(Locals, Stmts) ->
    {Locals, Stmts}.

make_formal_arraydec(Type, Ident) ->
    {make_meta(line_of(Type), formal_arraydec), type_of(Type), value_of(Ident)}.

make_if(Keyword, Cond, Then, Else) ->
    {make_meta(line_of(Keyword), 'if'), Cond, Then, Else}. % if?

make_while(Keyword, Cond, Stmt) ->
    {make_meta(line_of(Keyword), while), Cond, Stmt}.

make_empty_return(Keyword) ->
    {make_meta(line_of(Keyword), return)}.

make_return(Keyword, Expr) ->
    {make_meta(line_of(Keyword), return), Expr}.

make_function_call(Ident, Actuals) ->
    {make_meta(line_of(Ident), funcall), value_of(Ident), Actuals}.

make_array_element(Ident, Index) ->
    {make_meta(line_of(Ident), arrelem), value_of(Ident), Index}.

make_binop(Lhs, Op, Rhs) ->
    {make_meta(line_of(Op), binop), Lhs, type_of(Op), Rhs}.

make_ident(Ident) ->
    {make_meta(line_of(Ident), ident), value_of(Ident)}.

make_intconst(Intconst) ->
    {make_meta(line_of(Intconst), intconst), value_of(Intconst)}.

make_charconst(Charconst) ->
    {make_meta(line_of(Charconst), charconst), value_of(Charconst)}.

make_unop(Op, Rhs) ->
    {make_meta(line_of(Op), unop), type_of(Op), Rhs}.
