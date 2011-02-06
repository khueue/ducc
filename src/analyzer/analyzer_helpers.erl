-module(analyzer_helpers).
-export([exception/3,
         get_meta/1,
         get_tag/1,
         get_type/1,
         tag_member/2,
         convertible_types/3,
         convertible_to/3,
         eval_type/2]).

exception(Node, Format, Args) ->
    Message = io_lib:format(Format, Args),
    {analyzer_exception, {get_line(Node), Message}}.

get_line(Node) ->
    {Line, _Tag} = get_meta(Node),
    Line.

get_meta(Node) ->
    erlang:element(1, Node).

get_tag(Node) ->
    {_Line, Tag} = get_meta(Node),
    Tag.

get_type(Node) ->
    erlang:element(2, Node).

tag_member(Node, Tags) ->
    Tag = get_tag(Node),
    lists:member(Tag, Tags).

convertible_types([], [], _Env) -> ok;
convertible_types([F|Formals], [A|Actuals], Env) ->
    convertible_to(eval_type(F, Env), eval_type(A, Env), A),
    convertible_types(Formals, Actuals, Env).

convertible_to(ExpectedTuple, ActualTuple, Actual) ->
    try first_accepts_second(ExpectedTuple, ActualTuple)
    catch
        incompatible ->
            throw(exception(Actual, 'inconvertible types', []))
    end.

eval_type(nil, _Env) ->
    {empty_return_expr, void};
eval_type(Node = {{_, binop}, Lhs, _Op, Rhs}, Env) ->
    Type = widest_type(eval_type(Lhs, Env), eval_type(Rhs, Env), Node),
    {binop, Type};
eval_type(_Node = {{_, unop}, _Op, Rhs}, Env) ->
    {_, Type} = eval_type(Rhs, Env),
    {unop, Type};
eval_type(Node = {{_, ident}, Name}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {get_tag(FoundNode), get_type(FoundNode)};
eval_type(_Node = {{_, intconst}, _Name}, _Env) ->
    {intconst, int};
eval_type(_Node = {{_, charconst}, _Name}, _Env) ->
    {charconst, char};
eval_type(Node = {{_, arrelem}, Name, _Index}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {arrelem, get_type(FoundNode)};
eval_type(Node = {{_, funcall}, Name, _Actuals}, Env) ->
    FoundNode = analyzer_env:lookup(Name, Node, Env),
    {funcall, get_type(FoundNode)};
eval_type(_Node = {{_, fundec}, Type, _Name, _Formals}, _Env) ->
    {fundec, Type};
eval_type(_Node = {{_, fundef}, Type, _Name, _Formals, _Locals, _Stmts}, _Env) ->
    {fundef, Type};
eval_type(_Node = {{_, scalardec}, Type, _Name}, _Env) ->
    {scalardec, Type};
eval_type(_Node = {{_, arraydec}, Type, _Name, _Size}, _Env) ->
    {arraydec, Type};
eval_type(_Node = {{_, formal_arraydec}, Type, _Name}, _Env) ->
    {formal_arraydec, Type}.

widest_type(Type1, Type2, Node) ->
    try widest_type(Type1, Type2)
    catch
        incompatible ->
            throw(exception(Node, 'incompatible types', []))
    end.

widest_type({_,_}, {arraydec,_})        -> throw(incompatible);
widest_type({arraydec,_}, {_,_})        -> throw(incompatible);
widest_type({_,_}, {formal_arraydec,_}) -> throw(incompatible);
widest_type({formal_arraydec,_}, {_,_}) -> throw(incompatible);
widest_type({_,int}, {_,int})           -> int;
widest_type({_,int}, {_,char})          -> int;
widest_type({_,char}, {_,char})         -> char;
widest_type({_,char}, {_,int})          -> int;
widest_type({_,_}, {_,_})               -> throw(incompatible).

first_accepts_second({formal_arraydec, Type}, {arraydec, Type})        -> ok;
first_accepts_second({formal_arraydec, Type}, {formal_arraydec, Type}) -> ok;
first_accepts_second(_, {arraydec,_}) -> throw(incompatible);
first_accepts_second({arraydec,_}, _) -> throw(incompatible);
first_accepts_second(_, {formal_arraydec,_}) -> throw(incompatible);
first_accepts_second({_,void}, {_,void}) -> ok;
first_accepts_second({_,int}, {_,int})   -> ok;
first_accepts_second({_,int}, {_,char})  -> ok;
first_accepts_second({_,char}, {_,char}) -> ok;
first_accepts_second({_,char}, {_,int})  -> ok;
first_accepts_second(_, _) -> throw(incompatible).
