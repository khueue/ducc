Definitions.

Digit               = [0-9]
Letter              = [A-Za-z_]

WhiteSpace          = [\s\n\r\f\t]

Logical             = (&&|(\|\|)|!)
Comparator          = (<|>|<=|>=|==|!=)
Symbol              = []()[}{/;,*+=-]

LineComment         = (//)

MultiCommentStart   = (/\*)
MultiCommentEnd     = (\*/)
% Allowed inside multi-line comments:
NormalChar          = [^*/]
SafeStar            = (\*[^/])
SafeSlash           = ([^*]/)

Rules.

% Integer.
{Digit}+ :
    {token,
        {integer,TokenLine,list_to_integer(TokenChars)}}.

% Identifier.
{Letter}({Letter}|{Digit})* :
    Identifier = list_to_atom(TokenChars),
    case is_reserved(Identifier) of
        true ->
            {token,
                {Identifier,TokenLine}};
        false ->
            {token,
                {identifier,TokenLine,Identifier}}
    end.

% Character literal.
'(.|(\\n))' :
    Chars = string:substr(TokenChars, 2, TokenLen-2),
    {token,
        {character,TokenLine,list_to_atom(Chars)}}.

{LineComment}(.*) :
    skip_token.

{MultiCommentStart}(/*)({NormalChar}|{SafeStar}|{SafeSlash})*(\**){MultiCommentEnd} :
    skip_token.

{Logical}|{Comparator}|{Symbol} :
    {token,
        {list_to_atom(TokenChars),TokenLine}}.

{WhiteSpace}+ :
    skip_token.

Erlang code.

type_of(Word) ->
    case is_reserved(Word) of
        true -> Word;
        false -> identifier
    end.

is_reserved('char') -> true;
is_reserved('else') -> true;
is_reserved('if') -> true;
is_reserved('int') -> true;
is_reserved('return') -> true;
is_reserved('void') -> true;
is_reserved('while') -> true;
is_reserved(_) -> false.
