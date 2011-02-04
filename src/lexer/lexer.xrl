Definitions.

Digit               = [0-9]
Letter              = [A-Za-z_]

WhiteSpace          = [\s\n\r\f\t]

Logical             = (&&|(\|\|)|!)
Comparator          = (<|>|<=|>=|==|!=)
Symbol              = []()[}{/;,*+=-]

LineCom             = (//)

BegCom              = (/\*)
EndCom              = (\*/)
NotStar             = [^*]
NeitherStarSlash    = [^*/]
Star                = (\*)

Rules.

% Integer.
{Digit}+ :
    {token,
        {intconst,TokenLine,list_to_integer(TokenChars)}}.

% Identifier.
{Letter}({Letter}|{Digit})* :
    Word = list_to_atom(TokenChars),
    case is_reserved(Word) of
        true ->
            {token,
                {Word,TokenLine}};
        false ->
            {token,
                {ident,TokenLine,TokenChars}}
    end.

% Character literal.
'(.|(\\n))' :
    Chars = string:substr(TokenChars, 2, TokenLen-2),
    {token,
        {charconst,TokenLine,list_to_atom(Chars)}}.

{LineCom}(.*) :
    skip_token.

% Old, does not accept "/* * / */":
% {BegCom}{Slash}*({NeitherStarSlash}|{NotStar}{Slash}|{Star}{NotSlash})*{Star}*{EndCom} :

{BegCom}({NotStar}*{Star}+{NeitherStarSlash})*{NotStar}*{Star}*{EndCom} :
    skip_token.

{Logical}|{Comparator}|{Symbol} :
    {token,
        {list_to_atom(TokenChars),TokenLine}}.

{WhiteSpace}+ :
    skip_token.

Erlang code.

is_reserved(Word) ->
    Reserved = ['char','else','if','int','return','void','while'],
    lists:member(Word, Reserved).
