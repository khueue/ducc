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

% Natural number.
{Digit}+ :
    token({intconst, TokenLine, erlang:list_to_integer(TokenChars)}).

% Identifier.
{Letter}({Letter}|{Digit})* :
    Word = erlang:list_to_atom(TokenChars),
    case is_reserved(Word) of
        true  -> token({Word, TokenLine});
        false -> token({ident, TokenLine, TokenChars})
    end.

% Character literal.
'(.|(\\n))' :
    Chars = string:substr(TokenChars, 2, TokenLen-2),
    token({charconst, TokenLine, erlang:list_to_atom(Chars)}).

{LineCom}(.*) :
    skip().

% Multi-line comment.
{BegCom}({NotStar}|{Star}+{NeitherStarSlash})*{Star}*{EndCom} :
    skip().

% Unterminated multi-line comment.
{BegCom}({NotStar}|{Star}+{NeitherStarSlash})*{Star}* :
    error_token("unterminated comment").

{Logical}|{Comparator}|{Symbol} :
    token({erlang:list_to_atom(TokenChars), TokenLine}).

{WhiteSpace}+ :
    skip().

Erlang code.

token(Token) ->
    {token, Token}.

error_token(Message) ->
    {error, Message}.

skip() ->
    skip_token.

is_reserved(Word) ->
    Reserved = ['char','else','if','int','return','void','while'],
    lists:member(Word, Reserved).
