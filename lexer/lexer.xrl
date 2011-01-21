Definitions.

Digit = [0-9]
WhiteSpace = [\s\n\r\f\t]
Letter = [A-Za-z_]
Logical = (&&|(\|\|)|!)
Comparator = (<|>|<=|>=|==|!=)
Symbol = []()[}{/;,*+=-]
LineComment = (//)
MultiCommentStart = (/\*)
MultiCommentEnd   = (\*/)
StuffInComment = ([^*/]|[^*]/|\*[^/])

Rules.

% Integer.
{Digit}+ :
    {token,
        {integer,TokenLine,list_to_integer(TokenChars)}}.

% Identifier.
{Letter}({Letter}|{Digit})* :
    Identifier = list_to_atom(TokenChars),
    {token,
        {type_of(Identifier),TokenLine,Identifier}}.

% Character literal.
'(.|(\\n))' :
    {token,
        {character,TokenLine,list_to_atom(TokenChars)}}.

{LineComment}(.*) :
    skip_token.

{MultiCommentStart}(/*)({StuffInComment}*)(\**){MultiCommentEnd} :
    skip_token.

{Logical}|{Comparator}|{Symbol} :
    {token,
        {symbol,TokenLine,list_to_atom(TokenChars)}}.

%{Comparator} :
%    {token,
%        {comparator,TokenLine,list_to_atom(TokenChars)}}.

%{Symbol} :
%    {token,
%        {symbol,TokenLine,list_to_atom(TokenChars)}}.

{WhiteSpace}+ :
    skip_token.


Erlang code.

type_of(Word) ->
    case is_reserved(Word) of
        true -> reserved;
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
