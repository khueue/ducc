-module(translator_driver).
-export([translate/3]).

translate(Stream, ParseTree, Lines) ->
    RtlCode = translator:translate(ParseTree, Lines),
    {ok, RtlCode}.
