-module(analyzer_driver).
-export([analyze/2]).

analyze(_Stream, ParseTree) ->
    analyzer:analyze(ParseTree).
