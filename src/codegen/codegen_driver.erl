-module(codegen_driver).
-export([generate_code/3]).

generate_code(_Stream, RtlCode, Lines) ->
    AsmCode = codegen:generate_code(RtlCode, Lines),
    {ok, AsmCode}.
