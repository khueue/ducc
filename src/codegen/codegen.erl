-module(codegen).
-export([generate_code/2]).

generate_code(RtlCode, _Lines) ->
    %% XXX
    AsmCode = RtlCode,
    AsmCode.
