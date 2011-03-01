-module(emitter_driver).
-export([asm_to_string/3]).

asm_to_string(_Stream, AsmCode, _Lines) ->
    AsmString = emitter:asm_to_string(AsmCode),
    {ok, AsmString}.
