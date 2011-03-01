-module(codegen).
-export([generate_code/2]).

% -define(HELPER, codegen_helpers).
-define(ENV, codegen_env).

generate_code(RtlCode, Lines) ->
    Env = ?ENV:new(Lines),
    AsmCode = generate(RtlCode, Env),
    AsmCode.

generate(X, _Env) ->
    X.
