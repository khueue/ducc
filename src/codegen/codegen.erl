-module(codegen).
-export([generate_code/2]).

% -define(HELPER, codegen_helpers).
-define(ENV, codegen_env).

generate_code(RtlCode, Lines) ->
    Env = ?ENV:new(Lines),
    AsmCode = translate_toplevels(RtlCode, Env),
    AsmCode.

translate_toplevels([], _Env) -> [];
translate_toplevels([Toplevel|Toplevels], Env) ->
    ToplevelAsm = translate_toplevel(Toplevel, Env),
    [ToplevelAsm|translate_toplevels(Toplevels, Env)].

translate_toplevel(Toplevel, Env) ->
    Type = erlang:element(1, Toplevel),
    case Type of
        data -> translate_data(Toplevel, Env);
        proc -> translate_proc(Toplevel, Env)
    end.

translate_data({data, {label,Name}, Bytes}, _Env) ->
    data.

translate_proc({proc, {label,Name}, Formals, Temps, FS, Ins}, _Env) ->
    proc.
