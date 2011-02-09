-module(rtl_constructors).
-compile(export_all).

make_icon(Int, TempCount) ->
    create_instr(TempCount, [icon(Int)]).

create_instr(TempCount, Instructions) ->
    {TempCount+1, Instructions}.

char_to_int(Value) ->
    case atom_to_list(Value) of
        [$\\,$n] -> 10;
        [Int]    -> Int
    end.

icon(Int) ->
    {icon, Int}.
