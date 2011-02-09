% {Scope,  Location,    Type,   Bytes, }
%
% {global, {label,321}, array,  }
% {local,  {temp,123},  scalar, }

{local,{temp,123}},    array, {Type, Count, Offset}
{local,{temp,123}},   scalar, {Type}
{global,{label,321}},  array, {Type, Count}
{global,{label,321}}, scalar, {Type}

first_label() ->
    new_label({label, 99}).

new_label({label, Prev}) ->
    {label, Prev + 1}.

rv() ->
    {temp, 0}.

fp() ->
    {temp, 1}.

first_temp() ->
    % Skip return and frame temps.
    new_temp({temp, 1}).

new_temp({temp, Prev}) ->
    {temp, Prev + 1}.
