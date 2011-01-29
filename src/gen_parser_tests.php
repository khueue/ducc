<?php

// Run from project root: php src/gen_parser_tests.php

// Todo: Traverse dirs instead.
$files = array
    (
        "suite/quiet/lexer/l04.c",
        "suite/quiet/lexer/l05.c",
        "suite/quiet/parser/p01.c",
        "suite/quiet/parser/p02.c",
        "suite/quiet/parser/p03.c",
        "suite/quiet/parser/p04.c",
        "suite/quiet/parser/p05.c",
        "suite/noisy/simple/sim01.c",
        "suite/noisy/simple/sim02.c",
        "suite/noisy/simple/sim03.c",
        "suite/noisy/simple/sim04.c",
        "suite/noisy/simple/sim05.c",
        "suite/noisy/simple/sim06.c",
        "suite/noisy/simple/sim07.c",
        "suite/noisy/simple/sim08.c",
        "suite/noisy/simple/sim09.c",
        "suite/noisy/simple/sim10.c",
        "suite/noisy/simple/sim11.c",
        "suite/noisy/medium/circle.c",
        "suite/noisy/medium/fac-b.c",
        "suite/noisy/medium/fac.c",
        "suite/noisy/medium/fib.c",
        "suite/noisy/advanced/8queens.c",
        "suite/noisy/advanced/bubble.c",
        "suite/noisy/advanced/eval.c",
        "suite/noisy/advanced/primes.c",
        "suite/noisy/advanced/quick.c",
        "suite/incorrect/parser/pe01.c",
        "suite/incorrect/parser/pe02.c",
        "suite/incorrect/parser/pe03.c",
        "suite/incorrect/parser/pe04.c",
        "suite/incorrect/parser/pe05.c",
        "suite/incorrect/parser/pe06.c",
        "suite/incorrect/parser/pe07.c",
        "suite/incorrect/parser/pe08.c",
        "suite/incorrect/parser/pe09.c",
        "suite/incorrect/parser/pe10.c",
        "suite/incorrect/parser/pe11.c",
        "suite/incorrect/parser/pe12.c",
        "suite/incorrect/parser/pe13.c",
        "suite/incorrect/parser/pe14.c",
    );

foreach ($files as $file)
{
    $file_output = $file . '.parser';
    $cmd = "cat $file | lexer | parser > $file_output";
    echo $cmd, "\n";
    shell_exec($cmd);
}
