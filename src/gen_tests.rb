#!/usr/bin/env ruby -w
# encoding: UTF-8

def usage()
    puts "usage: ruby #{__FILE__} -l|-p|-a|-t file [file, ...]"
    exit 0
end

def run_test(command, file, program)
    file_out = "#{file}.#{program}"
    cmd = "#{command} #{file} > #{file_out}"
    puts cmd
    system cmd
end

def collect_output(file, command, program, output_file)
    echo_newlines = "echo \"\n\n\""
    echo_headline = "echo \"### #{command} #{file} gives:\n\""

    cmd = "#{echo_headline} >> #{output_file}"
    system cmd

    insert_tab_prefix = "sed -e 's/^/\t/'"

    cmd = "cat #{file}.#{program} | #{insert_tab_prefix} >> #{output_file}"
    system cmd

    cmd = "#{echo_newlines} >> #{output_file}"
    system cmd
end

if ARGV.length < 2
    usage()
end

command = case ARGV[0]
    when "-l"
        program = "lexer"
        "ducc -l"
    when "-p"
        program = "parser"
        "ducc -p"
    when "-a"
        program = "analyzer"
        "ducc -a"
    when "-t"
        program = "translator"
        "ducc -t"
    else
        usage()
end

files = []

for i in 1...ARGV.length
    files |= Dir[ARGV[i]]
end

output_file = "tests_output.md"
system "rm -rf #{output_file}"

echo_title = "echo \"# Testruns\n\n\""
cmd = "#{echo_title} >> #{output_file}"
system cmd

files.each do |file|
    run_test(command, file, program)
    collect_output(file, command, program, output_file)
end
