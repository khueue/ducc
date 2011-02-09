#!/usr/bin/env ruby -w
# encoding: UTF-8

# Run from project root: [ruby] src/gen_tests.rb program suite/**/*.c

def usage()
    puts "usage: ruby #{__FILE__} program file"
    exit 0
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

files.each do |file|
    file_out = "#{file}.#{program}"
    cmd = "#{command} #{file} > #{file_out}"
    puts cmd
    system cmd
end