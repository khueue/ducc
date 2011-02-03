#!/usr/bin/env ruby -w
# encoding: UTF-8

# Run from project root: [ruby] src/nice_tests.rb suite/incorrect/semantic/*.c

if ARGV.empty?
  puts "Usage: ruby #{__FILE__} glob_pattern [glob_pattern ...]"
  exit
end

program = 'analyzer'
pipes = ['lexer','parser',program].join(' | ')

files = []

ARGV.each do |pattern|
  files |= Dir[pattern]
end

system "rm tests.txt"

files.each do |file|
  echo = "echo \"\n\n\n\""
  cmd = "cat -n #{file} >> tests.txt"
  puts cmd
  system cmd
  cmd = "cat -n #{file}.analyzer >> tests.txt"
  puts cmd
  system cmd
  cmd = "#{echo} >> tests.txt"
  system cmd
end
