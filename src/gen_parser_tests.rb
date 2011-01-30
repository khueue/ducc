#!/usr/bin/env ruby -w
# encoding: UTF-8

# Run from project root: [ruby] src/gen_parser_tests.rb suite/**/*.c

if ARGV.empty?
  puts "Usage: ruby #{__FILE__} glob_pattern [glob_pattern ...]"
  exit
end

program = 'parser'
pipes = ['lexer',program].join(' | ')

files = []

ARGV.each do |pattern|
  files |= Dir[pattern]
end

files.each do |file|
  file_out = "#{file}.#{program}"
  cmd = "cat #{file} | #{pipes} > #{file_out}"
  puts cmd
  system cmd
end
