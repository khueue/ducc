#!/usr/bin/env ruby -w
# encoding: UTF-8

# Usage: ruby gen_markup.rb file.md > file.html

# gem install github_markup
require 'github/markup'
puts GitHub::Markup.render(ARGV[0], File.read(ARGV[0]))
