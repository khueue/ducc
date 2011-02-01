require 'github/markup'

file = 'report/ass2/report_ass2.md'

puts GitHub::Markup.render(file)
