#!/usr/bin/env ruby

def main(num_echoes)
  <<-MAIN
  loop do
    s = $stdin.gets
    break if s.chomp == 'quit'
    puts(s * #{num_echoes})
  end
  MAIN
end

eval(main(ARGV[0]))
