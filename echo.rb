#!/usr/bin/env ruby

def main(args)
  <<-MAIN
  num_echoes = #{args[0]}
  loop do
    s = $stdin.gets
    break if s.chomp == 'q'
    puts(s * num_echoes)
  end
  MAIN
end

eval(main(ARGV))

def map_block(a)
  b = []
  for x in a
    b << (yield x)
  end

  b
end

def map_block_arg(a, &f)
  b = []
  for x in a
    b << f.call(x)
  end

  b
end


def map_proc(a, f)
  b = []
  for x in a
    b << f.call(x)
  end

  b
end

def foo
  f = Proc.new { |n| n * 2 }
  map_block([1,2,3], &f)
end
