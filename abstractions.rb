module Abstractions
  def self.add_three_failures(h)
    a = h["a"]
    b = h["b"]
    c = h["c"]

    if a && b && c
      a + b + c
    else
      nil
    end
  end

  def self.deps(h)
    (ka, a)  = h["a"]
    if ka && a
      (kb, b)  = h[ka]
      if kb && b
        (_kc, c) = h[kb]
        if c
          a + b + c
        else
          nil
        end
      else
        nil
      end
    else
      nil
    end
  end

  def self.better_deps(h)
    (1..3).inject(["a", 0]) { |(k, n)|
      (next_key, next_n) = h[k]
      if (next_key.nil? || next_n.nil?)
        nil
      else
        [next_key, n + next_n]
      end
    }
  end

  def self.test_deps
    hdeps = {"a" => ["b", 1], "b" => ["c", 3], "c" => ["d", 5]}
    hdeps_boom = {"a" => ["b", 1], "b" => ["c", "oops"], "c" => ["d", 5]}

    actual = deps(hdeps)
    puts "deps gives #{actual}, should be 9"

    better = better_deps(hdeps)
    puts "better_deps gives #{better}, should be 9"

    # actual_boom = deps(hdeps_boom)
    # puts "whatever we splode"
    better_boom = deps(hdeps_boom)
  end

  def self.whoozits(n)
    n.odd? ? n * n + 1 : nil
  end

  def self.evenToS(n)
    n.even? ? n.to_s : nil
  end

  def self.to_i(s)
    if s == 0
      "0"
    else
      n = s.to_i
      n == 0 ? nil : n
    end
  end

  def self.kleisli(s)
    fss = [:evenToS, :whoozits, :to_i]
    fs = fss.reverse.map { |s| method(s) }
    fs.inject(s) { |a, f|
      a.nil? ? nil : f.call(a)
    }
  end
end
