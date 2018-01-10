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

  def self.test_deps
    hdeps = {"a" => ["b", 1], "b" => ["c", 3], "c" => ["d", 5]}
    actual = deps(hdeps)
    puts "deps gives #{actual}, should be 9"

    hdeps_boom = {"a" => ["b", 1], "b" => ["c", "oops"], "c" => ["d", 5]}
    actual_boom = deps(hdeps_boom)
    puts "whatever we splode"
  end
end
