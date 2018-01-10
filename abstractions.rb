module Abstractions

  HDEPS = {"a" => ["b", 1], "b" => ["c", 3], "c" => ["d", 5]}
  HDEPS_BOOM = {"a" => ["b", 1], "b" => ["c", "oops"], "c" => ["d", 5]}

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

  def self.abstract_deps(pure, mappend, mempty, f, s)
    (1..3).inject(pure.call([s, mempty])) { |m|
      m.bind { |(k, n)|
        f.call(k).bind { |(next_key, next_n)|
          pure.call([next_key, mappend.call(n, next_n)])
        }
      }
    }
  end

  class Maybe
    def initialize(a)
      @a = a
    end

    def bind
      if @a.nil?
        nil
      else
        # Let's pray the block returns another maybe
        yield @a
      end
    end
  end

  def self.test_abstract_deps
    pyooah = Maybe.method(:new)
    abstract_deps(pyooah,
                  ->(a,b){a + b},
                  0,
                  ->(k){pyooah.call(HDEPS[k])},
                  "a"
                 )
  end

  def self.test_deps
    actual = deps(HDEPS)
    puts "deps gives #{actual}, should be 9"

    better = better_deps(HDEPS)
    puts "better_deps gives #{better}, should be 9"

    # actual_boom = deps(HDEPS_BOOM)
    # puts "whatever we splode"
    better_boom = deps(HDEPS_BOOM)
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
