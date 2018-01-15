# FP in Ruby

<div class="notes">
- Now that I've explained some concepts and motivation - what can we achieve in Ruby
</div>

## Higher order functions

<div class="notes"
Starting here because later concepts depend on this
</div>

##

Higher order functions are essential to FP. Ruby doesn't quite support them, but we can approximate them with blocks/`Proc`.

##

```ruby
def map_block(a)
  b = []
  for x in a
    b << (yield x)
  end

  b
end

a = [1,2,3]
b = map(a) { |n| n * 3 }
# b == [3,6,9]
```

<div class="notes">
`yield` keyword calls block
</div>

##

```ruby
def map_block(a)
  b = []
  for x in a
    b << (yield x)
  end

  b
end

a = [1,2,3]
f = Proc.new { |n| n * 3 }
b = map(a, &f)
# b == [3,6,9]
```

<div class="notes">
If a method takes a block, can give it a `Proc` as its last arg and prefix with `&`
</div>

##

```ruby
def map_block_arg(a, &f)
  b = []
  for x in a
    b << f.call(x)
  end

  b
end

a = [1,2,3]
b = map(a) { |n| n * 3 }
# b == [3,6,9]
```

<div class="notes">
`&` turns anonymous block into a named `Proc`
</div>

##

```ruby
def map_proc(a, f)
  b = []
  for x in a
    b << f.call(x)
  end

  b
end

a = [1,2,3]
f = Proc.new { |n| n * 3 }
b = map_proc(a, f)
# b = [3,6,9]
```

##

`Symbol#to_proc` for instance methods

```ruby
# You wrote:
[1,2,3].map { |n| n.to_s }

# Why not:
[1,2,3].map(&:to_s)
```

- `&` prefix calls `#to_proc`
- `#to_proc` on symbols returns a `Proc` that executes the named _instance_ method
- `Proc` argument is taken as a block

## Referential transparency

- Immutability
- No free variables
- No side effects

##

Avoid methods with `!` suffix.

```ruby
# Unneccessary mutation
things = ['keyboard', 'mouse', 'display']
things.map! { |s| s.upcase }
# things == ['KEYBOARD', 'MOUSE', 'DISPLAY']

# Immutability
things2 = ['keyboard', 'mouse', 'display']
up_things2 = things.map { |s| s.upcase }
# things2 == ['keyboard', 'mouse', 'display']
# up_things2 == ['KEYBOARD', 'MOUSE', 'DISPLAY']
```

<div class="notes">
- Can always replace mutation with another variable
- Mutation is a problem because it hurts our ability to reason
   + Must interpret the code in your head to know what a variable contains at any point
   + With immutability, you only need to find where it's defined - can't be anything else after that
- Tradeoff is coming up with more names for things
</div>

##

Avoid `each` and `for` loops. Instead use functions like:

 - `select`
 - `map`
 - `inject`
 - `zip`

<div class="notes">
- `each` and `for` suggest side effects
- Each of these are higher order functions in the sense that they expect a block
- Check the `Array` and `Enumerator` interfaces for more
</div>

##

### `select`

```ruby
# Bad
nums = []
(1..10).each { |n| nums << n if n.even? }

# Good
nums = (1..10).select { |n| n.even? }

# Best
nums = (1..10).select(&:even?)

# => [2,4,6,8,10]
```

<div class="notes"
`filter` in other langs
</div>

##

### `map`

```ruby
# Bad
nums = [1,2,3]
squares = []
nums.each { |n| squares << n ** 2 }

# Good
squares = nums.map { |n| n ** 2 }

# => [1,4,9]
```

<div class="notes">
- Explicit mutation of `squares` - value change is observable
- `each` doesn't return values produced in block - suggests there aren't any or
  that you're building a result through mutation
</div>

##

### `inject`

```ruby
# Bad
nums = [1,2,3]
total = 0
nums.each { |n| total += n }

# Good
nums.inject(0) { |sum, n| sum + n }

# Best
nums.inject(0, &:+)
```

##

```ruby
[1,2,3].inject(0, &:+)
```

Ruby vs Haskell

```haskell
foldr (+) 0 [1,2,3]
```

##

```ruby
# this...
nums = [1,2,3]
nums_to_squares = {}
nums.each { |n| nums_to_squares[n] = n ** 2 }

# ...or this
nums_to_squares = nums.inject({}) do |h, n|
  h[n] = n ** 2
  h
end
```

<div class="notes">
- Sometimes we want to build up a different type of structure and map won't do
</div>

##

### `zip`

```ruby
odds = [1,3]
evens = [2,4]
pairs = []
(0..[odds.length, evens.length].min).each { |i|
  pairs << [odds[i], evens[i]]
}
# pairs == [[1,2], [3,4]]

zip_pairs = odds.zip(evens)
# zip_pairs == [[1,2], [3,4]]

```

## `self` methods

##

```ruby
class Foo
  def self.do_the_thing
    # can't access instance variables or methods
  end
end
```

## Modules

```ruby
module TicTacToe
  Game = Struct.new(:board, :next_symbol, :winner)
  
  def self.finished?(game)
    return true unless game.winner.nil?
    return true if board_full?(game.board)
    false
  end
end
```

<div class="notes">
- Modules cannot be instantiated
- Encapsulate data and functions, but no mutable state or inheritance
- Just data and functions
</div>

## Types

<div class="notes">
- Ruby is unityped
- No compiler support
- Can't do a lot
- Can at least make things clearer
</div>

### Simple data types

```ruby
Foo = Struct.new(:bar, :baz, :whoozitz)
```

<div class="notes">
- Sometimes you want organised data without operations
- Hash is obvious choice
- hashes are open - problematic
</div>

##

That's it for types

