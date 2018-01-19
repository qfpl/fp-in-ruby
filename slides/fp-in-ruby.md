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

## Purity / referential transparency

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
- Tradeoff is coming up with more names for things
</div>

##

Avoid `each` and `for` loops. Instead use functions like:

 - `select`
 - `map`
 - `inject`
 - `zip`

<div class="notes">
- `each` and `for` suggest side effects - no return values
- Each of these are higher order functions in the sense that they expect a block
- Check the `Array` and `Enumerator` interfaces for more
</div>

##

### `select`

```ruby
nums = []
(1..10).each { |n| nums << n if n.even? }

# Better
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
nums = [1,2,3]
squares = []
nums.each { |n| squares << n ** 2 }

# Better
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

<div class="notes">
- `inject` is `fold` or `reduce` in other langs
- often about summarising/collapsing/aggregating a collection
</div>

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

##

```ruby
odds = [1,3,5]
evens = [2,4,6]

foo = odds.zip(evens) { |(a,b)| a * b }
```

<div class="notes">
`zip` can also take a block to produce a value from each array of zipped together values
</div>

## {data-background-image="images/its-a-trap.jpg" data-background-size="contain"}

## `foo == nil`!!!

```ruby
odds = [1,3,5]
evens = [2,4,6]

foo = odds.zip(evens) { |(a,b)| a * b }
```

##

```ruby
odds = [1,3,5]
evens = [2,4,6]

foo = []
odds.zip(evens) { |(a,b)| foo << a * b }
# foo == [2,12,30]
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

<div class="notes">
`self` methods help to enforce referential transparency
</div>

## `Struct`

```ruby
Foo = Struct.new(:bar, :baz, :whoozitz)

foo = Foo.new(1,2,3)

# => true
foo.bar + foo.baz == foo.whoozitz

# => Error!
foo.quux.nil?
```

<div class="notes">
- `Struct` is good for data types with known fields
- `Hash` is problematic because it is open - `Struct` is closed
</div>

##

```ruby
Foo = Struct.new(:bar, :baz) do
  def with_bar(bar)
    self.new(bar, self.baz)
  end
  
  def total
    self.bar + self.baz
  end
end
```

<div class="notes">
- `Struct` can also contain methods
- Not that close to FP, but closer to idiomatic Ruby
- Can use methods to provide convenient, immutable operations
</div>

## Modules

```ruby
module TicTacToe
  Game = Struct.new(:board, :next_symbol, :winner)
  
  def self.finished?(game)
    !game.winner.nil? || board_full?(game.board)
  end

  def self.with_winner(game, winner)
    self.new(game.board, game.next_symbol, winner)
  end
end
```

<div class="notes">
- Modules cannot be instantiated
- Encapsulate data and functions, but no mutable state or inheritance
- Set fields by returning a new instance of the data type
</div>

## Types

<div class="notes">
- Ruby is unityped
- No compiler support
- Can't do a lot
- Can at least make things clearer
</div>


## {data-background-image="images/functional-core.png" data-background-size="contain"}

<div class="notes">
In addition to the smaller scale stuff, there's a big picture idea
that was popularised about 5 years ago.

May also hear IO or effects at the edges - very common in Haskell/FP world

Talk given by Gary Bernhardt
</div>

##

- Any effects/IO at boundaries of application
- Bring things in to application's data structures
- Validate aggressively before core of app uses data
- Write pure transformations on those data structures
- Turn data structures into effects as late as possible

