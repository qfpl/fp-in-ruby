# FP in Ruby

<div class="notes">
- Now that I've explained some concepts and motivation - what can we achieve in Ruby
</div>

## Higher order functions

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
- No side effects

##



##

Avoid `each` and `for` loops - they suggest side effects. Instead use functions like:

 - `map`
 - `inject`
 - `zip`
 - `select`

<div class="notes">
- Each of these are higher order functions in the sense that they expect a block
- Check the `Array` and `Enumerator` interfaces for more
</div>

##

### `select`

```ruby
nums = []
(1..10).each { |n| nums << n if n.even? }
```

##

```ruby
nums = (1..10).select { |n| n.even? }
```

##

### `map`

##

```ruby
nums = [1,2,3]
squares = []
nums.each { |n| squares << n ** 2 }
```

<div class="notes">
- Explicit mutation of `squares` - value change is observable
- `each` doesn't return values produced in block - suggests there aren't any or
  that you're building a result through mutation
</div>

##

```ruby
nums = [1,2,3]
squares = nums.map { |n| n ** 2 }
```

##

### `inject`

##

```ruby
array.inject(initial_accumulator) do |accumulator, element|
  ...
  new_accumulator
end
``

<div class="notes">
- Important to note that `inject` returns the last accumulator value returned from the block
- This is better known as `fold` or `reduce`
</div>

##

```ruby
nums = [1,2,3]
nums_to_squares = {}
nums.each { |n| nums_to_squares[n] = n ** 2 }
```

<div class="notes">
- Sometimes we want to build up a different type of structure and map won't do
</div>

##

```
nums.inject({}) do |h, n|
  h[n] = n ** 2
  h
end
```

<div class="notes">
- `map` is preferable, but sometimes we want to change the structure being accumulated
- `inject` allows us to do this as it's more general
</div>

