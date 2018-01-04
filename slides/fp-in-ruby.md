# FP in Ruby

<div class="notes">
- Now that I've explained some concepts and motivation - what can we achieve in Ruby
</div>

## Referential transparency

- Immutability
- No side effects

##

Avoid methods with `!` suffix.

```ruby
things = ['keyboard', 'mouse', 'display']

# Unneccessary mutation
things.map! { |s| s.upcase }

# Immutability
up_things = things.map { |s| s.upcase }
```

<div class="notes">
- Can always replace mutation with another variable
- Mutation is a problem because it hurts our ability to reason
   + Must interpret the code in your head to know what a variable contains at any point
   + With immutability, you only need to find where it's defined - can't be anything else after that
- Tradeoff is coming up with more names for things
</div>

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

### Pro tip!

```ruby
nums = (1..10).select(&:even?)
```

- `:` prefix calls `#to_proc`
- `#to_proc` on symbols returns a `Proc` that executes the named method
- `&` in this context calls `#to_proc` and passes the result as a block

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

```
Array#inject(INITIAL_VALUE) do
  |ACCUMULATED_VALUE, NEXT_ELEMENT|
  ...
  NEW_ACCUMULATOR
end
```

<div class="notes">
- Important to note that `inject` returns the last accumulator value returned from the block
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

##

```ruby
nums.inject([]) do |a, n|
  a << n
  a
end
```

<div class="notes">
- Now the accumulator is declared and modified within `inject`
</div>

##

```ruby
nums.inject([]) do |a, n|
  a2 = a.clone
  a2 << n ** 2
  a
end
```

<div class="notes">
- We could go all the way and copy our structure, but given the expression is already
  referentially transparent, there's no point incurring the performance hit
</div>

##

## Immutability

##

Avoid methods with `!` suffixe
 
```

```


