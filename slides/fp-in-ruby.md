# FP in Ruby

<div class="notes">
- Now that I've explained some concepts and motivation - what can we achieve in Ruby
</div>

## Referential transparency

- Immutable data types
- No side effects

##

Avoid `each` and `for` loops - they suggest side effects

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

```
nums.inject({}) do |h, n|
  h[n] = n ** n
  h
end
```

<div class="notes">
- `map` is preferable, but sometimes we want to change the structure being accumulated
- `inject` allows us to do this as it's more general
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

## Immutability

##

Avoid methods with `!` suffixe
 
```

```


