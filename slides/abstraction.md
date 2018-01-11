# Abstraction

<div class="notes">
Put this in its own section, because it is a big one
</div>

##

Mathematics has a rich world of algebra that is hard to take advantage of without a good type system.

##

- `Monoid`
- `Functor`
- `Applicative`
- `Monad`

## Optional data

##

```haskell
addThreeMaybes :: Map String Int -> Int
addThreeMaybes h =
  liftA3 (\a b c -> a + b + c)
         (lookup h "foo")
         (lookup h "bar")
         (lookup h "baz")
```

<div class="notes">
Things to note:

- no mention of failure/nil/Nothing values
- only contains things unique to our problem:
   + `liftA3` to run the computation
   + lambda to specify what to do with each value
   + each of the values
- side note on Haskell - the type declaration isn't necessary here
</div>

##

```haskell
-- List of keys can be arbitrarily long. If any aren't present, we get `Nothing`.
addThreeMaybes :: Map String Int -> [String] -> Int
addThreeMaybes h keys =
  fmap sum . traverse (`M.lookup` h) $ keys
```

<div class="notes">
This is scarier, but should the need arise, we can extend the above to
an arbitrary number of inputs
</div>

##

```ruby
if h[:foo]
  if h[:bar]
    if h[:baz]
      h[:foo] + h[:bar] + h[:baz]
    else
      nil
    end
  else
    nil
  end
else
  nil
end
```

<div class="notes">
This is the obvious code you might see in places.
Gross repetition.
Can do better in Ruby.
</div>

##

```ruby
def self.add_three_failures(h)
  a = h["a"]
  b = h["b"]
  c = h["c"]

  unless a.nil? || b.nil? || c.nil?
    a + b + c
  else
    nil
  end
end
```

<div class="notes">
- Relies on hash lookups returning nil
- Explicit `nil` handling
</div>

##

```ruby
```

##

What if the computation to get each value changes?

<div class="notes">
In the examples so far, we're doing hash lookups that might fail by returning nil.
What if the computation returned either the value or an error message?
</div>

##

```haskell
addThreeValidations h keys =
  let
    f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
                AccSuccess
                (M.lookup k h)
  in
    fmap sum . traverse f $ keys
```

##

```haskell
-- Maybe
fmap sum . traverse (`M.lookup` h) $ keys

-- Validation
let
  f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
              AccSuccess
              (M.lookup k h)
in
  fmap sum . traverse f $ keys
```

##

```haskell
-- Maybe
let
  f = (`M.lookup` h)
in
  fmap sum . traverse f $ keys

-- Validation
let
  f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
              AccSuccess
              (M.lookup k h)
in
  fmap sum . traverse f $ keys
```

