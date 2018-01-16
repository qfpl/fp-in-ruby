# Abstraction

<div class="notes">
Put this in its own section, because it is a big one
</div>

##

Mathematics has a rich language for abstractions that we can steal.

- `Monoid`
- `Functor`
- `Applicative`
- `Monad`

<div class="notes">
- These words might seem strange
- Don't use them to obfuscate or exclude
- Precise meanings - safer to not use analogies or overloaded terms
</div>

## Optional data

##

```haskell
addThreeMaybes h =
  liftA3 (\a b c -> a + b + c)
         (lookup h "foo")
         (lookup h "bar")
         (lookup h "baz")
```

<div class="notes">
Things to note:

- uses idea of an `Applicative`
- no mention of failure/nil/Nothing values
- only contains things unique to our problem:
   + `liftA3` to run the computation
   + lambda to specify what to do with each value
   + each of the values
- no type signature - inference is great
</div>

##

```haskell
-- List of keys can be arbitrarily long. If any aren't present, we get `Nothing`.
addThreeMaybes h keys =
  fmap sum . traverse (`M.lookup` h) $ keys
```

<div class="notes">
Should the need arise, we can handle an arbitrary list of keys like so
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
def self.add_three_failures_list(h, keys)
  keys.inject(0) { |a, k|
    n = h[k]
    (a.nil? || n.nil?) ? nil : a + n
  }
end
```

<div class="notes">
Closer to the Haskell in structure, but still explicitly handling nil.
</div>

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
    
-- "foo" and "baz" aren't keys in the map
addThreeValidations someMap ["foo", "bar", "baz"]
-- => AccFailure ["Couldn't find key: foo","Couldn't find key: baz"]
```

<div class="notes">
Our map lookup is transformed into a type that captures
errors and aggregates them.
</div>

##

```haskell
-- Maybe
addThreeMaybes :: 
addThreeMaybes =
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

##

```haskell
addThings f xs =
  fmap sum . traverse f $ xs
  
addMaybes h keys =
  addThings (`M.lookup` h) keys
  

addValidations h keys =
  let
    validatedLookup k =
      maybe (AccFailure $ ["Couldn't find key: " <> k])
            AccSuccess
            (M.lookup k h)
  in
    addThings validatedLookup keys
  
addMultiplesOf n ns =
  addThings (*) ns n
```

<div class="notes">
- Factored repetition and separated concerns
- Function about adding things in some computational context
   + missing data, failure with record, computations that require an input
</div>

##

At this point, trying to write `addThings` in Ruby hurt my brain.

##

- No types to guide me or tell me when I'm wrong
- No existing abstractions to build on

## Take away

- Ruby gives you some basics
   + Methods that don't use mutation
   + Higher order functions via blocks/`Proc`
- Ruby falls down with deeper abstractions
   + Lack of types/tool suppport
   + Fighting the design/intentions of the language
   
<div class="notes">
- Using what you can will make your code better
- You'll hit a limit fairly quickly
   + Ability to keep everything in your head starts to break down
   + This is where tools/types help
- This _does_ matter. Ability to abstract further means...
   + less repetition
   + less bugs
   + shared understanding and code
   + steal from mathematicians
</div>

