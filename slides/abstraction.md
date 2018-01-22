# Abstraction

<div class="notes">
- Full benefits of FP require aggressive abstraction
- Taken from mathematics
- Want to demonstrate what Ruby can do and what it can't
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
-- List of keys can be arbitrarily long.
-- If any aren't present, we get `Nothing`.
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
-- => AccFailure [ "Couldn't find key: foo",
--               , "Couldn't find key: baz"
--               ]
```

<div class="notes">
Our map lookup is transformed into a type that captures
errors and aggregates them.
</div>

##

```haskell
addMaybes h keys =
  let
    f = (`M.lookup` h)
  in
    fmap sum . traverse f $ keys

addValidation h keys =
  let
    f k = maybe (AccFailure $ ["Couldn't find key: " <> k])
                AccSuccess
                (M.lookup k h)
  in
    fmap sum . traverse f $ keys
```

##

```haskell
                  
     
                      
    
    fmap sum . traverse f $ keys

                      
     
                                                           
                          
                              
    
    fmap sum . traverse f $ keys
```

##

```haskell
addThings f xs =
  fmap sum . traverse f $ xs
```

<div class="notes">
- Factored repetition and separated concerns
- Function about adding things in some computational context
   + missing data, failure with record, computations that require an input
</div>

##

```haskell
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
```
 
##
 
```haskell
addMultiplesOf n ns =
  addThings (*) ns n
  
-- > addMultiplesOf 5 [1,2,3]
-- 30
```

##

```ruby
def self.add_things(fmap, pure, lifta2, f, keys)
  fas = keys.inject(pure.call([])) { |fas, k|
    fa = f.call(k)
    lifta2.call(fa, fas) { |a, as| as << a }
  }

  fmap.call(fas) { |as| as.inject(0, &:+) }
end
```

<div class="notes">
- Have to pass in a bunch of extra functions that are defined for us in Haskell
- Could put these in a library
- Code was hard to write - got it wrong a few times and not confident there aren't corner cases
</div>

##

```ruby
def self.add_things_nil(h, keys)
  pure = Optional.method(:new)
  fmap = ->(o, &f){
    o.and_then { |x| pure.call(f.call(x)) }}
  lifta2 = ->(fa, fb, &g){
    fa.and_then { |a|
      fb.and_then { |b|
        pure.call(g.call(a, b))}}}
  f = ->(k){pure.call(h[k])}
  add_things(fmap, pure, lifta2, f, keys)
end
```

<div class="notes">
Have to do a lot of work to call it - no lang/lib support

Again, overly complicated and error prone without tools
</div>
##

- No types or compiler to guide me or tell me when I'm wrong
- No existing abstractions to build on
- Struggling against the language and all other Ruby libs

