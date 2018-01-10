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

##

```haskell
liftA3 (\a b c -> a + b + c)
       (lookup h "foo")
       (lookup h "bar")
       (lookup h "baz")
```

##

```haskell
fmap (foldr (+) 0) . traverse (\k -> M.lookup k m) $ ["foo", "bar", ...]
```

