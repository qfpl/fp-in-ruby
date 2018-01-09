# What is FP?

## {data-background-image="images/monadtshirt.jpg"}

<div class="notes"
- Lots of myths and misconceptions around FP
- Different definitions - will give you mine
</div>

##

It's programming with functions

<div class="notes">
- This isn't helpful without a clear definition of what I meant by function
</div>

## What's a function?

<div style="float: left; width: 30%">
  <img src="images/function.png" style="background-color: white" />
</div>

<div style="float: left; width: 10%">&nbsp;</div>

<div style="float: left; width: 60%; padding-top: 10%">
Mapping of inputs to outputs
</div>

<div class="notes">
- Only one output per input
- Directional
</div>

##

 - Purity / referential transparency
 - Types
 
## Purity

- No observable side effects
- No free variables

<div class="notes">
- Free variables are variables that are not local to the function
   + Not a paramater
   + Not created within scope of the function
</div>

##

```ruby
def foo(a)
  b = a ** 2
  (a + b) * @factor
end
```

## Referential transparency

> Replacing any call to a function with the function's return value results in a program with
> identical behavior.

##

```ruby
def add(a, b)
  a + b
end

def lyf
  2 * add(18, 3)
end
```

##

```ruby
def add(a, b)
  a + b
end

def lyf
  2 * 21
end
```

##

```ruby
def add(a, b)
  # HERE BE EFFECTS!
  puts "Adding #{a} and #{b} and #{@c}"
  a + b + @c
end

def lyf
  2 * add(18, 3)
end
```

<div class="notes">
- `add` is impure because it references an instance variable and prints
- `add` isn't referentially transparent because its return value may change without inputs changing
   + instance variable could change
- `add` has side effects - output not printed if replace call
</div>

##

```ruby
def order(items, credit_card)
  total_cost = 0
  for i in items
    total_cost += i.quantity * i.unit_price
  end

  # Actually charges the card
  charge(credit_card, total_cost)
  Order.new(items, total_cost)
end
```

<div class="notes">
- Let's look at a less trivial example
- Want to ensure you don't create an order without it getting paid for
- I can't get an order without charging a card - coupling
- More importantly, I can't replace the call to `order` with the returned `Order` and
  have my software work the same way
</div>

##

```ruby
def order(items, credit_card)
  total_cost = 0
  for i in items
    total_cost += i.quantity * i.unit_price
  end

  charge = Charge.new(credit_card, total_cost)
  Order.new(items, total_cost, charge)
end
```

<div class="notes">
- This _is_ referentially transparent
- Anywhere I see a call to `order`, I can replace it with the resulting `Order`
  and my program behaves exactly the same
- Idea that you can turn your effects into data and run them explicitly
- Furthermore, the code that runs effects can be tested more easily now - just
  fire data structures at the method
</div>

##

```ruby
def order(items, credit_card)
  total_cost = items.inject(0) do |s, i|
    s + i.quantity * i.unit_price
  end

  charge = Charge.new(credit_card, total_cost)
  Order.new(items, total_cost, charge)
end
```

##

Why is referential transparency desirable?

##

- Easy to reason about code
- Easy to test
- Fearless code changes

<div class="notes">
- Each function/method is entirely self contained
  + Never have to worry about state of the system
  + Who's pretended to be an interpreter and maintained state in their head to understand code?
- Testing doesn't require mocks/stubs. Pass arguments to a function and compare the results
  + At some point you want to test the whole system, but almost all code should be pure and rt
</div>

##

What am I giving up with referential transparency?

##

- No writes to the database
- No printing to the console
- No mutable state

<div class="notes">
- Some people might think I'm crazy
- People write software this way to do everything you do in Ruby
</div>

## {data-background-image="images/one-weird-trick.jpg" data-background-size="contain"}

##

Our programs don't execute side effects, they produce computations as pure
values that are executed by a runtime system.

##

```haskell
main :: IO ()
```

<div class="notes">
- `main` is the entrypoint to a Haskell program, similar to C
- It always has type `IO ()`
- This means it returns a computation (value!) that performs some IO (side effects) and yields no value
- The computation is a value that Haskell's runtime system can execute
</div>

##

```ruby
def main(num_echoes)
  <<-MAIN
  loop do
    s = $stdin.gets
    break if s.chomp == 'quit'
    puts(s * #{num_echoes})
  end
  MAIN
end

# Pretend runtime system
eval(main(ARGV[0]))
```

<div class="notes">
- This is very roughly analogous to what Haskell does
- We have a pure function that returns a computation (`String`), and the runtime system executes it
</div>

##

What are the downsides of referential transparency?

##

- Some algorithms may require mutation to achieve highest performance
- Hard to achieve in languages where it's not the default

## Types

##

<img src="images/function.png" width="60%" />

<div class="notes">
- Notice that our function is defined as being between two sets
- X is a set of coloured shapes, and Y is a set of colours
- types categorises values in the same way that set membership does
- Only defined for inputs in X - no need to worry about other inputs
</div>

##

Why are types desirable?

##

- Allow you to more precisely represent the intentions of the programmer
- Catches many errors before runtime
- Eliminates need for many tests
- Support you in writing complex code

<div class="notes">
- restricted types with few inhabitants make code easier to reason about
  + 3 possible inputs vs infinity
- achieving the same robustness requires a lot of boilerplate in unityped langs
- compiler error at function call site, vs testing a method handles type errors
- can make changes and then follow the compiler errors - no misses
</div>

##

Why are types _not_ desirable?

##

- When they're not expressive enough
- When tool support is lacking

<div class="notes">
- In theory there's a tradeoff, although personally I would always prefer a type system
</div>

##

"I've used language X and the types got in the way more than they helped"

<div class="notes">
- I felt this way too after using conventional, typed, OO languages
- Much better tools exist
   + Haskell
   + Idris
   + Agda
- Encourage you to try these if you have this opinion
</div>

##

 - Robust and widespread inference
 - Algebraic data types (sums and products)

<div class="notes">
- many Ruby expressions are valid Haskell programs thanks to inference
- Powerful and concise declarations
</div>

## Functions are values

<div class="notes">
- Another property of FP not implied by our definition
</div>

##

Functions are values, just like strings and numbers, that can be passed to other functions as arguments or returned by functions.

<div class="notes">
- lambdas/procs/blocks in Ruby
- functions that take/return functions are _higher order_
</div>

## Abstraction

<div class="notes">
- Finally - mathematics and software both about abstraction.
- Recognize patterns and factor them out.
- Common language for concepts
- _not_ about being obtuse and academic
</div>

