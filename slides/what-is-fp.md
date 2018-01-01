# What is FP?

## {data-background-image="images/monadtshirt.jpg"}

<div class="notes"
- Lots of myths and misconceptions around FP
- Different definitions - will give you mine
</div>

##

It's programming with functions

<div class="notes">
- Right now everyone's thinking "great, another condescending Haskell programmer"
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
</div>

##

```ruby
def order(items, credit_card)
  total_cost = 0
  for i in items
    total_cost += item.quantity * item.unit_price
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
    total_cost += item.quantity * item.unit_price
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

Why is referential transparency desirable?

##

- Easy to reason about
- Easy to test
- Fearless code changes

<div class="notes">
- Each function/method is entirely self contained
  + Never have to worry about state of the system
  + Who's pretended to be an interpreter and maintained state in their head to understand code?
</div>

##

The major implication of referential transparency is that expressions cannot have any
observable side effects.

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
- This means it returns a computation that performs some IO and yields no value
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
- This function isn't defined for any inputs that aren't coloured shapes - that is, elements of X
- types categorises values in the same way that set membership does
</div>

##

Why are types desirable?

##

- Allow you to more precisely represent the intentions of the programmer
- Catches many errors before runtime
- Obviates need for many tests
- Support you in writing complex code

<div class="notes">
- restricted types with few inhabitants make code easier to reason about
  + if my input must be one of three things, it's much easier to reason about than one that takes infinity
- to achieve the same level of robustness in a unityped language you must write a lot of boiler-plate
  defense code to ensure inputs are valid
  + tests might help, but software changes, requirements change, and it's easy to miss things
</div>

##

Not your parents' type system!

##

 - Robust and widespread inference
 - Powerful and concise declarations
   + Algebraic data types (sums and products)

<div class="notes">
- It's likely you haven't seen the sort of type system I'm talking about
- Conventional languages - Java, C#, C++ - have comparitively simple and clunky type systems
</div>

##

Why are types _not_ desirable?

##

- When they're not expressive enough
- When tool support is lacking

<div class="notes">
- In theory there's a tradeoff, although personally I would always prefer a type system
</div>

## Functions are values

##

Functions are values, just like strings and numbers, that can be passed to other functions as arguments.

<div class="notes">
- lambdas/procs/blocks in Ruby
</div>

##

A function that takes a function as input, or returns a function, is a _higher-order_ function.

