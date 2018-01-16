# What is FP?

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
 
<div class="notes"
- Consequences of mathematical function
</div>

## Purity

- No _observable_ side effects
- No free variables

<div class="notes">
- Free variables are variables that are not local to the function
   + Not a paramater
   + Not created within scope of the function
</div>

##

```ruby
# impure
def foo(a)
  b = a ** 2
  (a + b) * @factor
end

# pure
def bar(a, factor)
  b = a ** 2
  (a + b) * factor
end
```

<div class="notes"
`foo` is impure because it uses a free variable
</div>

## Referential transparency

> Replacing any call to a function with the function's return value results in a program with
> identical behavior.

<div class="notes"
- Related to purity
- Greatly enhances ability to reason about code
- Greatly reduces risk of change - e.g. extracting function
</div>

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

<div class="notes"
- Replaced call to `add` in `lyf` with result of call.
- This is safe because `add` is referentially transparent
</div>

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
  total_cost = items.inject(0) do |s, i|
    s + i.quantity * i.unit_price
  end

  # Actually charges the card
  charge(credit_card, total_cost)
  Order.new(items, total_cost)
end
```

<div class="notes">
- `order` is not referentially transparent
   + calls `charge`, which we're assuming charges the card
   + definitely can't replace calls to `order` with the returned `Order`
- I can't get an order without charging a card
   + Creation of an order is tied to effect of charging the card
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

<div class="notes">
- This _is_ referentially transparent
- Idea that you can turn your effects into data and run them explicitly
- We've also decoupled creation of data and running of effects
   + Simpler design and easier to test
</div>

##

Why is referential transparency desirable?

##

- Easy to reason about code
- Easy to test
- Fearless code changes

<div class="notes">
- Each function/method is entirely self contained
    + local reasoning
    + Never have to worry about state of the system
    + No pretending to be an interpreter/compiler
- Testing is simply passing arguments to functions and comparing the result
</div>

##

What am I giving up with referential transparency?

- Mutation
- Free variables
- Familiarity
- Byzantine system states

<div class="notes">
- Mutation and free variables are usually a bad idea anyway
- Familiarity is just time and practice - remember when you didn't know how to for loop?
</div>

##

What am I not giving up?

- State
- IO
- Anything you need to write great software

<div class="notes">
- Still do things like
   + Write to the DB
   + Print to the console
   + Read a file
   + Generate random numbers
- People write software this way to do everything you do in Ruby
- Web apps, transport models, vector tile server, games
</div>

## {data-background-image="images/one-weird-trick.jpg" data-background-size="contain"}

<div class="notes"
At this point might think I'm crazy for claiming you can maintain
RT and do more than heat your CPU
</div>

##

Our programs don't execute side effects, they produce computations as pure
values that are executed by a runtime system.

<div class="notes">
- Monadic state and IO
</div>

##

```ruby
def main(args)
  <<-MAIN
  num_echoes = #{args[0]}
  loop do
    s = $stdin.gets
    break if s.chomp == 'q'
    puts(s * num_echoes)
  end
  MAIN
end

eval(main(ARGV))
```

<div class="notes">
- Echo program
   + Reads number from command line
   + Echos input lines the specified number of time
- This is very roughly analogous to what Haskell does
- We have a pure function that returns a computation (`String`)
- Computation is evaluated separately (`eval` in this case)
- Please DO NOT do this - just illustrating the point
</div>

##

```haskell
main = do
  args <- getArgs
  let
    e s =
      error $ "Unable to parse '" ++ s ++ "' as Int"
    go' =
      case args of
        (s:_) -> either (const (e s)) go $ readEither s
        _     -> error "Expected at least one arg"
    go _ "q" = pure ()
    go n s   = do
      (() <$) . replicateM n . putStrLn $ s
      getLine >>= go n
  getLine >>= go'
```

<div class="notes">
- This code is pure and referentially transparent
- Returns a computation (value!) that is able to instruct a runtime system
  on what to do
</div>

##

What are the downsides of referential transparency?

##

- Some algorithms may require mutation to achieve highest performance
- Hard to achieve in languages where it's not the default

## Types

<div class="notes"
- Not required by definition, but overlaps with notion of sets
- I find them incredibly useful and so include them when I think
  about FP
</div>

##

Why are types desirable?

<div class="notes"
Not only required by mathematical definition - have benefits
</div>

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

```haskell
data Vehicle = Car
             | Bicycle
             | MotorBike
             
describeVehicle :: Vehicle -> String
describeVehicle Car =
  "Closed in people container on 3 or more wheels"
describeVehicle Bicycle =
  "Person powers two wheels with pedals"
describeVehicle MotorBike =
  "Two wheels powered by a motor"
```

<div class="notes"
Defined a sum type and pattern matched it in a function

 - Sum type is closed
 - Compiler can tell us if we've forgotten to handle a case
 - Compiler will fail if we include a case not in the type
</div>

##

Why are types _not_ desirable?

##

- Types not expressive enough
- Tool support is lacking
- Cumbersome to use
- Bad error messages

<div class="notes">
- In short - when the type system isn't good enough
- More conventional languages with static types poisoned the well a bit
- I felt this way too after using conventional, typed, OO languages
- Much better tools exist
- Encourage you to try these if you have this opinion
</div>

## Functions are values

##

Functions, just like strings and numbers, are values that can be passed to other functions as arguments or returned by functions.

<div class="notes">
- functions that take/return functions are _higher order_
- lambdas/procs/blocks in Ruby
</div>

## Abstraction

<div class="notes">
- Finally - mathematics and software both about abstraction.
- Recognize patterns and factor them out.
- Common language for concepts
- _not_ about being obtuse and academic
- Steal from mathematics research
</div>

