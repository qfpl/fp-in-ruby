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

Purity / referential transparency
 
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
- instance variable and printing - can't replace call
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
- This is very roughly analogous to what Haskell does
- We have a pure function that returns a computation (`String`)
- Computation is impure when evaluated - but the value representing the computation is pure
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
- Returns a value representing an impure computation
</div>

## Types

<div class="notes"
- Not required by definition, but overlaps with notion of sets
- Sets classify values and make our functions more precise - like types
- Think they're a very important tool in producing robust software
</div>

##

Why are types desirable?

##

- Allow you to more precisely represent the intentions of the programmer
- Catch many errors before runtime
- Eliminate need for many tests
- Support you in writing complex code

<div class="notes">
- Communication tool
- Easier to reason about - closed sums
- Gives us extra cycles to focus on our problem
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

```ruby
def describe_vehicle(v)
  case v
  when :car
    "Closed in people container on 3 or more wheels"
  when :bicycle
    "Person powers two wheels with pedals"
  when :motorbike
    "Two wheels powered by a motor"
  else
    raise "I don't know how to describe a '#{v}'"
  end
end
```

<div class="notes">
`v` could be anything. We're not even sure it's a symbol, let alone a symbol
from the set of vehicles.

Runtime errors can mean someone gets woken up at night.
</div>

##

Types aren't bad - type _systems_ are _sometimes_ bad.

<div class="notes">
- Types not expressive enough
- Compiler/tool support not good
- Very common, statically typed languages poisoned the well
- I felt this way too after using conventional, typed, OO languages
</div>

## Functions are values

##

- functions are _first class_ - can be treated like any other value
- functions that take/return functions are _higher order_

## Abstraction

<div class="notes">
- Finally - mathematics and software both about abstraction.
- Recognize patterns and factor them out.
</div>

