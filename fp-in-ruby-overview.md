# FP in Ruby

## What is FP

### FP is doing it with functions

 - mathematical functions
   + mapping of values from one set to another
   + only one output for any input
 - properties from mathematical functions
   + referential transparency
     - no mutation
     - no side effects
   + types
 - emphasis on abstraction
   
### Referential transparency

 - Can replace function call with return value anywhere you see it
 - code example of RT and not RT
 - Easier to reason about
 - Much less risky to change code
 
### Immutability

 - consequence of RT
 - Variables don't vary
 - Yes, really. Not as hard as you might think.
 - code example 
 
### No side effects

 - consequence of RT
 - You're crazy - how do you read/write files or talk to a database
 - We can separate computations that talk to the outside world from their execution
 - e.g. A ruby program that generates a ruby program for talking to files
   + we can always generate the same ruby script given the same inputs and uphold RT
   + ruby interpreter executing that code is doing the IO for us
   + IO monad in Haskell
 
### Types

 - Types categorise values
 - Mathematical definition of a function requires input and output sets
   + domain and codomain are the mathematical terms
 - Set membership classifies values
 - Benefits
   + We know which operations are valid and which aren't
   + Can statically check that everything aligns
   
### Higher order functions
   
### Abstractions

 - DRY
 - pattern recognition
 - common language for these ideas
 - abstract algebra to the rescue
 - we're not trying to be obtuse, academic, or exclusionary
   
## What can we do in Ruby?

### Immutability

 - Avoid methods with `!` suffixes
 - Pass things around explicitly rather than using instance variables
 - Use `self` methods to help enforce immutability
 - **WARNING**: Ruby doesn't assume immutability, so this can be slow
   for big, nested structures. In those cases, using mutation may be preferable.
   Ideally put that mutation behind an immutable interface.
 - Depends on discipline.
 
### Side effects

 - Idea of the functional core, imperative shell
 - Deal with the messy world at the edges (input and output) and
   view the rest of your program as a series of pure transformations on data
 - Can develop types/DSL to capture commands that you want to execute and then
   run them at the end/edges.
 - Errors are values - no exceptions

### Abstraction

 - Iteration
   + `for` loops are boilerplate - don't seem to be too common
   + `each` is better, however...
     - The type of `each` implies mutation and/or side effects
   + `map`, `inject` is better
   + Chain things
 - Type classes with duck typing
 - Stop using `nil` - can embed idea of missing values into your structs
 - Higher order functions using Proc/lambda
   
### Types

 - Use `Struct` to give you closed data types with known fields
 - These "types" give you the values you can pass around to form your domain
   specific language for which you write an interpreter
   
## What else is there?

 - Immutable by default languages
   + alleviate performance problems
   + no discipline required
 - Going all the way with static types and algebra
