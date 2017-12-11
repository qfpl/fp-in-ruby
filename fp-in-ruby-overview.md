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
   
### Referential transparency

 - Can replace function call with return value anywhere you see it
 - code example of RT and not RT
 - Easier to reason about
 - Much less risky to change code
 
### Immutability

 - Variables don't vary
 - Yes, really. Not as hard as you might think.
 - code example 
 
### No side effects

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
   
## What can we do in Ruby

### RT

 - Effects at the boundaries
   + 
