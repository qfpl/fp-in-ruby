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

##

 - Referential transparency
 - Types

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
  puts "Adding #{a} and #{b}"
  a + b
end

def lyf
  2 * add(18, 3)
end
```

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

  # Create a charge object that we can use to charge the card later
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

## Why is referential transparency desirable?

##
