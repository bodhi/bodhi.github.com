---
title: Turning ruby block callbacks into lego
---

[Matt Sears](http://www.mattsears.com) wrote a [great article about
creating callbacks with Ruby
blocks](http://www.mattsears.com/articles/2011/11/27/ruby-blocks-as-dynamic-callbacks)

The crux of his method was the building of an anonymous class that
would respond correctly to his callbacks

``` ruby
class Proc
  def callback(callable, *args)
    self === Class.new do
      method_name = callable.to_sym
      define_method(method_name) { |&block| block.nil? ? true : block.call(*args) }
      define_method("#{method_name}?") { true }
      def method_missing(method_name, *args, &block) false; end
    end.new
  end
end
```

For some reason this post captured my imagination and I started to
deconstruct his method, as I couldn't quite grasp why it worked. What
follows is a deconstruction of his method.

His example involved a small handler for "posting" twitter messages:

``` ruby
def tweet(message, &block)
  Twitter.update(message)
  block.callback :success
rescue => e
  block.callback :failure, e.message
end

tweet "Ruby methods with multiple blocks. #lolruby" do |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end
```

I'm going to work backwards, substituting and expanding this code
until we reach a good position to see what's really happening. We'll
ignore Matt's "bonus" style for focus. Firstly, just expand the call
to `#tweet`, and define the block as a local lambda:

``` ruby
block = lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end

begin
  Twitter.update(message)
  block.callback :success
rescue => e
  block.callback :failure, e.message
end
```

For now, let's ignore the `rescue` clause, in fact we can trim
everything except for one of the `block.callback` calls. So, going
with `:success`:


``` ruby
block = lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end

block.callback :success
```

Once we get to here, we need to go back to our original definition of `Proc#callback`

``` ruby
class Proc
  def callback(callable, *args)
    self === Class.new do
      method_name = callable.to_sym
      define_method(method_name) { |&block| block.nil? ? true : block.call(*args) }
      def method_missing(method_name, *args, &block) false; end
    end.new
  end
end

block = lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end

block.callback :success
```

Expanding out `Proc#callback`, and realising that in this context,
`self` is `block`, and `callable` is `:success`, and `args` is `[]`
(In the case of the `:failure`, `args` would be the parameters passed
to the callback),

``` ruby
block = lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end

block === Class.new do
  method_name = :success
  define_method(method_name) { |&block| block.nil? ? true : block.call }
  def method_missing(method_name, *args, &block) false; end
end.new
```

Simplifying the definition of block as a lambda and the `Proc#===` call, our code becomes

``` ruby
lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end.call(Class.new do
  method_name = :success
  define_method(method_name) { |&block| block.nil? ? true : block.call }
  def method_missing(method_name, *args, &block) false; end
end.new)
```

Time to simplify the anonymous class definition, let's get rid of the
error handling in `#success` too.

``` ruby
lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end.call(Class.new do
  def success &block
    block.call
  end
  def method_missing(method_name, *args, &block) false; end
end.new)
```

Hey, now we are getting somewhere! Let's give the anonymous class a name.


``` ruby
class SuccessCallback
  def success &block
    block.call
  end
  def method_missing(method_name, *args, &block) 
    false
  end
end

lambda |on|
  on.success do
    puts "Tweet successful!"
  end
  on.failure do |status|
    puts "Error: #{status}"
  end
end.call(SuccessCallback.new)
```

So what has happened? We've created a `SuccessCallback` class that
*only* responds to `#success`. And all `#success` does is call a block
passed to it. Any other method call will absorb any parameters and
*any block passed*. This is the heart of Matt's system, as we can see
in the next expansion, where we unwrap the callback lambda.


``` ruby
class SuccessCallback
  def success &block
    block.call
  end
  def method_missing(method_name, *args, &block) 
    false
  end
end

on = SuccessCallback.new

on.success do
  puts "Tweet successful!"
end
on.failure do |status|
  puts "Error: #{status}"
end
```

And it becomes clear from looking at the definition of
`SuccessCallback` why the callback works. `on.success` just
trampolines back to the block passed, and `on.failure` falls through
to `SuccessCallback#method_missing` and thus just returns false and
ignores the passed block.

In the `:failure` case, we would have ended up with:

``` ruby
class FailureCallback
  def failure &block
    block.call(message) # message is the value of `e.message` in the original code
  end
  def method_missing(method_name, *args, &block) 
    false
  end
end

on = FailureCallback.new

on.success do
  puts "Tweet successful!"
end
on.failure do |status|
  puts "Error: #{status}"
end
```

So, in essence, Matt's method works by:

1. Defining an anonymous class that responds to a single method,
`#success` in our example, that calls straight back to a block passed.

2. Yielding an instance of this anonymous class to the callback block.

3. The callback block calling *every* event method on the anonymous
instance. However, since the anonymous class ignores all messages bar
the event that was "triggered", actually evaluating only the block
matching the trigger.

Writing out these steps, I feel a bit sheepish in taking so long to
grasp how exactly Matt's code worked. But what we can do is extract
out the concept embodied by the anonymous class, and reformulate the
code.

``` ruby
class SavantTrampoline
  def initialize method_name, *args
    @method_name = method_name.to_sym
    @args = args
  end
  def method_missing method, &block
    if method.to_sym == @method_name
      yield *@args if block_given?
    else      
      false
    end
  end
end

class Proc
  def callback(callable, *args)
    self.call SavantTrampoline.new(callable, *args)
  end
end
```

As a pedagogical exercise, this formulation allows me to more clearly
understand the roles of each actor in this code.