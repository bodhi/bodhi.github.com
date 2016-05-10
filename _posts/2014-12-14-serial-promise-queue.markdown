---
title: A serial queue for asynchronous operations in Javascript
layout: default
---

Let's get straight to the point:

{% highlight javascript %}
Queue = function() {
  var queueEnd = Promise.resolve();

  this.enqueue = function(item) {
    var thunk;
    var promise = new Promise(function(resolve, reject) {
      thunk = function() {
        if (item.length > 0) {
          item(resolve, reject);
        } else {
          var result = item();
          if (result && result.then) {
            result.then(resolve);
          } else {
            resolve();
          }
        }
      };
    });

    queueEnd.then(thunk);
    queueEnd = promise;

    return promise;
  };
};
{% endhighlight %}

Of course, you could just use something like [Async.js's
`queue`](https://github.com/caolan/async#queue). But we suffer from
[NIH](http://en.wikipedia.org/wiki/Not_invented_here) here!

I couldn't find much documentation on the semantics of `Promise`, but
it seems that the promise's function parameter is executed *before*
the function returns. The thunk is necessary to work around this, to
avoid executing the queued action (`item`) immediately.

Also, there's no provision for handling failing actions. Not
production code, blah blah blah.

---

It has 3 interfaces:

1. Queue an action that takes 2 parameters (identical to the signature
   for the parameter to `Promise`). The action *must* eventually call
   one of `resolve` or `reject`, otherwise the queue will deadlock.

2. Queue an action that takes no parameters, and returns a promise (or
   rather, something that is *thenable*).

3. Queue an action that returns nothing, or a value without a `then`
   attribute. This case is assumed to indicate a *synchronous* action.

Uses 1 and 2 are for asynchronous actions, that will not have
completed when the function to trigger the action returns (for
example, an `XMLHTTRequest`). In these cases, give the action a way
(well, two) to signal completion once the action *does* complete.

## "Show Your Working"


I decided to use [*Javascript
promises*](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Promise),
because I never need much motivation to make things overly complex.

My first hack used an array to store each queued action as a promise,
but I realised that the only part of the array that is every useful is
*the last item*. So we only need to keep the most recent action.

But, how do we start it off? <del>Well, we start with a `null` queue, and
then have a condition that...</del> Hah, no need! We're using promises, so
we can just start off with *an already completed action*, via
`Promise.resolve()`:

{% highlight javascript %}
Queue = function() {
  var queueEnd = Promise.resolve();
  // ...
{% endhighlight %}

---

Next, we just need to keep track of the the end of the
queue. This only changes when we *enqueue* an action. To rephrase the
problem:

> Perform the action passed to `enqueue()` *after* the currently
  executing action *completes*.

Luckily, it appears that anything passed to `then()` of an already
resolved promise will not be executed until the next time through the
Javascript event loop:

{% highlight javascript %}
new Promise(function(resolve, reject) { 
  resolve();
  console.log("inside")
}).then(function() { 
  console.log("then-ed"); 
});
console.log("outside");
{% endhighlight %}

Results in output of:

    inside
    outside
    then-ed

So,

1. Throw our action's new promise (technically, the `thunk`) at
   `then()` of the current promise.

2. Make the new promise the one we mean when we say *the currently
   executing action*. We can do this because from the point of view of
   the enqueued action, *all of the previous actions* are the
   currently executing action. *Serial* queue, remember?

   And because of Javascript's *wonderful* single-threaded-ness, we
   can just swap the variables over.

{% highlight javascript %}
// ...

var promise = new Promise(...

// ...

queueEnd.then(thunk);
queueEnd = promise;
{% endhighlight %}

---

For setting up the thunk, we can take advantage of the reason we
*need* the thunk to help us:

{% highlight javascript %}
// ...

this.enqueue = function(item) {
  var thunk;
  var promise = new Promise(function(resolve, reject) {
    thunk = function() {
      // simplified version of thunk
      item();
      resolve();
    };
  });

  queueEnd.then(thunk);
  // ...
{% endhighlight %}

We need to create a function that will execute the action, and then
resolve the promise. But we can't do that until we're *in the middle
of creating the promise*. We can actually tease this apart, so that
it's clearer what is going on:

{% highlight javascript %}
function thunkify(item, resolveFn) { // Who needs error handling!?
  return function() {
    item();
    resolveFn();
  }
}

this.enqueue = function(item) {
  var thunk;
  var promise = new Promise(function(resolve, reject) {
    thunk = thunkify(item, resolve);
  });

  queueEnd.then(thunk);
// ...
{% endhighlight %}

So we use the promise as the end of the queue, but [actually use the
thunk](http://metrouk2.files.wordpress.com/2014/03/ghostbusters.gif)
as the *thing* that is executed when the current action completes.

Put it all together, et voilà. A queue that will serialise
asynchronous actions. No warranty expressed or implied.
