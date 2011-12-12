---
title: Don't Tab Me Bro… (or jQuery's preventDefault vs. browser tabs)
layout: default
---

The [project](http://my.asics.co.uk) I'm currently working just had a
big update, and part of that update was a reworking of the integration
with [Mixpanel](http://mixpanel.com) to track usage of the
site. Mixpanel is great, except that they don't provide a synchronous
Javascript API for tracking events. This generally isn't a problem as
most of the events we want to track do not result in a full page
reload. *But*. But. To track links to external pages correctly, we
need to use Mixpanel's callback API to log the event before the
browser gets sent on its merry way:

{% highlight javascript %}
// Hypothetical jQuery binding
$(".navigation_tab").click(function(event) {
  var link = this;
  event.preventDefault(); // Slow down, big boy, we have to log the event first

  mpq.track("clicked on some navigation tab", { the_tab: this.id }, function() {
    // This function is called by Mixpanel after the event has been logged.
    document.location = link.href;
  });
});
{% endhighlight %}

Those of you in the audience who haven't dozed off may have already
picked up on the problem: `event.preventDefault();` +
`document.location = a_href;` = *inability to open said link in a new
tab/window* – unless you do a context-menu dance.

1. We need to `preventDefault()` because if we don't, the browser heads
off to the link, possibly aborting our attempt to track the click. 

2. We need to pass the callback to `mpq()` because of #1, so that the
browser *eventually* goes to the link.

3. The browser is sent to the original `href` programmatically by the
callback – after Mixpanel does it's thing. This action is only
indirectly in response to the original click, the native event that
triggered this whole process has been discarded.

4. If the user originally ⌘-clicked (ctrl-click for the non-Mac
people) to open the link in the new tab, they lose. The link opens in
the current window/tab. The user gets annoyed.

Ironically I've seen similar behaviour on other sites, and just
thought the developers were incompetent, or perhaps slightly
malicious. Now I've become what I hate…

## A demo

<aside>Warning: The following code snippets are written with
jQuery. If you're easily offended by such matters, please hold your
nose for the next few paragraphs.</aside>

<script type="text/javascript" src="http://code.jquery.com/jquery-1.7.1.min.js">
</script>

Try to open the following link in a new tab:

<a href="#document-location" id="doc-loc">An annoying link</a>

<script type="text/javascript">
  jQuery("#doc-loc").click(function(e) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      document.location = link.href;
    }, 500);
  });
</script>

What happens is, as described earlier, the browser's default response
to the link is suppressed via `preventDefault()`. Then, after our
hypothetical asynchronous logging method has completed, the callback
is, er, called back, setting the browser's location:

{% highlight javascript %}
jQuery("#doc-loc").click(function(e) {
  var link = this;
  e.preventDefault(); // BOOM!
  setTimeout(function() {
    document.location = link.href; // Open, but not in a new tab >_<
  }, 500);
});
{% endhighlight %}

Well, that just won't do, so we need to come up with a workaround. How
about just retriggering the event? The first time I tried this, I just
ended up with Javascript errors, so I deleted that code. But for this
writing I tried `dispatchEvent()` which actually retriggers the jQuery
handlers:

<a href="#redispatch" id="dispatch">Double-triggered link</a>

<script type="text/javascript">
jQuery("#dispatch").click(function(e) {
  if (!e.originalEvent.retriggered) {
    event = e;
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      e.originalEvent.retriggered = true;
      link.dispatchEvent(e.originalEvent);
    }, 500);
  }
});      
</script>

{% highlight javascript %}
jQuery("#dispatch").click(function(e) {
  if (!e.originalEvent.retriggered) {
    event = e;
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      e.originalEvent.retriggered = true; // To avoid retriggering to ∞.
      link.dispatchEvent(e.originalEvent); // Try the event again.
    }, 500);
  }
});      
{% endhighlight %}

But since we called `preventDefault()` the first time round, the
retriggered event *also* skips the browser's default action. But,
we're on the right track, and when you try something, and it doesn't
work, then the next step is always do the same thing with a copy of
the original:

<a href="#copied-event" id="copy">Trigger a copy</a>

<script type="text/javascript">
jQuery("#copy").click(function(e) {
  if (!$(this).data("delayed-event")) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
        var newEvent = document.createEvent("MouseEvent");
        newEvent.initMouseEvent(e.type, e.bubbles, e.cancelable, e.view, 
                   e.detail, e.screenX, e.screenY, e.clientX, e.clientY, 
                   e.ctrlKey, e.altKey, e.shiftKey, e.metaKey, 
                   e.button, e.relatedTarget);
        $(link).data("delayed-event", true);
        link.dispatchEvent(newEvent);
        $(link).data("delayed-event", false);
    }, 500);
  }
});      
</script>

{% highlight javascript %}
jQuery("#copy").click(function(e) {
  if (!$(this).data("delayed-event")) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      // create a copy of the original event
      var newEvent = document.createEvent("MouseEvent");
      newEvent.initMouseEvent(e.type, e.bubbles, e.cancelable, e.view, 
                 e.detail, e.screenX, e.screenY, e.clientX, e.clientY, 
                 e.ctrlKey, e.altKey, e.shiftKey, e.metaKey, 
                 e.button, e.relatedTarget);
      $(link).data("delayed-event", true); // Avoid infinite death spiral
      link.dispatchEvent(newEvent); // Trigger 'new' event
      $(link).data("delayed-event", false);
    }, 500);
  }
});      
{% endhighlight %}

Cool! It works. Wrapping it up in a proper little function is left as
an exercise to the reader. Another method to try would be triggering
the callback via plain old jQuery::

<a href="#triggered-event" id="trigger">Plain old jQuery trigger</a>

<script type="text/javascript">
jQuery("#trigger").click(function(e) {
  if (!$(this).data("delayed-event")) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      $(link)
        .data("delayed-event", true)
        .trigger("click")
        .data("delayed-event", false);
    }, 500);
  }
});      
</script>

{% highlight javascript %}
jQuery("#trigger").click(function(e) {
  if (!$(this).data("delayed-event")) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      $(link)
        .data("delayed-event", true)
        .trigger("click") // click the link again
        .data("delayed-event", false);
    }, 500);
  }
});      
{% endhighlight %}

It doesn't work, and from [reading the
documentation](http://api.jquery.com/trigger/):

> Although `.trigger()` simulates an event activation, complete with a
> synthesized event object, it does not perfectly replicate a
> naturally-occurring event.
>
> To trigger handlers bound via jQuery *without also triggering the
> native event*, use `.triggerHandler()` instead.

(Emphasis mine) I'm not really sure if should work or not. The first
paragraph would indicate *maybe*, the second implies *yes*. I'm
probably doing something wrong.

I imagine this is documented somewhere in one of those *Javascript:
all the awesome stuff, none of the crap*–type books, but I don't have
copies of them, so I can't check. If you can point me to a good
discussion about this, [please do](http://twitter.com/bodhi). As for
my project, I'll be reworking the tracking code to get my
open-in-new-tab functionality back as soon as I get time, <small>and
test in Internet Explorer, <small>and…</small></small>

<!-- Why are you digging around in my HTML?

## Trash to cut-paste from

Try to open each of the following links in a new tab:

<ol>
  <li><a href="#document-location" id="doc-loc">#document-location</a></li>
  <li><a href="#delayed-click" id="delay-click">#delayed-click</a></li>
  <li><a href="#trigger-click" id="trigger">#trigger-click</a></li>
</ol>


<script type="text/javascript">
  jQuery("#doc-loc").click(function(e) {
    var link = this;
    e.preventDefault();
    setTimeout(function() {
      document.location = link.href;
    }, 500);
  });

  jQuery("#delay-click").click(function(e) {
    if (!$(this).data("delayed-event")) {
      var link = this;
      e.preventDefault();
      setTimeout(function() {
        newEvent = document.createEvent("MouseEvent");
        newEvent.initMouseEvent(e.type, e.bubbles, e.cancelable, e.view, 
                   e.detail, e.screenX, e.screenY, e.clientX, e.clientY, 
                   e.ctrlKey, e.altKey, e.shiftKey, e.metaKey, 
                   e.button, e.relatedTarget);
        $(link).data("delayed-event", true);
        link.dispatchEvent(newEvent);
        $(link).data("delayed-event", false);
      }, 500);
    }
  });

  jQuery("#trigger").click(function(e) {
    if (!$(this).data("delayed-event")) {
      var link = this;
      e.preventDefault();
      setTimeout(function() {
        $(link).data("delayed-event", true);
        $(link).click();
        $(link).data("delayed-event", false);
      }, 500);
    }
  });      
</script>

-->