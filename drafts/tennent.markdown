---
title: "Tennent’s Principle of Correspondence"
layout: default
---

Over the years I've encountered several references to R.D. Tennent and
his <q>principle of correspondence</q>. The first time I was Marc
Chung's 2009 article,
[How Closures Behave In Ruby][chung-blocks]. Going back and reading it
now, Marc has removed the reference to the Principle, but
[Google turns up][google-serp-tcp] a couple of pages about
it. [A question on `programmers.stackexchange.com`][stackexchange-tcp]
has an interesting exchange, which I will discuss later. But there
doesn't appear to be a good source for *the actual statement* of the
principle… until now.[^jk]

## The Principle of Correspondence

Quoting from R. D. Tennent's Principle's of Programming Languages —
published in *1981*:

> \[…\] the underlying semantic notions for both parameter and
> definition mechanisms are simply *expression evaluation* (of an
> actual parameter or the right-hand side of a definition) and
> *identifier binding* (of a formal parameter or the left-hand side of
> a definition.) \[…\] For any parameter mechanism, an analogous
> definition mechanism is possible, and *vice versa*. This is known as
> the **_principle of correspondence_**.
  
<div class="citation">R.D. Tennent, <cite>Principles of Programming
  Languages, §9.1 Definitions and Blocks, p.130.</cite></div>

On <cite>pp. 127–8</cite> he gives two example blocks of PASCAL code,

{% highlight pascal %}
var i : integer;
begin
  i := -j;
  write(i)
end
{% endhighlight %}

and

{% highlight pascal %}
procedure p(i : integer);
  begin
    write(i)
  end;
begin
  p(-j)
end
{% endhighlight %}

and states that

> It is evident that these two fragments are equivalent.

### So, like, it's about, like, `return`, right?

No. In short, a function's *parameter list* is equivalent to the
*variable definitions* for a block. On <cite>p. 133</cite>, Tennent
states

> The principle of correspondence establishes the relationship between
> *parameters* of abstracts and *definitions*.

This demonstrates a common misconception that the principle of
correspondence is relevant to how languages handle `return`s in
lambdas. This mis-attribution, according to
[answers](http://programmers.stackexchange.com/a/120409)
[on](http://programmers.stackexchange.com/a/116405) the previously
mentioned
[question on `programmers.stackexchange.com`][stackexchange-tcp], is
due to Neal Gafter's 2006 article discussing
[adding closures to Java][gafter-java-closures][^java-closures]. In my
mind, Gafter gets a bit of a free pass, as he hedges by referring to
<q>Tennent's Correspondence and Abstraction Principles</q>.

Katz also falls prey to this in his otherwise excellent
[JavaScript Needs Blocks][js-needs-blocks], when he states:

> In short, Tennent’s Correspondence Principle says:
> > For a given expression `expr`, `lambda expr` should be equivalent.

Gafter and Katz's contexts actually appear to be Tennent's principle
of *qualification* in <cite>§9.3</cite> of Tennent's book. This is to
code blocks what *correspondence* is to parameters & variable
definitions. I'm splitting hairs here, but that's what programmers do!

## Ramifications of the principle

Tennent doesn't pass judgment in his book, only stating that

> \[…\]the principle of correspondence can be useful to a language
>  designer by pointing out possible inconsistencies and deficiencies.

 In Neal Gafter's previously mentioned
[adding closures to Java][gafter-java-closures][^java-closures], Neal
has stronger words

> Tennent's principles are very powerful because violations of them
> tend to show up in the language as flaws, irregularities,
> unnecessary restrictions, unexpected interactions or complications,
> and so on.

In [Douglas Crockford's article][crockford-tcp] – which sadly has
disappeared from the face of the Internet, leaving only a possibly
incomplete RSS-feed remnant – Crockford states

> the Correspondence Principle is descriptive, not prescriptive. He
> uses it to analyze the (by now forgotten) Pascal programming
> language, showing a correspondence between variable definitions and
> procedure parameters.

Crockford also notes that 

> Tennent does not identify the lack of correspondence of `return`
> statements as a problem.

In fact I have not found any references to `return` statements in the
book at all.

## An Example

To demonstrate a violation of the principle, Tennent points to
PASCAL's <q markdown="1">`var` parameter mechanism</q>:

> There is no definition mechanism that is the exact analog of the
> `var` parameter mechanism.

<aside markdown="1">PASCAL has a mechanism whereby a *variable* can be passed to a
function, rather than the variable's *value*. This is similar to passing
a pointer to a variable in C, but without needing to remember to
dereference the variable.</aside>

To explore this, let's start with a simple PASCAL procedure

{% highlight pascal %}
procedure inc(var i : integer);
  begin
    i := i + 1
  end;
  
var x : integer;
begin
  x := 1;
  inc(x);
  writeln(x);
end
{% endhighlight %}

running this will result in an output of `2`. An equivalent in C

{% highlight c %}
void inc(int *i) {
  *i = *i + 1;
}

int x = 1;
inc(&x);
printf("%d", x);
{% endhighlight %}

also results in `2`.

<aside markdown="1">
Interestingly, I believe these examples aren't directly replicable in
Ruby or Javascript, as you can't pass *l-values* to a function, only
*r-values*. The only way that I can think of to approximate it is by
defining `inc` as a lambda with a free variable. But, buyer beware.

For reference, since I only just learnt these definitions myself, an
*l-value* is the *location* or *address* of a variable, and an
*r-value* is the *value* that the variable contains.
</aside>

In PASCAL I know of no way to write an equivalent block definition for
the above code, but in C we could rearrange as

{% highlight c %}
int x = 1;
{
  int *i = &x;
  *i = *i + 1;
}
printf("%d", x);
{% endhighlight %}

and we can see that C's parameter mechanisms have
generally[^c-qual] equivalent block definition
counterparts, where PASCAL does not.

## Further reading

Languishing at the bottom of the aforementioned
[`programmers.stackoverflow`][stackexchange-tcp] thread – and, let's be honest, at the
bottom of this article too – is a an excellent comment with links to
[an excellent post about Tennent's principles][claus-tennent] by Claus
Reinke; the Tennent paper that Claus is referring to:
[Language design methods based on semantic principles][tennent-semantic-principles];
and to an intriguing looking paper by Reinke,
[On functional programming, language design, and persistence][claus-fpldp]

> Is there any reason to prefer functions as units of programming over
> other forms, such as predicates, relations, objects, processes,
> etc.?

Sadly, I haven't had the time to read either of the Tennent or Reinke
papers, so this is as much further reading for me as for you.

[^c-qual]: I'm hardly a C expert, thus the qualification.
[^java-closures]: Wait, this discussion has been going on for that long?!
[^jk]: Take this as eccentric writing, rather than egotistical. Please?

[gafter-java-closures]: http://gafter.blogspot.com.au/2006/08/tennents-correspondence-principle-and.html
[google-serp-tcp]: http://www.google.com/search?q=Tennent's+correspondence+principle
[js-needs-blocks]: http://yehudakatz.com/2012/01/10/javascript-needs-blocks/
[hn-on-katz]: http://news.ycombinator.com/item?id=3448027
[stackexchange-tcp]: http://programmers.stackexchange.com/questions/116395/what-is-the-good-explanation-of-tennents-correspondence-principle
[crockford-tcp]: http://java.sys-con.com/node/793338/
[chung-blocks]: http://blog.marcchung.com/2009/02/18/how-closures-behave-in-ruby.html#comment-6418304

[claus-tennent]: http://permalink.gmane.org/gmane.comp.lang.javascript.ecmascript4.general/9305
[tennent-semantic-principles]: http://www.springerlink.com/content/n43h438l03811671/
[claus-fpldp]: http://community.haskell.org/~claus/publications/fpldp.html