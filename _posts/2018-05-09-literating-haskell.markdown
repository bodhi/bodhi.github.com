---
layout: default
title: Stepping Stones Towards Literate Programming
---

A logbook of setting up [literate
programming](https://en.wikipedia.org/wiki/Literate_programming) for
Haskell experiments.

Creating a testbed Haskell project with [Stack](https://www.haskellstack.org)
-----------------------------------------------------------------------------

To get started, create a new project with Stack:

    $ stack new literate-haskell

But this creates a slightly complicated project with many extraneous
files.

Ah, it seems that Stack supports
[templates](https://github.com/commercialhaskell/stack-templates), and
there's one called `simple`. Let's try that.

    $ stack new literate-haskell simple

That creates a much simpler project:

    $ find .
    <uninteresting files omitted>
    ./LICENSE
    ./literate-haskell.cabal
    ./README.md
    ./Setup.hs
    ./src
    ./src/Main.hs
    ./stack.yaml

Switching to use Literate Haskell is easy:

    mv src/Main{,l}hs

Ok, now we can start writing Literate code!

Installing [Pandoc](https://pandoc.org)
---------------------------------------

We can write Literate Haskell files in Markdown syntax, and transform
the `.lhs` file with Pandoc into a regular Markdown file that can be
processed by Jekyll (via GitHub Pages).

    $ brew install pandoc

Now we can tell Pandoc to generate Markdown from "Literate Markdown":

    $ pandoc -f markdown+lhs -t markdown src/Main.lhs

Cool. Let's add some front matter to make Jekyll happy:

    $ head -n 4 src/Main.lhs
    ---
    title: Stepping Stones Towards Literate Programming
    layout: default
    ---

Hmm, it's not being included in the output:

    $ pandoc -f markdown+lhs -t markdown src/Main.lhs | head -n 4
    A logbook for setting up [literate
    programming](https://en.wikipedia.org/wiki/Literate_programming) for
    Haskell experiments.

Standalone Mode
===============

By accident when messing around with Pandoc output templates, I
accidentally discovered that [`--standalone`
(`-s`)](https://pandoc.org/MANUAL.html#general-writer-options) will
include the front matter. Not sure if this is documented somewhere...
So:

    $ pandoc -s -f markdown+lhs -t markdown src/Main.lhs | head -n 4
    ---
    layout: default
    title: Stepping Stones Towards Literate Programming
    ---

The gas is on. But hang on, code blocks are being annotated:

    ``` {.sourceCode .literate .haskell}
    module Main where
    ```

and Jekyll doesn't like that syntax. Can we disable it? Yes, by turning
off the `fenced_code_attributes` extension:

    $ pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs
    <uninteresting bits omitted>
    ``` sourceCode
    module Main where
    ```

And Kramdown (the GitHub Markdown processor) seems to be ok with this.

<aside>
Discovering this took a fair bit of trawling through Pandoc
documentation and GitHub issues.
</aside>
<!-- :( -->

Generating Files for Jekyll
---------------------------

For now, we can just write an `index.markdown` file

    $ pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs > index.markdown

and Jekyll can show it to us at <http://localhost:4000/literate-haskell>

Some Oddities
-------------

I use `<aside>` to put things → over there, but *Pandoc* seems to get
confused with the element following an `<aside>`. From the previous
section:

<aside>
Yep, over here. Except on narrow screens, when it's more like "down
below, here".
</aside>
    <aside>Discovering this took a fair bit of trawling through Pandoc
    documentation and GitHub issues.</aside>

    Generating Files for Jekyll
    --------------

and the generated output:

    <aside>
    Discovering this took a fair bit of trawling through Pandoc
    documentation and GitHub issues.
    </aside>
    Generating Files for Jekyll
    ---------------------------

The blank line after `</aside>` has been lost, which confuses Kramdown,
we end up with

    <p>Generating Files for Jekyll
    —————————</p>

instead of the expected `<h2>`

    <h2>Generating Files for Jekyll</h2>

Ah, we can *interrupt* with a comment:

    <aside>Discovering this [...]</aside>

    <!-- :( -->

    Generating Files for Jekyll
    --------------

and... er... what?

    <aside>
    Discovering this [...]
    </aside>
    ``` sourceCode
    !-- :( -->
    ```

    Generating Files for Jekyll
    ---------------------------

Fine, try indenting with a space

     <!-- :( -->

and...

    <aside>
    Discovering this took a fair bit of trawling through Pandoc
    documentation and GitHub issues.
    </aside>
    <!-- :( -->

    Generating Files for Jekyll
    ---------------------------

👏👏👏 <!-- 3 invisible clapping emojis above -->

<aside>
I can't actually see these emoji in Emacs, and the reasons for that are
<a
href="https://news.ycombinator.com/item?id=13011185">"hotly
debated"</a>.
</aside>
Oh. Next bug is that Pandoc isn't passing `markdown="1"` through, it's
just being stripped. I'll just re-write the previous aside in pure HTML
for now, and try to resolve it later.

Finally, Some Haskell
---------------------

``` sourceCode
module Main where
```

``` sourceCode
main :: IO ()
main = do
  putStrLn "hello world"
```

and we can run it directly:

    $ stack build  --exec literate-haskell
    literate-haskell-0.1.0.0: build (exe)
    Preprocessing executable 'literate-haskell' for literate-haskell-0.1.0.0..
    Building executable 'literate-haskell' for literate-haskell-0.1.0.0..
    [1 of 1] Compiling Main             (
      src/Main.lhs,
      .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/literate-haskell/literate-haskell-tmp/Main.o
    )
    Linking .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/literate-haskell/literate-haskell ...
    literate-haskell-0.1.0.0: copy/register
    Installing executable literate-haskell in
      /<snip>/literate-haskell/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/bin
    hello world

👏👏👏 <!-- 3 invisible clapping emojis above -->

<aside>
Why does the HTML comment not need a leading space here?! Let's check
over there ←
</aside>
``` sourceCode
!-- test... -->
```

Sigh.

Automating It
-------------

Let's use a `Makefile` to generate the Jekyll post:

    ../_posts/2018-05-09-literating-haskell.markdown: src/Main.lhs
        pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs \
     > ../_posts/2018-05-09-literating-haskell.markdown

But can't we reuse the target name in the command? Ah,
[`$@`](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html),
bingo!

    ../_posts/2018-05-09-literating-haskell.markdown: src/Main.lhs
        pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs > $@

And so,

    $ make
    pandoc -s -f markdown+lhs -t markdown-fenced_code_attributes src/Main.lhs >
      ../_posts/2018-05-09-literating-haskell.markdown
    $ make
    make: `../_posts/2018-05-09-literating-haskell.markdown' is up to date.

👏👏👏, I guess? <!-- 3 invisible clapping emojis above -->
