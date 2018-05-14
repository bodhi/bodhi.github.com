---
layout: default
title: Passing IORefs through the FFI
---

[Previously](callback-and-state) we worked through how to persist state
with a callback managed by a framework. My simplified model made the
problem trivial, *by relaxing a key constraint*: The callback is passed
through the GHC FFI in a `FunPtr`.

<aside>
"Framework as" in "don't call us, we'll call you!"
</aside>
Let's re-introduce this constraint. As usual, we need to get the
preamble out of the way:

``` sourceCode
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import Data.IORef (IORef, newIORef)
import Control.Monad (join)
import GHC.Generics (Generic(..))
import Foreign (Ptr, Storable(..), newStablePtr, deRefStablePtr,
                castStablePtrToPtr, castPtrToStablePtr)
import Foreign.CStorable (CStorable(..))
```

``` sourceCode
import Data.StateVar as SV
```

The Problem
-----------

We have two parts:

1.  A *control* thread, that constructs the "world", and triggers the
    starting of the audio thread, passing a) a callback function and b)
    a reference for the callback.

2.  A callback function that is isolated from the control thread. It
    cannot pass data back to the control thread by a return value, it
    needs to communicate via a shared variable.

The callback function is called an arbitrary number of times, and we
want to be able to persist some kind of state across invocations.

<aside>
For example, persisting the phase of the last sample of a
fixed-frequency oscillator, so that the next audio buffer can continue
at this phase offset rather than starting at 0 phase for each buffer
</aside>
<!-- :( -->

Complexifying the Model
-----------------------

We were working with

``` sourceCode
type Callback a = a -> IO ()
```

(where `a` is the type of the variable shared between the control thread
and the callback function) as a simplification of

    type AURenderCallback a =
      Ptr a -> -- app-specific reference
      Ptr AudioUnitRenderActionFlags -> -- some flags
      Ptr AudioTimeStamp -> -- "current" time
      UInt32 -> -- input "bus" number
      UInt32 -> -- number of frames/samples in the buffer
      Ptr AudioBufferList -> -- struct that holds the buffers
      IO OSStatus

To effect the constraint we can change the type:

``` sourceCode
type Callback' a = Ptr a -> IO ()
```

And if we try to use an `IORef`

    cb :: Callback' (IORef Int)
    cb = peek >=> flip modifyIORef' (+ 1)

we get

    src/Main.lhs:52:8: error:
        • No instance for (Foreign.Storable.Storable (IORef Int))
            arising from a use of ‘peek’
        • In the first argument of ‘(>=>)’, namely ‘peek’
          In the expression: peek >=> flip modifyIORef' (+ 1)
          In an equation for ‘cb’: cb = peek >=> flip modifyIORef' (+ 1)
       |
    52 | > cb = peek >=> flip modifyIORef' (+ 1)
       |        ^^^^

The key part is

> No instance for `(Foreign.Storable.Storable (IORef Int))`

:(

I can't even begin to figure out whether we could create a `Storable`
instance for `IORef`!

------------------------------------------------------------------------

As mentioned previously, <a
href="http://hackage.haskell.org/package/StateVar">StateVar</a> provides
a good-looking abstraction over `IORef` (and others), so

    cb' :: Callback' (SV.StateVar Int)
    cb' = peek >=> flip ($~!) (+ 1)

but we run straght into the same problem:

    storable-refs/src/Main.lhs:87:9: error:
        • No instance for (Storable (StateVar Int))
            arising from a use of ‘peek’
        • In the first argument of ‘(>=>)’, namely ‘peek’
          In the expression: peek >=> flip ($~!) (+ 1)
          In an equation for ‘cb'’: cb' = peek >=> flip ($~!) (+ 1)
       |
    87 | > cb' = peek >=> flip ($~!) (+ 1)
       |         ^^^^

Can we define a `Storable` instance for `StateVar`? Not easily, but we
can create a wrapper for the functions we need and define a storable
instance for that.

`StoredVar`
-----------

My first attempts at this clearly demonstrated my confusion as I tried
all sorts of experiments with `FunPtr`s wrapping `IO a`, until I
realised *I had no need to call any of the functions from outside of
Haskell* (which is the entire reason for the existence of `FunPtr`).

Once I realised this, an implementation using `StablePtr` quickly fell
into place.

<aside>
I think <code>StablePtr</code> is needed because we don't maintain a
reference to the <code>get</code>/<code>set</code>/<code>modify</code>
functions on the non-callback side of the system, and the callback
lifecycle is managed by CoreAudio, not Haskell
</aside>
``` sourceCode
data StoredVar a = StoredVar {
  getPtr :: Ptr (),
  setPtr :: Ptr ()
  -- modify :: Ptr ()
} deriving (Generic, CStorable)

instance Storable (StoredVar a) where
 peek      = cPeek
 poke      = cPoke
 alignment = cAlignment
 sizeOf    = cSizeOf
```

We need to convert the functions we need into:

1.  A stable pointer to the result, so that it doesn't get freed while
    in use by the callback

2.  An opaque reference that can be passed through the FFI.

``` sourceCode
store :: IORef a -> IO (StoredVar a)
store a = do
  g <- newStablePtr $ (SV.get :: (IORef a) -> IO a) a
  s <- newStablePtr $ ((SV.$=) :: (IORef a) -> a -> IO ()) a
  pure $ StoredVar (castStablePtrToPtr g) (castStablePtrToPtr s)
```

Then we need to be able to reconstruct the functions from the pointers:

``` sourceCode
get :: StoredVar a -> IO a
get = join . deRefStablePtr . castPtrToStablePtr  . getPtr

set :: StoredVar a -> a -> IO ()
set sv v = do
  f <- (deRefStablePtr . castPtrToStablePtr  . setPtr) sv
  f v
```

This appears to me to be missing some type safety, and if we switch
around `s` and `g` in the definition of `store`, GHC crashes with:

    storable-refs: internal error: PAP object entered!
        (GHC version 8.2.2 for x86_64_apple_darwin)
        Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
    Process exited with ExitFailure (-6):
      storable-refs/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/bin/storable-refs

We can provide type "helpers":

``` sourceCode
type Getter t a = t a -> IO a
type Setter t a = t a -> a -> IO ()
```

and tighten up the definition a bit:

``` sourceCode
store' :: IORef a -> IO (StoredVar a)
store' a = do
  g <- newStablePtr $ (SV.get :: Getter IORef a) a
  s <- newStablePtr $ ((SV.$=) :: Setter IORef a) a
  pure $ StoredVar (castStablePtrToPtr g) (castStablePtrToPtr s)
```

``` sourceCode
get' :: Getter StoredVar a
get' = join . deRefStablePtr . castPtrToStablePtr  . getPtr

set' :: Setter StoredVar a
set' sv v = do
  f <- (deRefStablePtr . castPtrToStablePtr  . setPtr) sv
  f v
```

But it's still a bit rough, for example these two lines need to match:

    g <- newStablePtr $ (SV.get :: Getter IORef a) a

    get''' :: Getter StoredVar a

Perhaps it's worth exploring phantom types here? I don't really know...

Wrapping, er, Up
----------------

Exercise the wrapper by `get`ting the initial value, `print`ing it,
`set`ting it to a new value, and then `print`ing again.

``` sourceCode
exec :: Show a => a -> a -> IO ()
exec v1 v2 = do
  r <- newIORef v1
  rs <- store r
  val <- Main.get rs
  print val
  set rs v2
  val' <- Main.get rs
  print val'
```

``` sourceCode
main :: IO ()
main = do
  exec "hello" "world"
  exec 1 2
```
