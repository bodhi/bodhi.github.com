---
title: Maintaining State in a Callback
layout: default
---

<aside>There's a bunch of assumed knowledge in this that i'm not going
to introduce, as this is mostly notes and explorations to clarify my
own understanding. Apologies if this renders the following
unintelligble for you!</aside>

Straight to the problem: in audio apis like core audio and jack, you
pass a "render" function to the API, and the API creates a new thread
and calls your callback intermittently to generate new audio samples.

Then on another thread you can handle user input or other events, and
change the way the audio is rendered by the callback function.

In languages with mutable state, we could either:

1. Have some piece of shared state that is updated by the event
   handler and examined by the callback, or

2. Do the real audio rendering on the event handling thread. Then the
   callback can simply copy the rendered audio into the Core Audio
   buffer.

And really, #2 is just a specific instance of #1.

In Haskell this becomes a bit more difficult. But we need to get this
out of the way before we begin...

Preamble & Imports
-----

> module Main where
>
> import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newMVar)
> import Control.Monad (liftM, (>=>))
> import Data.IORef (IORef, newIORef, modifyIORef', readIORef)

---

On a (as-yet-unreleased) Core Audio wrapper I've been experimenting
with, the callback is:

```
type AURenderCallback a =
  Ptr a -> -- app-specific reference
  Ptr AudioUnitRenderActionFlags -> -- some flags
  Ptr AudioTimeStamp -> -- "current" time
  UInt32 -> -- input "bus" number
  UInt32 -> -- number of frames/samples in the buffer
  Ptr AudioBufferList -> -- struct that holds the buffers
  IO OSStatus
```

We can strip all the extraneous detail and work with a simplified
type

> type Callback a = a -> IO ()

The Challenge
-------

We want to be able to maintain some kind of state across *multiple*
invocations of the callback. We can't use a *State* monad because we
have nowhere to put the next state.

Since we're in `IO`, a simple initial take can try an `MVar` for
`a`. And to simplify the logic, we're just going to count how many
times the function was called:

> mvarCB :: Callback (MVar Int)
> mvarCB m = do
>   n <- takeMVar m
>   putMVar m $ n + 1

And a test harness:

> testMVarCB :: IO Int
> testMVarCB = do
>   m <- newMVar 0
>   mvarCB m
>   mvarCB m
>   takeMVar m

This works:

```
*Main> testMVarCB
2
```

An Alternative: `IORef`
-----

The callback:

> iorefCB :: Callback (IORef Int)
> iorefCB r = do
>   modifyIORef' r $ \n -> n + 1

The harness:

> testIORefCB :: IO Int
> testIORefCB = do
>   r <- newIORef 0
>   iorefCB r
>   iorefCB r
>   readIORef r

Uh huh:

```
*Main> testIORefCB
2
```

Idle Curiosity
-----

Can we make the callback generic such that we can use either?

Wrapper for the `MVar` or `IORef`:

> data State a = State { 
>   get :: IO a,
>   modify :: (a -> a) -> IO ()
> }

Creating `IORef`s:

> ioRefState :: a -> IO (State a)
> ioRefState a = newIORef a >>= \r -> pure $ State {
>   get = readIORef r,
>   modify = modifyIORef' r
> }

Creating `MVar`s:

> mvarState :: a -> IO (State a)
> mvarState a = newMVar a >>= \r -> pure $ State {
>   get = takeMVar r,
>   modify = \f -> takeMVar r >>= putMVar r . f
> }

Generic Logic:

> stateCB :: Callback (State Int)
> stateCB r = modify r $ \n -> n + 1

Generic Harness:

> testStateRef :: State Int -> IO Int
> testStateRef s = stateCB s >> stateCB s >> get s

Yes:

```
*Main> ioRefState 0 >>= testStateRef 
2
*Main> mvarState 0 >>= testStateRef 
2
```

---

Well, this was much less difficult than I was expecting. I'd read
about having to embed the callback as part of a closure, but I guess
that's only required when you can't pass an app-specific reference
when passing the callback to the API.

---

Gotta Run Em All...

> main :: IO ()
> main = do
>   mapM_ ((liftM show) >=> putStrLn)  [
>       testMVarCB,
>       testIORefCB,
>       ioRefState 0 >>= testStateRef,
>       mvarState 0 >>= testStateRef
>     ]
