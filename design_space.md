# Design Space

In this document, I want to lay out the design space for the surface language. That is, the language in which the distinction between higher-order effects and algebraic effects (and by extension between handling and elaboration) is hidden.

To do so, I will attempt to write the examples from the Hefty Algebras paper in both the surface and core language.

## Some syntax notes

- The syntax I'm using is a strange combination of Haskell and Rust.
- `[...]` are type parameters. I'm trying to make them all explicit and not do a Haskell-style implicit parameter thingy.
- Values are implicitly cast to computations if necessary by wrapping them in a `return(...)`
-  We have parentheses for function calls because that's clearer if there's no currying. We need this to elegantly support operations with multiple parameters.
- I use `{...}` as a combined scope and do notation with `;` separating computations.
- Continuations are invoked with `resume` (instead of an explicit `k` variable). This is following the lead of Koka. It must be a function though, not a language construct.
- I'm stealing some syntax design from Rust, so enabling Rust syntax highlighting makes sense.
- Using Koka's `<>` for effect rows frees `!` up for the never type, which is especially useful since many operations do not resume and therefore have the never type.
- In the syntax for `effect`, the return type of the operations is the type of the value they call `resume` on.
- In the syntax for `handler`, the return type is the type they elaborate into. This is probably a bit confusing.

# The translation from surface to core

- Each effect `E` with higher-order operations is split into `E_a` and `E_h` (for algebraic and higher-order, respectively).
  - The algebraic operations must have return type `<F> M[a]`
  - The higher-order operations must have return type `<F> a`
  - `E_a` contains the former, `E_h` contains the latter.
- If the handler `hE` is an effect from `<E> a -> <F> M[a]` where `M` is some monad, then
- The handler is split into a handler `hE_a` and `hE_h`
- In `hE_h`:
  - Any occurrence of `E` is rewritten to `E_a`.
  - Any occurrence of `hE` is rewritten to `hE`.

> Note: we might choose to not rewrite and make this explicit somehow.

## Revelations
- We need higher-order effects specifically because we need to distribute handlers over the computations.
- Because of how algebraic effects works, this also distributes continuations over each computations.
- But this presents a problem, because that doesn't work in many higher-order effects.
- This is where elaboration comes in. Elaboration is kind of like "pre-handling". The elaboration comes before other handling and therefore enforces some order?

## "I'm an idiot" questions
- ~~Can't we preserve algebraic properties of higher-order effects by turning them into non-higher-order effects by turning computations `<e> a` into functions `() -> <e> a`. Then most things just kinda work. This is similar to the models for higher-order effects with thunked computations.~~ No, see revelation above

## Design Questions
- What is the "handle type" of the higher-order operations.
  - If we really want to make the elaboration-handling divide opaque then it would need to be the same as the other operations.
  - For now, I stuck with defining it strictly as an operation.

- How is the type of the elaboration defined?
  - Do the effects that we elaborate into need to be defined explicitly?
  - Are those identical to the effects in the handler return type?

- If we elaborate into additional effects, are those handled in that handler as well?
  - Due to the optionally transactional exception handling, I think we should NOT handle those, because then we can decide on the order between the `Transact` and `State` effect separately from the `Exc` effect.
  - This means that more effects can elaborate into `Transact` and we can combine their behaviour. If we do indeed do not do this, then this new effect is part of the return type of the handler.
  - I.e. a the handler for `Exc` has type `<Exc> a -> <Transact> a`. But then it also becomes part of the type for the non-higher-order operations, which might not be what we want. Of course we could have some implicit cast.
  - The transactionality stays defined in the order that the handlers are invoked in, which might be enough, but maybe we want it to be defined by the handler? This is also not the case in the hefty algebras paper though.

- Is the handler that is exposed in the surface language always the combination of the elaboration and the handler in the core language or does it make sense to expose them separately as well.

- Computation types
  - How "first-class" do these need to be?
  - Higher-order operations definitely need them.
  - Do we need variables that can hold types like `<e> a`?
  - Do we need functions that can take `<e> a`?
  - I _think_ we don't.

- What about operations that have both value and computation parameters? Can we have partially higher-order operations? The `local` effect might be a good example here.

## Examples
### Simple Exceptions

- Assume the language has algebraic data types and a `Maybe` type with constructors `Just` and `Nothing` like Haskell.
- In `catch`, the type of `m1` depends on the elaboration. If the effect elaborates into something else, the type changes too. Hence, we must assume that it's always `Exc_h` to `Exc_a`, otherwise it's undefined. Or it needs to be user-specified. This is similar to a type inference problem. Say we have functions `toB :: A -> B1`, `toB :: A -> B2`, `fromB :: B1 -> C` and `fromB :: B2 -> C`, then what is the implementation of `fromB . toB`?

#### Surface
```rust
effect Exc {
    throw() -> <Exc> a
    // catch is higher-order because it takes computations
    catch(m1: <Exc> a, m2: <Exc> a) -> <Exc> a
}

handler hExc: <Exc> a -> Maybe[a] {
    // The algebraic operations must return Maybe[a], because that's what the
    // handler specifies
    return(x: a) -> Maybe[a] {
        Just(x)
    }
    throw() -> Maybe[a] {
        Nothing
    }
    // catch must return `<Exc> a`, which represents the elaboration.
    // This is a bit weird because we want it to elaborate away from
    // `Exc` into the algebraic subset of `Exc`. It can therefore not
    // contain the catch effect itself. Maybe we need an operation for
    // algebraic subset `Alg[Exc]` of `Exc`, in which case the signature
    // for catch is (<Alg[Exc]> a, <Alg[Exc]> a) -> <Alg[Exc]> a.
    // Other syntax is possible (!Exc, Exc', etc.).
    // Alternatively maybe it's okay to just let the return type be
    // Maybe[a].
    catch(m1: <Exc> a, m2: <Exc> a) -> <Exc> a {
        v <- hExc(m1);
        match v {
            Just(v) => { resume(v) }
            Nothing => {
                v <- m2;
                resume(v)
            }
        }
    }
}
```

#### Core
```rust
effect Exc_a {
    throw() -> <Exc_a> a
}

heffect Exc_h {
    catch(m1: <Exc_h> a, m2: <Exc_h> a) -> <Exc_h> a
}

handler hExc: <Exc_a> a -> Maybe[a] {
    return(x: a) -> Maybe[a] {
        Just(x)
    }
    throw() -> Maybe[a] {
        Nothing
    }
}

elaboration eExc: <Exc_h> a -> <Exc_a> a {
    catch(m1: <Exc_a> a, m2: <Exc_a> a) -> <Exc_a> a {
        v <- hExc(m1);
        match v {
            Just(v) => {
                resume(v)
            }
            Nothing => {
                v_ <- m2;
                resume(v_)
            }
        }
    }
}
```

### Lambda as a Higher-Order Operation
- I need generics to have both interpretation
- We even need to be generic over effects
- The return could be implicitly generated, because it's unnecessary for higher-order effects that only have an elaboration.
- I'm wondering about values of type `<e> a`. These are sort of equivalent to `() -> <e> a`, i.e. a function from unit to `a` with effects `e`. Does it need to be higher-order if we do it that way. I guess it's still generic over effects and constructs an effectful computation. Not sure whether that counts.

#### Surface
```rust
// We're generic over:
// * e, the set effects that the lambdas and variable computations can have

effect Lam[e] {
    lam[a,b](f: <e> a -> <e> b)           -> <Lam[e]>   (<e> a -> <e> b)
    var[a]  (x: <e> a)                    -> <Lam[e],e> a
    app[a,b](f: <e> a -> <e> b, x: <e> a) -> <Lam[e],e> b
}

// Call by value
handler CBV: <Lam[e]> a -> <e> a {
    return(x: a) -> x
    lam[b,c](f: <e> b -> <e> c) -> <e> a {
        resume(comp)
    }
    var(x: <e> a) -> <e> a {
        // Actually, x will be basically of type a (wrapped in a return)
        // but we have to do it this way because var expects <e> a to enable
        // call by name.
        v <- x;
        resume(v)
    }
    app[b,c](f: <e> b -> <e> c, x: <e> b) -> <e> a {
        // x is evaluated before the function and
        // cast to a computation when passed into `f`.
        v <- x;
        r <- f(v);
        resume(r)
    }
}

// Call by name
handler CBN: <Lam[e]> a -> <e> a {
    return(x: a) -> <e> a {
        x
    }
    lam[b,c](fun: <e> b -> <e> c) -> (<e> b -> <e> c) {
        resume(comp)
    }
    var[b](x: <e> b) -> <e> a {
        v <- x;
        resume(v)
    }
    app[b,c](f: <e> b -> <e> c, x: <e> b) -> <e> a {
        res <- f(x);
        resume(res)
    }
}
```

#### Core
```rust
// All operations are higher-order, so the corresponding algebraic effect is empty
effect Lam_a {}

heffect Lam_h[e] {
    lam[b,c](f: <e> b -> <e> c)           -> <Lam[e]>   (<e> b -> <e> c)
    var[b]  (x: <e> b)                    -> <Lam[e],e> b
    app[b,c](f: <e> b -> <e> c, x: <e> b) -> <Lam[e],e> c
}

handler hCBV: <Lam_a> a -> a {
    return(x: a) -> x,
}

elaboration eCBV: <Lam_h[e]> a -> <e> a {
    lam[b,c](fun: <e> b -> <e> c) -> {
        resume(comp)
    }
    var(x: <e> a) -> {
        v <- x;
        resume(v)
    }
    app[b,c](f: <e> b -> <e> c, x: <e> b) -> {
        v <- x;
        r <- f(v);
        resume(r)
    }
}
```

#### Alternative that is maybe not higher-order?

In fact, only `app` needs to be an effect? `lam` doesn't do much and `var` has the same implementation every time? Idk, maybe that changes if you get into more complex things (like call-by-need).

Does this break algebraic properties? With raw computations it definitely does, but with functions it might be okay? It just piggy-backs on functions in the language, so it's just some weird higher-order functions. In a way, the continuation is not distributed over values (because they're no computations), so the algebraic law still seems to hold.

Okay I think I figured it out, we need handlers to be applied to the computations, otherwise it's not modular. So the algebraic law holds, but we get in trouble when combining it with other effects.

```rust
// A thunked `a` with effects `e`
type V[a,e] = () -> <e> a

// A function from thunked `a` to `b` with effects `e`
type F[a,b,e] = V[a,e] -> <e> b

effect Lam[e] {
    lam[b,c](f: F[b,c,e])            -> <Lam[e]>   F[b,c,e]
    var[b]  (x: V[b,e])              -> <Lam[e],e> V[b,e]
    app[b,c](f: F[b,c,e], x: V[b,e]) -> <Lam[e],e> b
}

handler CBV: <Lam[e]> a -> <e> a {
    return(x: a) -> <e> a {
        x
    }
    lam[a,b](f: F[b,c,e]) -> <e> a {
        resume(f)
    }
    var[a](x: V[b,e]) -> <e> a {
        v <- x();
        resume(v)
    }
    app[a,b](f: F[b,c,e], x: V[b,e]) -> {
        // Since x is now a function, we need to call it
        v <- x();
        // `fun` creates an anonymous function
        res <- f(fn() { v });
        resume(res)
    }
}
```

### Optionally Transactional Exception Catching
#### Surface

and also basically core because only catch needs elaboration

```rust
effect Transact {
    sub[a,t](b: Ref[t] -> <e> a, k: t -> <e> a) -> <Transact> a
    jump[a,t](ref: Ref[t], x: t) -> <Transact> a
}

effect Exc {
    throw() -> <Exc> a
    catch(m1: <Exc> a, m2: <Exc> a) -> <Exc> a
}

handler hTransact {
    sub(b: (t -> <e> a) -> <e> a, k: t -> <e> a) -> {
        res <- b(k);
        resume(res)
    }
    jump(ref: t -> <e> a, x: t) -> {
        res <- ref(t);
        resume(res)
    }
}

// TODO: Should the type be `<Exc> a -> <Transact> Maybe[a]`?
handler hExc: <Exc> a -> Maybe[a] {
    return(x: a) -> Maybe[a] {
        Just(x)
    }
    throw() -> Maybe[a] {
        Nothing
    }
    // This is difficult, because we now implicitly declare that Transact is
    // part of the effects that we elaborate into, in addition to Exc_a.
    // Maybe this needs to be explicit.
    catch(m1: <Exc> a, m2: <Exc> a) -> <Transact,Exc> a {
        sub (
            fn (r) {
                v <- hThrow(m1);
                match v {
                    Just(v) => {
                        resume(v)
                    }
                    Nothing => {
                        jump(r)
                    }
                }
            },
            fn (_) {
                v <- m2
                resume(v)
            }
        )
    }
}
```

### Logic Programming
#### Surface
```rust
effect Choice {
    or()                 -> <Choice> bool
    fail()               -> <Choice> !
    once(m: <Choice> a)  -> <Choice> a
}

handler hChoice: <Choice> a -> [a] {
    return(x: a) -> [a] {
        [x]
    }
    or() -> [a] {
        a <- resume(false);
        b <- resume(true);
        a ++ b
    }
    fail() -> [a] {
        []
    }
    once(m: <Choice> a) -> <Choice> a {
        list <- hChoice(m);
        match list {
            head::_ => {
                resume(head)
            }
            _ => {
                fail()
            }
        }
    }
}
```

#### Core
```rust
effect ChoiceA {
    or()   -> <ChoiceA> bool
    fail() -> <ChoiceA> !
}

heffect ChoiceH {
    once(m: <ChoiceA> a) -> <ChoiceA> a
}

handler hChoice: <ChoiceA> a -> [a] {
    return(x: a) -> {
        [x]
    }
    or() -> [a] {
        a <- resume(false);
        b <- resume(true);
        a ++ b
    }
    fail() -> [a] {
        []
    }
}

elaboration eChoice: <ChoiceH> a -> <ChoiceA> a {
    once(m: <ChoiceA> a) -> <ChoiceA> a {
        list <- hChoice(m);
        match list {
            first::_ => {
                resume(first)
            }
            _ => {
                fail()
            }
        }
    }
}
```

### Concurrency
- `resume` needs to be a function, not a language construct. Or at least it needs to be possible to use it as a function.

#### Surface
```rust
// A data structure for Concurrency to be handled into
// It must be parametrized by the effects
data Resumption[a, e] {
    Done(a)
    More(() -> <e> Resumption[a,e])
}

// Exhaust a resumption to its final value
fn exhaust[a](r: Resumption[a,e]) -> <Yield,e> a {
    match r {
        Done(a) => { a }
        More(r) => {
            a <- r();
            yield();
            exhaust(r)
        }
    }
}

// Try to take a step in the resumption
// If we're already done, we do nothing
fn step[a](r: Resumption[a,e]) -> <e> a {
    match r {
        Done(a) => { Done(a) }
        More(f) => { f() }
    }
}

fn interleave[a](r1: Resumption[a,e1], r2: Resumption[b,e2]) -> <Yield,e1,e2> a {
    match r1 {
        Done(a) => {
            exhaust(r2);
            a
        }
        More(f) => {
            r1_ <- f();
            yield();
            r2_ <- step(r2);
            yield();
            interleave(r1, r2)
        }
    }
}

// Allows a computation to be paused
// In particular the yield operation indicates a place where side effects from another c
// computation may interleave this computation.
effect Concurrency {
    yield() -> <Concurrency> ()
    fork(m1: <Concurrency> a, m2: <Concurrency> b) -> <Concurrency> a
    atomic(m1: <e> a) -> <Concurrency,e> a
}

handler hConcurrency: <Concurrency,e> a -> <e> Resumption[a,e] {
    return(x: a) -> <e> Resumption[a,e] { Done(a) }
    
    yield() -> <e> Resumption[a,e] { More(resume) }
    
    fork(m1: <Concurrency> a, m2: <Concurrency> b) -> <Concurrency> a {
        r1 <- hConcurrency(m1);
        r2 <- hConcurrency(m2);
        interleave(r1, r2)
    }****

    // atomic(m1: <e> a) -> <Transact,e> a {
    //     sub(
    //         fn(ref) {
    //             y <- m1;
    //             jump(ref)
    //         },
    //         resume,
    //     )
    // }
}
```

#### Core
```rust
effect Concurrency {
    fork(m1: <Concurrency> a, m2: <Concurrency> b) -> <Concurrency> a
    atomic(m1: <e> a): <Concurrency,e> a
}
```
