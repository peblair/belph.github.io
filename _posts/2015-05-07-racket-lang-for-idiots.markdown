---
layout: post
title: "A Complete Idiot's Guide to Making Languages in Racket,<br/> Part 1: Introduction"
date: 2015-05-07 10:37:45
categories: [Racket, programming, guides]
---
I have heard many people refer to Racket as being a "language laboratory."
I have also wondered what these many people meant. I mean, sure, Racket is
a Scheme language, so it has the power of Lisp-style macros (plus hygiene),
but why were people singling out *Racket*? At first, I assumed it was merely
because I happened to be a freshman at Northeastern University (where one of the
large Racket-contributing Programming Language Theory groups is) whose
Fundamentals of Computer Science professor happened to specialize in programming
languages. After some research, however, I have come to the conclusion that this
sentiment was not some sort of self-congratulating bias toward this particular
Scheme dialect, but instead a compelling fact. Racket has many unique (as far as I know)
features which enable users to craft an implementation of most any language they desire.

This being said, how the hell does one actually, well, make a language in Racket?

The answer is surprisingly simple once one gets their bearings. Before we get
started, let me just say that the prerequisite for following this guide is
a working knowledge of standard Racket. I'm not saying you need to know the
entirety of the language documentation by hand, but you should at least be comfortable
with something a step or two beyond writing fizzbuzz. Logistics aside, let's
dive in.

To keep things simple, we're going to implement a language that is identical
to Racket with a couple of exceptions:

   1. Management says `+` is too 'math-y', so we're removing it and replacing
      it with `plus`.
   2. People think `define` is too long, so we'll re-implement it as `def`.
   3. Programming is all about a single point of control. That's why constants
      are useful. As such, we'll define a constant `SEVEN` to be equal to `7`.
      (Side note: I heard a story online once where a guy worked at a company
      that made them do this with most numbers. Debugging became fun when fixes
      turned out to be things like `#define SEVEN 8`)

That's it. Think that's too simple? Read the title.

Okay, so we have a goal, but how do we start? Perhaps the best way would be
to go ahead and create our new definitions. Let's make a new file called `defs.rkt`,
where we will put these things.

The first one is easy:
{% highlight racket %}

;; Adds the two given numbers together
(define/contract (plus a b)
  (number? number? . -> . number?)
  (+ a b))

{% endhighlight %}

Assuming that a function which adds two things together warrants no additional
explanation, let's skip ahead and make our constant:
{% highlight racket %}
;; Represents the median of the interval [6, 9)
(define SEVEN 7)
{% endhighlight %}

Alright, now for the tricky one. As many of you know, `define` is not a function
in Racket, but instead a macro. Now, since we are doing a simple renaming, we *could*
use Racket's `make-rename-transformer` function, but that takes the fun out of it.
Instead, let's write a still-simple-but-not-quite-that-simple version:
{% highlight racket %}
;; Because I like syntax-parse
(require (for-syntax syntax/parse))
;; Defines a way to define without define
(define-syntax (def stx)
  (syntax-parse stx
    ; We don't really care what the contents are
    [(_ contents ...) (quasisyntax/loc stx (define contents ...))]))
{% endhighlight %}

And that is it. If we add a `(provide ...)` form, we get the following nice little
`defs.rkt` file:
{% highlight racket linenos %}
#lang racket
;; Because I like syntax-parse
(require (for-syntax syntax/parse))

(provide plus SEVEN def)

;; Adds the two given numbers together
(define/contract (plus a b)
  (number? number? . -> . number?)
  (+ a b))

;; Represents the median of the interval [6, 9)
(define SEVEN 7)

;; Defines a way to define without define
(define-syntax (def stx)
  (syntax-parse stx
    ; We don't really care what the contents are
    [(_ contents ...) (quasisyntax/loc stx (define contents ...))]))
{% endhighlight %}

Now if you open another file, add `(require "defs.rkt")`, and try to use the
above functions, you see that they work as intended. Hooray! We've added these
functions to Racket! We're done, right?

Nope. Remember, we didn't want to just *add* these functions to Racket,
but, instead, we wanted to *replace* functions in Racket. After importing
`defs.rkt`, users can still use `+` and `define` all they want. We would like
to remove this ability and constrain users to using our new definitions.

To do this, let's go ahead and make a new file. Since we're making a custom Racket,
we can go ahead and call it `cuket.rkt`. Now, if only there was a way for us to
provide, except for `+` and `define`,  all bindings from `racket`, along with our new
definitions in `defs.rkt`. Luckily, a trip to the [documentation][1] tells us that
we can do such a thing with the `(except-out ...)` and `(all-from-out ...)` forms.
Let's go ahead and put that together:
{% highlight racket linenos %}
#lang racket
(require "defs.rkt")
(provide (except-out (all-from-out racket)
                     +
                     define)
         (all-from-out "defs.rkt"))
{% endhighlight %}

Now, let's make one more file called `cuket-test.rkt`:
{% highlight racket linenos %}
#lang racket
(require "cuket.rkt")

;; Should work:
(def (foo x)
  (plus x SEVEN))

;; Should not work:
(define (bar x)
  (+ x 8))
{% endhighlight %}

Now, if you're following along in DrRacket, there's a chance you're freaking
out right now since, while the comments say that the second form should *not*
work, it, nevertheless, works fine.

Instinct might say, "Well, that's because we have yet to actually implement the
language." In reality, however, our custom language has *already been made*. Our
error is, in fact, contained within `cuket-test.rkt`. Let's briefly digress and
take a look at some of the Racket documentation.

First of all, what does that first `#lang racket` do in each file? When learning
Racket, users instinctively place it at the top of every file (if DrRacket doesn't
do it for them automatically), yet these beginners never really think about why
it is there. Yes, there are things like `typed/racket` and `scribble` which can
replace this `racket` declaration, but, if those are the only other languages,
why not make `#lang racket` implied as opposed to needing to be expressly stated
at the top of each file?

The answer to this question lies in the [documentation][2], where we see that
{% highlight racket %}
#lang racket
body ...
{% endhighlight %}
expands to
{% highlight racket %}
;; (name is some name that is generated
;;  from the file name)
(module name racket
  body ...)
{% endhighlight %}
If we follow this breadcrumb trail to yet another part of the [docs][3], we
find what we are looking for:

> The `module-path` form must be as for `require`, and it supplies the initial bindings 
> for the body forms. **That is, it is treated like a `(require module-path)` prefix before** 
> **the forms, except that the bindings introduced by `module-path` can be shadowed by** 
> **definitions and `require`s in the module body forms.**

Bingo. This is the source of our problem. How so? Let's think about it:
In `cuket.rkt`, we export every part of `racket` except for `+` and `define`,
along with the bindings provided by `defs.rkt`. Thus, inside of `cuket-test.rkt`,
when we say `(require "cuket.rkt")`, this gives us those bindings (hence the first
portion of `cuket-test` working correctly). Now, later on in the file, what happens
when we say `(define ...)`? As we just said, `cuket.rkt` doesn't provide `define`, so
we can't get it from there. Instead, since the identifiers `define` and `+` are not
provided by anything which is explicitly imported, Racket searches in the only other place
it can: `racket` (from `#lang racket`). Now, let's review the fix we've gotten ourselves
in: we want to provide all bindings from `racket` except for `define` and `+`, which
we have done in `cuket.rkt`, but, by nature of being in `#lang racket`, we have access to
*all* of the bindings in `racket`, including those we are trying to exclude. In other
words, the `#lang racket` is undermining our `provide` statement in `cuket.rkt`.

The solution? Ditch `#lang racket`.

Now, if you try and just remove that line in your file, things will break. A lot. So
don't do that. Instead, we need to replace it with another `#lang` statement. Now, we
know that `racket` provides everything one needs to be a language, since that's the `#lang`
we've been using this whole time. Furthermore, I'll go ahead and tell you that `+` and
`define` are not the things which racket provides which makes its `#lang` work (I'll
go into those in another part of this guide). Logically speaking, since we have provided
*everything* in Racket except for those two things in `cuket.rkt`, that file should provide
everything needed for it to work as a `#lang` form. Because `cuket.rkt` is not a 
collection installed in Racket, simply typing `#lang "cuket.rkt"` will not work, since the
collection directory is where it will search for that file. On the bright side, Racket
has the handy `s-exp` file in its collection directory, which is built just for things like
this. All we have to do is write `#lang s-exp "cuket.rkt"`, and we're on our way:
{% highlight racket linenos %}
#lang s-exp "cuket.rkt"
;; Remember that #lang imports
;; the file for you, so no
;; (require ...) is needed

;; Should work:
(def (foo x)
  (plus x SEVEN))

;; Should not work:
(define (bar x)
  (+ x 8))
{% endhighlight %}
Now the second definition does not work, while the first definition does. That's all there
is to making a simple little language with `s-exp`! There's more to know about `#lang` and
friends in general, of course, which I will cover in a later part of this guide. Before we
do that, though, we can take a better look at that `(all-from-out racket)` line in the [next section][4]

[1]: http://docs.racket-lang.org/reference/require.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._except-out%29%29
[2]: http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._hash-lang%29
[3]: http://docs.racket-lang.org/reference/module.html#%28form._%28%28quote._~23~25kernel%29._module%29%29
[4]: ../../2016/02/26/racket-lang-for-idiots-2.html
