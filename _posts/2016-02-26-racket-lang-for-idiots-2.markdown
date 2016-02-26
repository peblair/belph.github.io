---
layout: post
title: "A Complete Idiot's Guide to Making Languages in Racket,<br/> Part 2: Ditching all-from-out"
date: 2016-02-26 16:34:45
categories: [Racket, programming, guides]
---
[Last time][1] we set the stage for understanding how Racket languages are implemented, but
we made use of a crutch to get Cuket up and running; namely, the phrase `(all-from-out racket)`.
Before continuing on, let's remove this dependency and, in the process, gain a better understanding
of how Racket expands programs.

To do so, we will be creating a new language. Kind of. From the user's perspective, this language
will be identical to Cuket (with an exception, as we will see). Under the hood, however, we will 
no longer simply pass the buck off to Racket to handle the heavy lifting. Practically, this language
will be a bit worse, but, in terms of our understanding, it will be much better. As such, (for lack
of a better name) we'll call this language Better-Cuket. To start, we copy `cuket.rkt` to 
`better-cuket.rkt` and `cuket-test.rkt` to `better-cuket-test.rkt` (in the latter, we also change
`#lang s-exp "cuket.rkt"` to be `#lang s-exp "better-cuket.rkt"`).

Like a band-aid, let's go ahead and just rip out the `(all-from-out ...)` expression from 
`better-cuket.rkt`, giving us the following:
{% highlight racket %}
;; better-cuket.rkt
#lang racket
(require "defs.rkt")
(provide (all-from-out "defs.rkt"))
{% endhighlight %}

### Requirement 1: #%module-begin

If we take a look at `better-cuket-test.rkt`, we now see the following error:

> `better-cuket-test.rkt:1:0: module: no #%module-begin binding in the module's language in:`
> `(module anonymous-module "better-cuket.rkt" (#%module-begin (def (foo x) (plus x SEVEN))))`
>  `#(1 62)`

What is this going on about? To understand, we need to look at how Racket expands our file. While
I touched on it before, I left out the whole story. Let's take a look at `better-cuket-test.rkt`:
{% highlight racket %}
;; better-cuket-test.rkt
#lang s-exp "better-cuket.rkt"

(def (foo x)
  (plus x SEVEN))
{% endhighlight %}
If you recall, I mentioned that this will be expanded out to the following:
{% highlight racket %}
(module better-cuket-test "better-cuket.rkt"
  (def (foo x)
    (plus x SEVEN)))
{% endhighlight %}
The truth is that Racket will expand this *further* into the following:
{% highlight racket %}
(module better-cuket-test "better-cuket.rkt"
  (#%module-begin
    (def (foo x)
      (plus x SEVEN))))
{% endhighlight %}
(Note: You can actually see this level of expansion by selecting "Macro hiding: Disable" in
DrRacket's Macro Stepper)

Now, the question remains of what that `#%module-begin` business *is*. Here
is the gist:

Modules live in the *top-level context*. In other words, they are syntactic forms
which Racket knows how to expand as top-level (i.e. outermost) expressions in a program.
Once fully expanded, a `module` form has one thing in its body: a `#%plain-module-begin`
form. This tells the expander that the contents of that form (i.e. the body of the module)
is ready to be expanded. This allows for some neat tricks. For example, consider the following
program:
{% highlight racket %}
#lang racket
(add1 0)
(add1 1)
(add1 2)
(add1 3)
{% endhighlight %}
If you run this program, you'll notice that the REPL prints out the results:

    1
    2
    3
    4

At the same time, however, your program does not have any print statements. How does this
work? Does Racket just *know* to act specially in these situations? Actually, no. The [documentation][2]
mentions this behavior explicitly. What happens is that the `#%module-begin` form for
`#lang racket/base` actually wraps all top-level expressions in a call to a [`print-values` function][3]
which prints every non-`#<void>` result to the REPL.

The punchline is that `#%module-begin` is just a macro which expands to a `#%plain-module-begin`
form (or to another macro which does). This means that it allows you to perform arbitrary
preprocessing of the module body (i.e. of the program written in your language). To illustrate
this, we can create the following `my-modbeg.rkt` file:
{% highlight racket %}
;; my-modbeg.rkt
#lang racket
(require (for-syntax syntax/parse))

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out (my-#%module-begin #%module-begin)))

;; Helper for my-#%module-begin
;; (Yes, we're only checking the top-level right now...
;;  ...more on that later)
(define-for-syntax (find-add1s stx)
  (syntax-parse stx
    [(((~literal add1) args ...) . rst)
     (quasisyntax/loc stx
       ((displayln (format "You're calling add1 with ~a!" '(args ...)))
        (add1 args ...) #,@(find-add1s #'rst)))]
    [(other . rst) #`(other . #,(find-add1s #'rst))]
    [() stx]))

;; We'll name our #%module-begin something else
;; and rename it when exported
(define-syntax (my-#%module-begin stx)
  ;; Preprocess and delegate to the racket/base #%module-begin
  (syntax-parse stx
    [(_ . body) #`(#%module-begin #,@(find-add1s #'body))]))
{% endhighlight %}
Then, we can make a corresponding `my-modbeg-test.rkt` file:
{% highlight racket %}
;; my-modbeg-test.rkt
#lang s-exp "my-modbeg.rkt"

(sub1 0)
(add1 2)
(add1 3)
(sub1 4)
{% endhighlight %}
When you run the above, you'll get the following output on the REPL:

    -1
    You're calling add1 with (2)!
    3
    You're calling add1 with (3)!
    4
    3
    
As you can imagine, the possibilities for what you can do with this are endless.

For now, let's patch up our `better-cuket.rkt` file as follows:
{% highlight racket %}
;; better-cuket.rkt
#lang racket
(require "defs.rkt")
(provide (all-from-out "defs.rkt")
         #%module-begin) ;; We'll just use Racket's
{% endhighlight %}

### Requirement 2: #%app and #%datum

Returning to our `better-cuket-test.rkt`, we find a new error:

> `#%app: unbound identifier; also, no #%top syntax transformer is bound in: #%app`

Cutting to the chase, let's talk some more about expansion. During this low-level
expansion process, Racket will eventually stumble upon any function calls you have.
In our case, the `(plus x SEVEN)` call inside of `foo` is the culprit.

When the Racket expander sees syntax, it wants to reduce it somehow. If it's an identifier,
fine, that's simple enough. If it's a macro use, then that's easy; just call the macro
and continue expanding the result. There are two special cases, however: function calls
and literals.

In truth, the expander basically leaves them alone. There is one caveat, however: it
puts them in either an `#%app` or `#%datum` syntactic form first. Consider the following expression:
{% highlight racket %}
(+ 2 3)
{% endhighlight %}
The expander will convert this to the following:
{% highlight racket %}
(#%app + (#%datum . 2) (#%datum . 3))
{% endhighlight %}

These are macros in the same way that `#%module-begin` is. As such, they allow you to
intercept arbitrary function calls and literals in the module body. Suppose we change
`my-modbeg-test.rkt` to the following:
{% highlight racket %}
#lang s-exp "my-modbeg.rkt"

(define (foo x)
  (add1 x))

(sub1 0)
(add1 2)
(add1 3)
(sub1 4)
(foo  5)
{% endhighlight %}
This will print the following to the REPL:

    -1
    You're calling add1 with (2)!
    3
    You're calling add1 with (3)!
    4
    3
    6
    
Notice how the call to `add1` inside of `foo` was not expanded properly.
While we could modify `my-modbeg.rkt` to recursively traverse each sub-form
in the program, we can also take an alternative approach. Let's put the following
inside of `my-app.rkt`:
{% highlight racket %}
;; my-app.rkt
#lang racket
(require (for-syntax syntax/parse))

(provide (except-out (all-from-out racket) #%app)
         (rename-out (my-#%app #%app)))

;; Custom #%app implementation
(define-syntax (my-#%app stx)
  (syntax-parse stx
    [(_ (~literal add1) args ...)
     (syntax/loc stx
       (begin (let ((arglist (list args ...))) ;; Evaluate the arguments
                (displayln (format "You're calling add1 with ~a!" arglist))
                (#%app apply add1 arglist))))]
    [(_ other ...) (syntax/loc stx (#%app other ...))]))
{% endhighlight %}
Then, consider the following `my-app-test.rkt`:
{% highlight racket %}
;; my-app-test.rkt
#lang s-exp "my-app.rkt"

(define (foo x)
  (add1 x))

(sub1 0)
(add1 2)
(add1 3)
(sub1 4)
(foo  5)
{% endhighlight %}
*This* will print the following to the REPL:

    -1
    You're calling add1 with (2)!
    3
    You're calling add1 with (3)!
    4
    3
    You're calling add1 with (5)!
    6
    
Which is the behavior we want. Similar interceptions can be done with
`#%datum`, but you will typically just want to delegate to Racket's version
(which just calls `quote`).
Updating our `better-cuket.rkt` file:
{% highlight racket %}
;; better-cuket.rkt
#lang racket
(require "defs.rkt")
(provide (all-from-out "defs.rkt")
         #%module-begin
         #%app    ;; We'll just use Racket's for all of these
         #%datum)
{% endhighlight %}

Then, looking at `better-cuket-test.rkt`, we find....no errors! Try updating
the file's contents to the following and running:
{% highlight racket %}
;; better-cuket-test.rkt
#lang s-exp "better-cuket.rkt"

(def (foo x)
  (plus x SEVEN))

(foo 2)
(foo 5)
{% endhighlight %}
When we run this, we get `9`, `12`, and.....oh.

### 'Requirement' 3: #%top-interaction

We are now greeted with a friendly warning message:

> Interactions disabled: "better-cuket.rkt" does not support a REPL (no #%top-interaction)

Awesome. Now, note that for this section I put "Requirement" inside of some scare quotes.
This is because, as we saw by running our program, `#%top-interaction` is not *required*.
However, you will need it if you would like a REPL. 

What does the name `#%top-interaction` even *mean*? Well, remember that mumbo-jumbo I said
earlier about modules living in a *top-level context*? It just so happens that the REPL lives
there too. In short, `#%top-interaction` provides a means of controlling all forms of interactive
evaluation, whether it be from the REPL or from functions such as `load` (i.e. by the [load handler][4]).

So, when do you mess with `#%top-interaction`? Typically when you want to mess with
the REPL. Consider the following silly example: first, we make `my-top-interaction.rkt`:
{% highlight racket %}
;; my-top-interaction.rkt
#lang racket
(require (for-syntax syntax/parse))

(provide (except-out (all-from-out racket) #%top-interaction)
         (rename-out (my-#%top-interaction #%top-interaction)))

;; Custom #%top-interaction implementation
;; Protip: Don't type a string in the REPL
(define-syntax (my-#%top-interaction stx)
  (syntax-parse stx
    [(_ . f) (syntax/loc stx (add1 f))]))
{% endhighlight %}
Then, we put the following in `my-top-interaction-test.rkt`:
{% highlight racket %}
;; my-top-interaction-test.rkt
#lang s-exp "my-top-interaction.rkt"

(define (foo x)
  (+ x 1))

(foo 2)
{% endhighlight %}
As expected, this program prints `3` when we run it. However, if we run `(foo 2)` in the REPL,
we get `4` as the result. This is because our `my-#%top-interaction` macro intercepted what we
wrote. 

We can now update our `better-cuket.rkt` file:
{% highlight racket %}
;; better-cuket.rkt
#lang racket
(require "defs.rkt")
(provide (all-from-out "defs.rkt")
         #%module-begin
         #%top-interaction
         #%app    ;; We'll just use Racket's for all of these
         #%datum)
{% endhighlight %}
And, running our `better-cuket-test.rkt` file, we get the behavior we want, REPL and all
(note that the definitions in `defs.rkt` are the only available definitions to `better-cuket-test.rkt`,
so you might consider adding some `racket/base` functions such as `-` or `*` to the `provide` form).

### Conclusion

You might notice that we are using Racket's implementations of all of these features, so you
might ask, "why not just use `(all-from-out racket/base)`?" The truth is that many times you
are better off just doing that. As I said at the beginning, the point of this was not to enhance
the language's usability, but instead to recognize what it was that the `(all-from-out racket)`
was doing to make your module usable with `#lang s-exp`. These core syntactic forms are tools
which language designers can (and should) leverage when implementing a complex language in Racket.
In addition to these, there are more core syntactic forms (see: `#lang racket/kernel`) which
one can use to assist with module processing (I always thought `#%top` was a fun one, but I am
fairly certain that there are those who will kill me for saying that).

So, what's next? We are still using `#lang s-exp` as a crutch; none of the *cool* languages have that!
We will work towards getting rid of that, but first we must talk about *the reader*. Stay tuned!


[1]: ../../../2015/05/07/racket-lang-for-idiots.html
[2]: https://docs.racket-lang.org/reference/module.html#%28form._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._~23~25module-begin%29%29
[3]: https://github.com/racket/racket/blob/7151d6d03463bcaefa84ce961db253c35c2ac547/racket/collects/racket/private/modbeg.rkt#L10
[4]: https://docs.racket-lang.org/reference/eval.html#%28tech._load._handler%29
