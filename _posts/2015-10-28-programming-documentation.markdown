---
layout: page
permalink: /school/lp2/
show_in_nav: false
title: "Programming Language Designer's Handbook: Writing Good Documentation"
---
You've done it. You have designed and created a new programming language which
is so great that everyone must hear about it and start using it. So, you take
the natural first steps of making it publicly available (either by making a
webpage or just through platforms such as GitHub) and maybe write a couple of
posts on [/r/programming][r-prog], [Hacker News][hn], or the mailing list of
your choice. It is going to take more than this for people to begin referring
to you as the next great [Benevolent Dictator for Life][bdfl], however.

One of the absolute most important things you must do if you want your programming
language to be successful is *provide good introductory documentation*.

But what makes documentation "good"?

### Introductory Documentation is not a Reference
Notice that the above states that *introductory* documentation specifically
is the most important thing to have. Is it a good idea to have a reference manual
which specifies every nitty-gritty detail of your language? Sure. You and other
people working on software which depends on the fine details of your language will
appreciate that level of detail. Is that going to be of *any use whatsoever*
to someone who knows nothing about your language? No.

You are an expert in your programming language. It is important to expect
that anyone reading your documentation is not.

As an example, let's compare a COBOL [reference manual][cobol-ref] and a COBOL
[beginners' tutorial][cobol-tut]. Consider the documentation for a basic conditional:
`if`[^1]. Here is the tutorial's documentation:

> **IF** statement checks for conditions. If a condition is true the IF block is executed; and if the condition is false, the ELSE block is executed.
>
> **END-IF** is used to end the IF block. To end the IF block, a period can be used instead of END-IF. But it is always preferable to use END-IF for multiple IF blocks.
>
> **Nested-IF** : IF blocks appearing inside another IF block. There is no limit to the depth of nested IF statements.
>
> **Syntax**
>
> Following is the syntax of IF condition statements:
>
>     IF [condition] THEN
>        [COBOL statements]
>     ELSE
>        [COBOL statements]
>     END-IF.
>
> **Examples** (omitted)

The tutorial presents the topic, explains what it does in plain English, and
even demonstrates how to use it.

Compare this to the reference's documentation on the same topic:

> The IF statement causes a condition to be evaluated (see the topic *Conditional Expressions* in the chapter *Procedure Division*). The subsequent action of the runtime element depends on whether the value of the condition is true or false.
>
> **General Format**
>
> ![Syntax Diagram][cobol-syntax-diagram]
>
> (Syntax Rules and General Rules omitted)

This explanation is great if the reader is trying to write a COBOL compiler, but, if they
are simply trying to learn how `if` statements work, this description is going to
do nothing but frustrate them. Not only does it redirect them to an overly-formal definition
of "condition," but it presents this basic construct with a complex syntax diagram.

The point I am trying to make here is that **explanations of features to beginners should strive to be no more complicated than the feature itself**.

### A Mixed Blessing: Your Readers Already Know How to Program
While it is important to understand that your readers are not experts in your language,
it is still reasonable for you to expect that they know how to program in *some* language.
Now, the *crucial* caveat here is understanding what exactly this entitles you to get
away with when authoring documentation. What it does *not* mean is that you do not have
to give examples of what a string[^2] and an integer look like in your language. What it
*does* mean is that you do not need to spend one to three paragraphs explaining
the *concepts* of strings and integers (unless they have a very unconventional meaning
in your language).

In other words, preexisting programming knowledge *only* entitles you to stay on topic. That
is to say, you still need to cover every single part (no matter how small) of your
language, but you are not required to provide deep conceptual definitions for each one of
those parts. The key to this is developing an intuition for what things in programming
comprise the "common ground" between (most) all languages. If you are positive that some
explanation falls within that commonality, then (and *only* then) you may omit it.

### Organization: Build Your Way Up
This may seem obvious to many, but it is essential that introductory documentation
follows a "from-the-ground-up" logical pattern. Why? It is the result of the following
totally-scientific theorem which I definitely did not just make up:

> **Theorem.** A programming language feature can be fully understood if *and only if* one of the following holds:
>
> 1. The feature is a piece of primitive/atomic data (e.g. strings and integers)
> 2. Every other feature which comprises that feature is already fully understood.

While I may not be versed enough in temporal logic to provide a full proof of this
on the spot, intuition tells us that this is clearly the case (almost reduntantly so).
The fact of the matter is that this is *so* obvious to veteran programmers (or really
most anyone if you tweak the wording to apply to learning in general) that it seems
foolish to say out loud. Nevertheless, by saying it aloud, it serves as a reminder to
ensure that documentation is written such that for every new topic, someone who has read
everything up to the introduction of that topic and fully understood it should be
reasonably expected to be able to understand that new topic.

Another thing to bear in mind is the resemblance of this layout to an introductory
computer science curriculum. "Wait a minute;" I can hear you say, "you just said that
we didn't need to re-teach them computer science!" This is true; I did say that, but
the difference is that now we are referring to *structure*, whereas before we were
discussing *depth*. Moreover, remember that introductory computer science classes often
aim to teach general programming concepts to students and use their language of choice
as a catalyst for achieving that goal; that is, while the language must be understood
in order to demonstrate them, the focus is on the concepts. The goal of a programming
language documentation writer is exactly the opposite: readers must understand the
general concepts of programming in order to reason about what is being written, but
those simply serve as a means to an explanation of the programming language's syntax
and behavior.

### Conclusion
It is important to remember that a programming language ultimately exists in order to be
used to communicate with a computer. Consequently, it is essential to this task that a
programming language has an appropriately accessible means of learning it. What good is
a programming language if no one (or only a combination of a select number of PhDs and
heiroglyphics experts) can use it? Even if your language is the greatest ever created,
skimping on a how-to guide will stunt its growth into obscurity.

The ultimate takeaway from all of this is to remember that language documentation is a
*conversation between computer scientists*, so you can gloss over some concepts.
Nevertheless, it still must be a *complete* tour of your language, for you cannot expect
even an expert at orienteering to find their way out of the woods without a map.

* * *
[^1]: In case a general audience finds themselves reading this: an `if`-statement is a standard construct in programming languages which effectively says `if a is true do b; otherwise, do c`.
[^2]: To the general audience reader: informally, you can thing of a [string][wiki-string] as a 'word.' Examples: `"hello"` and `"seven"` are strings, while `7` and `7.5` are not.

[r-prog]: http://reddit.com/r/programming "/r/programming"
[hn]: http://news.ycombinator.com "Hacker News"
[bdfl]: https://en.wikipedia.org/wiki/Benevolent_dictator_for_life "Wikipedia: Benevolent Dictator for Life"
[cobol-ref]: http://supportline.microfocus.com/Documentation/books/sx60/lrpubb.htm "COBOL Language"
[cobol-tut]: http://www.tutorialspoint.com/cobol/ "COBOL Tutorial"
[cobol-syntax-diagram]: http://supportline.microfocus.com/Documentation/books/sx60/lhpdf915.gif "Oh boy."
[wiki-string]: https://en.wikipedia.org/wiki/String_%28computer_science%29 "Wikipedia: String (Computer Science)"
[haskell-intro]: https://www.haskell.org/tutorial/ "A Gentle Introduction to Haskell Version 98"
[racket-guide]: http://docs.racket-lang.org/guide/ "The Racket Guide"
[python-tut]: https://docs.python.org/2/tutorial/ "The Python Tutorial"
[learn-you-hs]: http://learnyouahaskell.com/chapters "Learn You a Haskell for Great Good!"
