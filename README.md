Open Recursive Types Examples
=============================

This is a short example about how to use open recursive type definitions,
written up as a refresher for myself.

The examples cover:
* defining open data types
* closing open recursive data types with Fix
* conventions for constructors of these types
* abstracting out recursion (cata)
* annotations
* composing open types

The commentary is meant jog my memory more than it is to be accurate, because
the examples are about some techniques that I do not use very often, but find
very valuable when I do use them. However, if you see something blatently
wrong, let me know so that I can correct it, because the ideal situation is for
the examples and commentary to be both good at reminding me and be accurate
enough that anyone can use the examples as a reference.


Credits
-------

Other people have written about the same things, but for some reason I'm never
able to find the particular blog posts, videos, etc. that I am thinking of when
I need them. These examples are based partly on examples I saw long ago and
haven't been able to find again, and partly on refreshing my memory from:

* http://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
* http://www.haskell.org/haskellwiki/Catamorphisms

Other sets of examples and blog posts that go into the subject more deeply can
be found by searching Google for "haskell catamorphisms".
