---
title: A place for another SQL DSL
author: Lukwago Allan
tags: Haskell, SQL, DSL, library
description: Introduction to a type-safe non-TH Haskell SQL DSL inspired by Persistent-Esqueleto
useShortName: yes
---

## Intro

"Haskell SQL libraries: endless boilerplate, opaque meta-programming, or wall of 15 language extensions and no inference. Pick two" Stephen Diehl.

"If somebody implements something better than Esqueleto I will happily move over, but AFAICT most of the people implementing the newer SQL EDSL libraries hadn't used or learned Esqueleto before they set out to write a new library."  Haskell-Redit user.

"There are already to many haskell SQL DSL libraries" Haskell-Twitter user.

Early on while starting out in Haskell, I discovered Persistent and then subsequently Esqueleto. For the uninitiated, Persistent and Esqueleto are haskell SQL DSL libraries. Esqueleto builds on top of Persistent and offers better ergonomics and a batch of new features.

I like the approach  Esqueleto took in its SQL DSL development. It took the SQL we all know and like and re-packaged it into a safe typed DSL. In a way, if you know SQL you already know Esqueleto. But....! there is always a but.

* I don't like the fact that Esqueleto forces you to architecture your application in a certain away.

* I also hate that we have to use Template Haskell which typically introduces a substantial compilation time cost in any medium to large application.

Squid is an attempt to have all the advantages of Esqueleto minus the costs through leveraging new modern Haskell features. The only cost is that you have to suffer a list of exotic language extensions but not 15 more like 5 :-). I promise . Squid also aims to maintain good type inference.

The quotes above can be found on this [Reddit post](https://www.reddit.com/r/haskell/comments/8qxvir/a_comparison_among_various_database_edsls_selda/)

See library source code

* [epicallan/squid](@github)

## Motivation

## Implementation

## Squid and Persistent Esqueleto

## Squid and the rest

https://tathougies.github.io/beam/about/faq/#how-does-beam-compare-with-x

## Usage Example

## Conclusion

## Acknowledgment
