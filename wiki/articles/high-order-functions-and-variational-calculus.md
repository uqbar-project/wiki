---
layout: article
title: Orden superior y calculo variacional (Referencia Externa)
---

# Introduction

Shamessly taken from http://web.archive.org/web/20070209204638/http://ergodicity.iamganesh.com/2006/08/07/higher-order-functions/

# Higher order functions

Joel Spolsky, of the JoelOnSoftware fame, recently wrote about higher order functions in “modern” languages. While these features have existed for decades in functional programming languages, they have gained a lot of attention lately due to Google’s MapReduce technology.

Simply put, map and reduce are primitives in languages such as lisp. Thinking in terms of lists (naturally), map lets you apply a function to every element of a list, while reduce lets you combine values of the list in some way. Pick up any lisp book for more details on this programming paradigm.

I find a great deal of similarity between functionals in programming, and functionals in variational calculus. Unfortunately, variational calculus is a much neglected field of calculus specially at the undergraduate level. Variational calculus lets you define “functions of functions.” For example, the functional defined for the continuous function y(t) in the region [a,b]:

{% link_image hofunctions1.jpg %}

is the area under the y(t) curve. See how J is completely independent of y(t)? That’s the power of variational calculus.

In general, if we have functions of the form:

{% link_image hofunctions2.jpg %}

we can use the [Euler-Lagrange](https://en.wikipedia.org/wiki/Euler%E2%80%93Lagrange_equation) equations to derive a differential equation to solve the problem.

Variational calculus finds much use in diverse fields such as aerospace control (optimize a trajectory based on fuel cost), operations research (optimize FedEx’s drop off targets based on distance travelled), and image inpainting (total variational models). If you haven’t figured it out yet, “optimization” is the reason this entry is posted under Numerical.