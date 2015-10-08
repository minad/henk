# Pure Type Systems for Functional Programming

See http://www.staff.science.uu.nl/~jeuri101/MSc/jwroorda/

We present a functional programming language based on Pure Type Systems (PTSs). We show how we can define such a language by extending the PTS framework with algebraic data types, case expressions and definitions. To be able to experiment with our language we present an implementation of a type checker and an interpreter for our language.

PTSs are well suited as a basis for a functional programming language because they are at the top of a hierarchy of increasingly stronger type systems. The concepts of `existential types', `rank-n polymorphism' and  `dependent types' arise naturally in functional programming languages based on the systems in this hierarchy. There is no need for ad-hoc extensions to incorporate these features.

The type system of our language is more powerful than the Hindley-Milner system. We illustrate this fact by giving a number of meaningful programs that cannot be typed in Haskell but are typable in our language. A `real world' example of such a program is the mapping of a specialisation of a Generic Haskell function to a Haskell function.

Unlike the description of the Henk language by Simon Peyton Jones and Erik Meijer we give a complete formal definition of the type system and the operational semantics of our language. Another difference between Henk and our language is that our language is defined for a large class of Pure Type Systems instead of only for the systems of the lambda-cube.
