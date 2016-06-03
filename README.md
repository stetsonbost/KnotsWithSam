# KnotsWithSam

This is the repository for the research Ben Garbuz and I are doing with Professor Sam Nelson.
Currently, we are working on finding finite-type invariants of knotted surfaces in 4-space.
I am writing a program that will automate this process because the computations are somewhat tedious and can take a while to do by hand.

The main calculations are done using `FiniteType.py`, which takes the path to a file containing a linear combination of diagrams.
Here's the process for obtaining the invariants. 
Given an inner product of linear combinations of Gauss diagrams for knotted surfaces:
   1. We expand the diagrams as the sum of their subdiagrams.
   2. We compute the inner product of the linear combination. This is basically the number of subdiagrams the two linear combinations have in common.
   3. We generate the Yoshikawa submodule generators for subdiagrams with exactly _n_ crossings (which includes saddles).
   4. We use a modified version of the Gram-Schmidt process to obtain a set of orthogonal vectors, which gives us the invariant.

Initially `GaussSubiagrams.py` was used to enumerate the Gauss codes for all subdiagrams of a knotted surface, given its Gauss code.
This file is no longer used because we have significantly changed the way we are representing Gauss codes, but I used a similar algorithm to find the subdiagrams in `FiniteType.py`.
I am currently trying to integrate `Yoshikawa.pl` (Prolog) with `FiniteType.py` to facilitate the creation of the Yoshikawa submodule generators because logic programming is well-suited for this task.
