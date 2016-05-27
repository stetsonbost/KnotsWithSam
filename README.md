# KnotsWithSam

This is the repository for the research Ben Garbuz and I are doing with Professor Sam Nelson.
Currently, we are working on finding finite-type invariants of knotted surfaces in 4-space.
I am writing a program that will automate this process because the computations are somewhat tedious and can take a while to do by hand.

The main calculations are done using `FiniteType.py`, which takes the path to a file containing a linear combination of diagrams.

Initially `GaussSubiagrams.py` was used to enumerate the Gauss codes for all subdiagrams of a knotted surface, given its Gauss code.
This file is no longer used because we have significantly changed the way we are representing Gauss codes, but I used a similar algorithm to find the subdiagrams in `FiniteType.py`.
