#!/usr/bin/env runhaskell
{- |
Module      :  <File name or $Header$ to be replaced automatically>
Description :  Produces finite type invariants of knotted surfaces
Authors     :  Stetson Bost, Ben Garbuz
Date        :  2016.05.20

This program will take the Gauss diagram for an oriented knotted surface as a
5-tuple [x0,x1,x2,x3,x4], specified as follows:
  x0 =
         1 positive crossing (classical)
        -1 negative crossing (classical)
         2 vertical saddle
        -2 horizontal saddle
For classical crossings,
  x1 = component number of the arrowhead
  x2 = numeric label of the arrowtail
  x3 = component number of the arrowtail
  x4 = numeric label of the arrowhead
For saddles, (which don't have arrow heads)
  x1

To compile, use Makefile (type "make" at command line) or type at command line
        ghc -o invariants Invariants.hs 

To run, type at command line
        ./invariants <file_with_gauss_codes>
-}

-- | TODO: EVERYTHING


purpose = "Given a gauss code for a knotted surface,\
      \this program will produce some (which?) of its invariants."

main = putStrLn purpose