#!/usr/bin/env runhaskell
{- |
Module      :  Invariants.hs
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
For saddles, (which don't have arrowheads)
  x1

To compile, use Makefile (type "make invariants" at command line) or type at command line
        ghc -o invariants Invariants.hs 

To run, type at command line
        ./invariants <file_with_gauss_codes>
-}

purpose = "Given a gauss code for a knotted surface,\
      \this program will produce some (which?) of its invariants."


-- | This is the example in Sam's diagrams.pdf
test1 = [[2,1,1,2,10],[1,2,9,1,2],[-2,1,3,2,8],[-1,2,7,1,4],[-1,2,5,2,6]]

-- | TODO: find another example to play with.
--let test2 = [[]]

-- | TODO: work out our idea by hand for a small example.
-- |    This will greatly help with the implementation of this

-- | Calculates the subdiagrams as powersets
getSubdiagrams :: [[a]] -> [[a]]
getSubdiagrams [[first]] = 
  [[first]]
getSubdiagrams [[first],rest] = [[first]]

main :: IO ()
main = do
  print (purpose ++ "\nWe'll eventually take in commandline args")

--let orthogonalSet = gramScmidt input
--let subdiagrams = getSubdiagrams orthogonalSet
--putStrLn "subdiagrams:\n " ++ subdiagrams

