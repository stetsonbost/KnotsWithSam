#!/usr/bin/python
"""
    Module      :  FiniteType.py
    Description :  Produces finite type invariants of knotted surfaces
    Authors     :  Stetson Bost, Ben Garbuz

    ===========================================================================
    To run this code on a test file containing a linear combination:
        python FiniteType.py test.txt
    ===========================================================================

    This program will take a linear combination of Gauss diagrams for oriented
    knotted surfaces as a 5-tuple (list). First, label the circles (numbers or
    characters). Then number the arrow/chord endpoints in order. The 5-tuples
    [x0,x1,x2,x3,x4] are specified as follows:
        x0 =
               1 positive crossing
              -1 negative crossing
               0 saddle

      For classical crossings,
        x1 = component number of the arrowhead
        x2 = numeric label of the arrowtail
        x3 = component number of the arrowtail
        x4 = numeric label of the arrowhead

      For saddles, (which don't have arrowheads)
        x1 = component number of the lower of the numeric labels
        x2 = lower of the saddle's numeric labels
        x3 = component number of the higher of the numeric labels
        x4 = higher of the saddle's numeric labels
      Note: For saddles, we choose x2 < x4 simply to avoid ambiguity.
"""
import sys  # used in main to read command line arguments
import ast  # used in main to convert string to list
from copy import deepcopy # used in calculateSubdiagrams

def calculateSubdiagrams(subdiagrams, diagram):
  """
  Returns set of subdiagrams whose sum is diagram
  """
  # Add diagram (as tuple) to set of subdiagrams 
  subdiagrams.add(tuple(diagram))

  # Go through the crossings of the diagram
  for crossing in diagram:
    # Make a deep copy of the diagram
    copyDiagram = deepcopy(diagram)
    # Remove a crossing from the code
    copyDiagram.remove(crossing)
    # Recursively find remaining subdiagrams
    subdiagram = calculateSubdiagrams(subdiagrams, copyDiagram)
    # Add new subdiagrams to the set of all subdiagrams
    subdiagrams.union(subdiagram)

  return subdiagrams

def obtainLinearCombination(linearComboString):
  # TODO
  print "TODO"
  return None

def innerProduct(linearCombo1, linearCombo2):
  """
  Returns the inner product of 2 linear combinations of subdiagrams.
  The arguments linearCombo1 and linearCombo2 must be linear combinatons 
    of sets of subdiagrams
  """
  innerProd = 0
  # TODO: I'm starting with simple sets on both sides, not sum of sets, 
  #   with coefficient 1. We need to fix this later
  # TODO: find way to represent dashed vs undashed arrows
  #   For now, assume both are already sums (sets) of dashed subdiagrams

  # TODO: copy and remove elements as we go through them
  for subdiagram1 in linearCombo1:
    for subdiagram2 in linearCombo2:
      # for equivalent elements, increment innerProd
      if equiv(subdiagram1, subdiagram2):
        innerProd += 1



  return innerProd

def equiv(subdiagram1, subdiagram2):
  """
  Returns True if both dashed subdiagrams are equivalent
    i.e. (0,A,1,A,3) is equivalent to (0,X,2,X,3)
  Returns False otherwise
  """
  # TODO
  print "TODO"
  if len(subdiagram1) != len(subdiagram2):
    return False

  return True

def yoshikawa(innerProduct, n):
  # TODO
  print "TODO"
  return None

def gramSchmidt(innerProduct, n):
  # TODO
  print "TODO"
  return None

def main():
  # if len(sys.argv) != 3:
  #   raise ValueError("Invalid number of arguments.",
  #     "FiniteType.py requires 2 arguments.")

  if len(sys.argv) != 2:
    raise ValueError("Invalid number of arguments.",
      "FiniteType.py requires 1 argument.")

  # Read input file with linear combination of diagrams
  #   This should be a set/list/dictionary of tuples (a,D) where
  #   D is the diagram and a is an integer coefficient
  linearComboString = open(sys.argv[1]).read()
  linearCombo = ast.literal_eval(linearComboString) 
  # linearCombo is list of tuples of form (int, list of 5-tuples of ints)

  # TODO: Check that input file is valid

  # Expand each diagram (solid arrows/chords) as sum of its
  #   subdiagrams (dashed arrows/chords).
  #   Subdiagrams sum will be Set or list of 5-tuples.
  # Rewrite the linear combination using the subdiagrams.

  linearComboSubdiagrams = []
  for diagram in linearCombo:
    subdiagrams = calculateSubdiagrams(set(), diagram[1])
    linearComboSubdiagrams.append((diagram[0], subdiagrams))

  print linearComboSubdiagrams

  # TODO: Compute inner product of 2 diagrams using their sums
  #   of subdiagrams.
  # innerProd = innerProducts(linearCombo1, linearCombo2)

  # TODO: Generate Yoshikawa submodule generators for n arrows
  #   sys.argv[2] is the number of arrows/chords
  # TODO: adjust "if len(sys.argv) != 2:" to take 3rd argument needed
  # generators = yoshikawa(innerProd, sys.argv[2])

  # TODO: Use Gram-Schmidt to get invariant from Yoshikawa generators
  # invariants = gramSchmidt(generators)

if __name__ == '__main__':
  main()