#!/usr/bin/python
"""
    Module      :  FiniteType.py
    Description :  Produces finite type invariants of knotted surfaces
    Authors     :  Stetson Bost

    ===========================================================================
    To run this code on a test file containing a linear combination:
        python FiniteType.py test.txt
    ===========================================================================

    This program will take a linear combination of Gauss diagrams.
    We express this linear combination 
          n1*G1 + n2*G2 + ...
    where n1, n2, ... are integer coefficients and G1, G2, ... are Gauss 
    diagrams, in the form
          [(n1,G1,C1), (n2,G2,C2), ...]
    where C1, C2, ... are the components of the Gauss diagram corresponding to
    G1, G2,..., respectively. We represent a Gauss diagram G for oriented
    knotted surfaces as a list of its crossings in the form of 5-tuples. First,
    label the circles (usually numbers or characters). Then number the arrow/
    chord endpoints in order. The 5-tuples (x0,x1,x2,x3,x4) are specified as
    follows:
        x0 =
               1 positive crossing
              -1 negative crossing
               0 saddle

      For classical crossings,
        x1 = component number of x2
        x2 = numeric label of the arrowtail
        x3 = component number of x4
        x4 = numeric label of the arrowhead

      For saddles, (which don't have arrowheads)
        x1 = component number of x2
        x2 = lower of the saddle's numeric labels
        x3 = component number of x4
        x4 = higher of the saddle's numeric labels
      Note: For saddles, we choose x2 < x4 simply to avoid ambiguity.

    So we have a Gauss diagram represented as
          G = [(x0,x1,x2,x3,x4), (y0,y1,y2,y3,y4), ...].

    Now that we have a labelling for the components of the diagram, we can
    construct a dictionary C representing the components of G. Suppose G has
    components labelled 'A', 'B', ..., and the components have crossings a1,
    a2, ..., am for 'A', then b1, b2, ..., bn for 'B', and so on. Then we can
    represent the components as
          C = {'A':[a1,a2,...,am], 'B':[b1,b2,...,bn], ...}.
"""

import sys  # used in main to read command line arguments
import ast  # used in main to convert string to list
from copy import deepcopy # used in calculateSubdiagrams
from subprocess import call # used to execute terminal commands

def calculateSubdiagrams(subdiagrams, diagram, components):
  """
  Returns set of subdiagrams whose sum is diagram
  """
  
  for piece in components.items():
    piece = tuple(piece[1])

  componentList = []
  for comp in components.items():
    # create a list whose first element is the component name
    piece = [comp[0]]
    # append the list of crossings
    piece.append(tuple(comp[1]))
    componentList.append(tuple(piece))

  # Add diagram (as tuple) to set of subdiagrams with components
  subdiagrams.add((tuple(diagram), tuple(componentList)))

  # Go through the crossings of the diagram
  for crossing in diagram:
    # Make a deep copy of the diagram and components
    copyDiagram = deepcopy(diagram)
    copyComponents = deepcopy(components)
    # Remove a crossing from the code and components
    copyDiagram.remove(crossing)
    copyComponents[crossing[1]].remove(crossing[2])
    copyComponents[crossing[3]].remove(crossing[4])
    # Recursively find remaining subdiagrams
    subdiagram = calculateSubdiagrams(subdiagrams, copyDiagram, copyComponents)
    # Add new subdiagrams to the set of all subdiagrams
    subdiagrams.union(subdiagram)

  return subdiagrams

def obtainLinearCombination(linearComboString):
  # TODO
  print "TODO"
  return None

def innerProd(linearCombo1, linearCombo2):
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

  # copy linear combinations to make it easier to alter them as needed
  linearComboCopy1 = deepcopy(linearCombo1)
  linearComboCopy2 = deepcopy(linearCombo2)
  print len(linearComboCopy2[0][1])

  # get list of subdiagrams, get coefficients
  for coeffAndSubdiagrams1 in linearComboCopy1:
    subdiagramSet1 = coeffAndSubdiagrams1[1]
    coeff1 = coeffAndSubdiagrams1[0]
    subdiagramsToRemove1 = []

    for coeffAndSubdiagrams2 in linearComboCopy2:
      subdiagramSet2 = coeffAndSubdiagrams2[1]
      coeff2 = coeffAndSubdiagrams2[0]
      subdiagramsToRemove2 = []

      # look for equivalent pairs of dashed subdiagrams in both lists
      for subdiagram1 in subdiagramSet1:
        for subdiagram2 in subdiagramSet2:          
          # for equivalent pairs of dashed subdiagrams, increment innerProd by 
          #   product of coefficients and remove the subdiagrams to avoid 
          #   over-counting diagrams
          if equiv(subdiagram1, subdiagram2):
            innerProd += coeff1 * coeff2
            subdiagramsToRemove1.append(subdiagram1)
            subdiagramsToRemove2.append(subdiagram2)
          print "  ", innerProd
        # remove the used subdiagrams
        # print "sub 2\t" + str(subdiagramSet2)
        # print " 2 " + str(subdiagramsToRemove2)
        # print " 1 " + str(subdiagramsToRemove1)
        for removal2 in subdiagramsToRemove2:
          # print "  remove " + str(removal2)
          subdiagramSet2.remove(removal2)
        subdiagramsToRemove2 = []
      # for removal1 in subdiagramsToRemove1:
      #   subdiagramSet1.remove(removal1) 
  return innerProd

def getNext(crossing):
  """
  Given a crossing, returns the next crossing
  """
  return

def getPrev(crossing):
  """
  Given a crossing, returns the previous crossing
  """
  return

def equiv(subdiagram1, subdiagram2):
  """
  Returns True if both dashed subdiagrams are equivalent
    i.e. (0,'A',1,'A',3) is equivalent to (0,'X',2,'X',3),
  Returns False otherwise

  Arguments must be a tuple of crossings representing a single subdiagram
  """
  # TODO: Make sure arguments are the right type
  # TODO: Make this work for subdiagrams of length >= 1
  # subdiagrams are not equivalent if they have different numbers of crossings
  # print "sub1\t", subdiagram1, len(subdiagram1[0])
  # print "sub2\t", subdiagram2, len(subdiagram2[0])
  if len(subdiagram1[0]) != len(subdiagram2[0]):
    return False
  # look for a match
  for i in range(len(subdiagram1[0])-1):
    crossing1 = subdiagram1[0][i]
    typeMatch = False
    for j in range(len(subdiagram2[0])-1):
      crossing2 = subdiagram2[0][j]
      print "\tc1 ",crossing1
      print "\tc2 ",crossing2
      # check for same crossing type
      # TODO: check for empty crossing
      if len(crossing1) == 5 and len(crossing2) == 5:
        if crossing1[0] == crossing2[0]:
          print  "              :)"
          typeMatch = True
        

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
  #   This should be a list of tuples (a,D,C) where
  #   a is an integer coefficient and
  #   D is the diagram (as a list of 5-tuples) and
  #   C is the dict of components with an ordered list of their crossings
  linearComboString = open(sys.argv[1]).read()
  linearCombo = ast.literal_eval(linearComboString) 
  # linearCombo is list of tuples of form 
  #    (integer, list of 5-tuples, dictionary of components)

  # TODO: Check that input file is valid, meaning it is in the format specified
  #   above and doesn't have duplicate crossings

  # Expand each diagram (solid arrows/chords) as sum of its
  #   subdiagrams (dashed arrows/chords).
  #   Subdiagrams sum will be Set or list of 5-tuples.
  # Rewrite the linear combination using the subdiagrams.

  linComboSubdiagrams = []
  for diagram in linearCombo:
    subdiagrams = calculateSubdiagrams(set(), diagram[1], diagram[2])
    linComboSubdiagrams.append((diagram[0], subdiagrams))
  # linComboSubdiagrams is a
  #   List of tuples            (<-- linear combinations)
  #     of integer and set      (<-- integer = coefficient, set = subdiagrams)
  #       of tuples             (<-- tuple = dashed subdiagrams)
  #         of tuples           (<-- tuple = crossing)
  #           of the form explained above (5-tuples)
  # print linComboSubdiagrams

  # TODO: Pick sets of subdiagrams
  # TODO: Compute inner product of 2 diagrams using their sums
  #   of subdiagrams.
  # innerProdTest = innerProd([linComboSubdiagrams[0]], [linComboSubdiagrams[1]])
  # print innerProdTest, "\n"

  # testing equiv

  sub1 =  (((0, 'A', 1, 'B', 10), (0, 'A', 3, 'B', 8), (-1, 'B', 7, 'A', 4)), (('A', (1, 3, 4)), ('B', (7, 8, 10))))
  sub2 =  (((0, 'A', 1, 'B', 10), (1, 'B', 9, 'A', 2), (0, 'A', 3, 'B', 8)),  (('A', (1, 2, 3)), ('B', (8, 9, 10))))
  sub3 =  (((0, 'A', 1, 'B', 10), (0, 'A', 3, 'B', 8), (1, 'B', 9, 'A', 2)),  (('A', (1, 2, 3)), ('B', (8, 9, 10))))
  sub4 =  ((), (('A', ()), ('B', ())))
  sub5 =  ((), (('C', ()), ('D', ())))
  sub6 =  (((0, 'A', 1, 'B', 10), (0, 'A', 3, 'B', 8), (-1, 'B', 7, 'A', 4)), (('A', (1, 3, 4)), ('B', (7, 8, 10))))

  print equiv(sub1,sub1) # true
  print equiv(sub1,sub2) # false
  print equiv(sub2,sub3) # true
  print equiv(sub1,sub4) # false
  print equiv(sub4,sub5) # true

  # TODO: Generate Yoshikawa submodule generators for n arrows
  #   sys.argv[2] is the number of arrows/chords
  # TODO: adjust "if len(sys.argv) != 2:" to take 3rd argument needed
  # generators = yoshikawa(innerProd, sys.argv[2])

  # TODO: Use Gram-Schmidt to get invariant from Yoshikawa generators
  # invariants = gramSchmidt(generators)

if __name__ == '__main__':
  main()