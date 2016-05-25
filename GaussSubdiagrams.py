#!/usr/bin/python

"""
    Stetson Bost, author
    Sam Nelson, advisor

    ===========================================================================

    This program will enumerate the subdiagrams of a given Gauss diagram for a 
        knotted surface. Currently, only oriented knotted surfaces are 
        considered valid. These diagrams will be specified by their Gauss code 
        in the form of a tuple. 
    The Gauss codes are represented as follows:
        * Each crossing is numbered.
        * Classical crossings have either positve or negative writhe.
        * From an arbitrary point on a marked vertex diagram, we traverse the 
            surface, passing either under or over classical crossings, and 
            through saddles in a source-sink manner.
        * Each pass through a crossing in the Gauss code corresponds to a 
            number in a tuple.
        * Under-crossings correspond to negative numbers, over-crossings to 
            positive numbers.
        * A crossings is given its labeled integer value in the tuple, but is 
            augmented by 0.5 if it has a negative writhe.
    For example, the Gauss code 
            (u1-,o2-,u4+,o3+,u2-,o1-,u3+,o4+)
    corresponds to the tuple
            [[-1.5,2.5,-4,3,-2.5,1.5,-3,4]].

    IDEA: Use complex numbers for saddles?

"""

import sys                  # used in main to read command line arguments
import ast                  # used in main to convert string to list
from copy import deepcopy   # used in calculateSubdiagrams

def printSubdiagrams(gaussCode):
    """
        Prints all the subdiagrams of a given diagram given a valid Gauss code.
        Raises and error if the Gauss code is not valid.
    """
    # This will throw and error if gaussCode is not valid
    # TODO: UNCOMMENT THE FOLLOWING AFTER FIXING isValidGaussCode
    # isValidGaussCode(gaussCode)
    print "Valid Gauss Code!"
    # TODO: Calculate subdiagram codes
    setSubdiagrams = calculateSubdiagrams(gaussCode)
    print setSubdiagrams
    # TODO: Order the Set
    # TODO: Change Set into list?
    # TODO: Print subdiagram codes to a file
    
def calculateSubdiagrams(gaussCode):
    """
        Determines tuples corresponding to subdiagrams. Uses recursion.
        Returns a Set of the subdiagrams as tuples.
        gaussCode must be a list of (comlplex?) numbers
    """
    # Create a set of subdiagrams
    subdiagrams = set()
    # Create set of subdiagrams containing only the gaussCode
    gaussTuple = tuple(gaussCode)
    subdiagrams.add(gaussTuple)

    # TODO: Find way to go through list and not delete all elemets twice 
    for item in gaussCode:
        # Make a copy of the original gaussCode
        copyCode = deepcopy(gaussCode)
        # Remove a pair of corresponding elements in the code
        copyCode.remove(item)
        copyCode.remove(-item)
        # Add new subdiagrams to set of all subdiagrams
        subdiagram = calculateSubdiagrams(copyCode)
        subdiagrams = subdiagrams | subdiagram

    # print "after ", subdiagrams
    return subdiagrams

    



def isValidGaussCode(gaussCode):
    """
        Throw an error if input is an invalid Gauss code.
    """
    # TODO: FIX THIS TO WORK WITH NEW GAUSSCODE FORMAT
    if (type(gaussCode) is not list         # Make sure gaussCode is a list
        or not(len(gaussCode) > 0)          # Make sure gaussCode is nonempty
        or type(gaussCode[0]) is not list   # Make sure the first element of 
                                            #   gaussCode is a list
        ):
        raise ValueError('Gauss code is invalid')

    # Make sure each array of numbers sums to 0
    # TODO: is this necessary for each Gauss code?
    #       OR should the entire set of Gauss codes sum to 0?
    for code in gaussCode:
        dictionary = {} # key is element of Gauss code, value is boolean
        sumTotal = 0
        # Sum over all element of i
        for i in code:
            # Elements of code must be numbers
            if type(i) is (not int and not float and not complex):
                raise ValueError('Gauss code is invalid: ' + 
                    'contains an element (' + str(i) + ') that is not a number')

            # This section makes sure positive and negative elements of the 
            #   Gauss code always come in pairs

            # if abs(i) not in dictionary, add it to dictionary
            if not dictionary.has_key(abs(i)):
                dictionary[abs(i)] = i > 0
            # if abs(i) in dictionary
            else:
                # if there is a corresponding pair of elements,
                #   then delete the key
                if (i > 0) != dictionary[abs(i)] :
                    del dictionary[abs(i)]
                # else, there were two identical elements in the gauss code.
                #   which is not valid
                else:
                    raise ValueError('Gauss code is invalid: ' +
                        '2 identical elements in Gauss code')

            sumTotal += i
        if dictionary != {}:
            raise ValueError('Gauss code is invalid:' +
                'not all elements have a corresponding element')
        if sumTotal != 0:
            raise ValueError('Gauss code is invalid')

    # TODO: Make sure each crossing occurs exactly twice (under/over, saddle)

def main():
    """
        Accepts command line argument for Gauss code list.
        Asks user to input a Gauss code.
        Prints the subdiagrams of the knotted surface.
    """
    # TODO: Accept list of lists of Gauss codes

    # TODO: Get user input as a path to file with Gauss code(s)
    # TODO: Read input file
    # for gaussCodeString in sys.argv[1:]:
    #     print gaussCodeString, type(gaussCodeString)
    #     # Turn gaussCodeString into a list
    #     gaussCode = ast.literal_eval(gaussCodeString)
    #     print gaussCode, type(gaussCode)

        # TODO: Handle Error for invalid Gauss code
        # TODO: Ask user for new input while input is invalid
        # printSubdiagrams(gaussCode)

    printSubdiagrams([])
    printSubdiagrams([1,-1])
    printSubdiagrams([1,2.5,-2.5,-1])
    printSubdiagrams([1,2.5,-1,-2.5])
    printSubdiagrams([1,2.5,-3,3,-1,-2.5])

    # TODO: Write subdiagrams to file Subdiagrams.txt

if __name__ == '__main__':
    main()
