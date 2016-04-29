#!/usr/bin/python

"""
    GaussSubdiagrams.py

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

    IDEA: Use complex numbers for saddles

"""

def printSubdiagrams(gaussCode):
    """
        Prints all the subdiagrams of a given diagram given a valid Gauss code.
        Raises and error if the Gauss code is not valid.
    """

    # This will throw and error if gaussCode is not valid
    isValidGaussCode(gaussCode)
    print "Valid Gauss Code!"
    # TODO: Calculate subdiagram codes
    # TODO: Print/return subdiagram codes


def isValidGaussCode(gaussCode):
    """
        Throw an error if input is an invalid Gauss code.
    """
    # TODO: For invalid gaussCode, print the gaussCode(?) and the error
    if (type(gaussCode) is not list         # Make sure gaussCode is a list
        or not(len(gaussCode) > 0)         # Make sure gaussCode is nonempty
        or type(gaussCode[0]) is not list   # Make sure the first element of 
                                            #   gaussCode is a list
        ):
        raise ValueError('Gauss code is invalid')

    # Make sure each array of numbers sums to 0
    for code in gaussCode:
        sum = 0
        # Sum over all element of i
        for i in code:
            # Elements of code must be numbers
            if type(i) is not (int or float):
                raise ValueError('Gauss code is invalid')
            sum += i
        if sum != 0:
            raise ValueError('Gauss code is invalid')

    # TODO: Make sure each crossing occurs exactly twice (under/over, saddle)

    # TODO: For invalid gaussCode, print the gaussCode


def main():
    """
        Asks user to input a Gauss code.
        Prints the subdiagrams of the knotted surface.
    """
    # TODO: Get user input either as a Gauss code or path to file with Gauss
    #       codes.
    # TODO: Parse input

    gaussCode = [[1,2,3,-5]]
    print gaussCode

    # TODO: Handle Error for invalid Gauss code
    # TODO: Ask user for new input while input is invalid
    printSubdiagrams(gaussCode)

if __name__ == '__main__':
    main()