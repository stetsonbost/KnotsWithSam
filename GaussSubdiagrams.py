#!/usr/bin/python

"""
	GaussSubdiagrams.py

	===========================================================================

	This program will enumerate the subdiagrams of a given Gauss diagram for a knotted surface. Currently, only oriented knotted surfaces are considered valid. These diagrams will be specified by their Gauss code in the form of a tuple. 
	The Gauss codes are represented as follows:
		* Each crossing is numbered.
		* Classical crossings have either positve or negative writhe.
		* From an arbitrary point on a marked vertex diagram, we traverse the surface, passing either under or over classical crossings, and through saddles in a source-sink manner.
		* Each pass through a crossing in the Gauss code corresponds to a number in a tuple.
		* Under-crossings correspond to negative numbers, over-crossings to positive numbers.
		* A crossings is given its labeled integer value in the tuple, but is augmented by 0.5 if it has a negative writhe.
	For example, the Gauss code 
			(u1-,o2-,u4+,o3+,u2-,o1-,u3+,o4+)
	corresponds to the tuple
			[[-1.5,2.5,-4,3,-2.5,1.5,-3,4]].

"""


def main():
      print "This may eventually ask that the user provide a valid Gauss code."
if __name__ == '__main__':
    main()