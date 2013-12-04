'''
Command-line interface to Wi-Fi fingerprinting (k-Nearest Neighbors)
'''

from itertools import *
import argparse
import textwrap
from sys import stdin, stdout
import numpy as np

from loadcsv import *
from knn import *

def main():
    description = textwrap.dedent('''\
    Locate using Wi-Fi fingerprinting
    
    Usage
    -----
    This program reads lines from stdin that consist of
        id sig1 sig2 sig3
    where id is an arbitrary ID string without spaces,
    and sig is the signal strength at each station.
    Using k-nearest neighbors on the datafile, it locates the best match and prints
        id x y
    where (x,y) are the estimated coordinates.
    
    Datafile
    --------
    The training data file should be a CSV file containing lines of
        arbitrary string,x,y,sig1,sig2,sig3
    
    Arguments
    ---------''')
    usage = '%(prog)s [--help] datafile'
    parser = argparse.ArgumentParser(
        description=description, formatter_class=argparse.RawDescriptionHelpFormatter, usage=usage)
    parser.add_argument("datafile", type=argparse.FileType('r')
        , help="training data CSV file")
    args = parser.parse_args()
    
    locator = create_locator(args.datafile)
    
    # Main REPL
    while True:
        line = stdin.readline()
        if not line: break # len(line) is 0 when EOF
        if line == '\n': continue
        
        words = line.split()
        ident, sigs = words[0], np.float64(words[1:])
        result = locate(locator, sigs)
        
        print ident + ' ' + ' '.join(map(str, result))
        stdout.flush()

if __name__=="__main__":
    main()