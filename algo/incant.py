# 
# Command-line interface to multilateration
# 2013-11-25
# 

import numpy as np
from numpy import linalg as LA

from itertools import *
from functools import partial
from math import *

from ast import literal_eval
import argparse
from sys import stdin, stdout

from charm import *

# calibrate: stations, pts, sigs -> ks
# locate: stations, ks, sigs -> (x,y)

# http://mathieularose.com/function-composition-in-python/
def compose2(f, g):
    return lambda x: f(g(x))

def compose(*functions):
    return functools.reduce(lambda f, g: lambda x: f(g(x)), functions)

# compose2(np.float64, str.split)
def parse_coords(s):
    return np.float64(map(literal_eval, s.split()))

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-c", "--coordinates", required=True, type=parse_coords
        , help="coordinates of capture stations")
    parser.add_argument("-k", "--kconstants", nargs='*', type=np.float64
        , help="antenna constants for the stations")
    parser.add_argument("--calibrate", help="calibration mode")
    args = parser.parse_args()
    
    if args.calibrate:
        print "calibration interface not yet implemented"
        return

    stations = args.coordinates
    ks = args.kconstants
    
    # Debug prints
    # print stations, ks
    
    # Main REPL
    while True:
        line = stdin.readline()
        if not line: break # len(line) is 0 when EOF
        if line == '\n': continue
        
        words = line.split()
        id, sigs = words[0], np.float64(words[1:])
        result = locate(stations, ks, sigs)
        
        print id + " " + " ".join(map(str, result[:2]))
        sys.stdout.flush()

if __name__=="__main__":
    main()