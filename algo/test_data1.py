import numpy as np
from numpy import linalg as LA

from itertools import *
from functools import partial
from math import *

import random
import sys

from charm import *
from data1 import data1


# data1 imported from data1.py
data1_solved = solve_calibrate(data1[0], data1[1], data1[2])
data1_ks = data1_solved[0][:3] # antenna constants for data1

def locate_data1(sigs):
    return locate(data1[0], data1_ks, sigs)

# def est_sig_data1(x, y, t):
#     return est_recv_sig((data1[0], data1_ks), x, y, t)


def main():
    sample_count = 10
    pt_count = len(data1[1])
    for i in random.sample(xrange(pt_count), sample_count):
        print data1[1][i], locate_data1(data1[2][i])

if __name__=="__main__":
   main()