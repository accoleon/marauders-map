import numpy as np

from pandas import Series, DataFrame
import pandas as pd

# default column indices: 1 to 5
# (0 is timestamp; 1,2 are x and y; 3,4,5 are three stations)
_usecols = range(1,6)


# http://agiliq.com/blog/2012/06/understanding-args-and-kwargs/
def load_csv_(filename, delimiter=',', **kwargs):
    """Generic CSV loading, used by load_data"""
    return np.loadtxt(filename, delimiter=delimiter, unpack=True, **kwargs)

def load_data(filename, usecols=_usecols, **kwargs):
    """Loads a CSV training data file
    and returns two ordered lists of coordinates and signal strengths.
    
    The first two columns are taken as coordinates, the rest as signal strengths.
    """
    n = len(usecols) - 2 # number of signal strengths (stations)
    return load_csv_(filename, usecols=usecols, dtype='2float64, {0}float64'.format(n), **kwargs)


def agg_by_coords(pts, sigs, aggfunc='mean'):
    df = DataFrame(np.hstack((pts, sigs))).groupby([0,1]).agg(aggfunc).reset_index().values
    return np.split(df, [2], axis=1)

def mean_by_coords(pts, sigs):
    '''Take the mean signal strengths for each coordinate
    
    Returns [pts, sigs] where pts contains unique coordinates and sigs contains averages
    '''
    # df = DataFrame(np.hstack((pts, sigs))).groupby([0,1]).mean().reset_index().values
    # return np.split(df, [2], axis=1)
    return agg_by_coords(pts, sigs, aggfunc='mean')

def median_by_coords(pts, sigs):
    return agg_by_coords(pts, sigs, aggfunc='median')

# Testing code
if __name__=="__main__":
    data2 = load_data("../trainingdata.txt")
    print data2