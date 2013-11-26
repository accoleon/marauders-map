import numpy as np

_usecols = range(1,6) # columns 1 to 5


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


# Testing code
if __name__=="__main__":
   data2 = load_data("trainingdata.txt")
   print data2