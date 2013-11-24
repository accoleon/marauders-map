import numpy as np

# In progress

# http://agiliq.com/blog/2012/06/understanding-args-and-kwargs/
def load_csv(filename, delimiter=',', **kwargs):
    np.loadtxt(filename, delimiter=delimiter, unpack=true, **kwargs)


def load_data(filename, **kwargs):
    cols = range(1,6) # cols 1 to 5
    return load_csv(filename, usecols=cols, dtype='2float64, 3float64', **kwargs)