'''
Wi-Fi fingerprinting using k-Nearest Neighbors Regression
'''

import random
from sklearn.neighbors import KNeighborsRegressor
from sklearn import grid_search

from loadcsv import *

# ===============
# = Locator API =
# ===============

def create_locator(datafile):
    '''Create a ready-made locator with tuned defaults
    
    Args: datafile: training data CSV file
    '''
    data = load_data(datafile)
    pts, sigs = mean_by_coords(*data)
    # sigs = normalize_add(sigs)
    
    locator = KNeighborsRegressor(algorithm='kd_tree', weights=inv_weights, n_neighbors=3)
    locator.fit(sigs, pts)
    return locator

def locate(locator, sigs):
    '''Estimate the source location given the measured signals strengths
    
    Args:
        locator: a locator created using ``create_locator``
        sigs: array of signal strengths
    '''
    est = locator.predict(sigs) # normalize_add(sigs))
    return est.flatten()
    
# ======================
# = kNN configurations =
# ======================

def invsq_weights(dists):
    '''Weight for (x,y) coordinates by inverse-square distance
    
    Returns a (n,2) array when given n weights
    '''
    with np.errstate(divide='ignore'):
        ws = 1. / dists**2
    ws[np.isinf(ws)] = 1e3  # replace inf with an arbitrary large value. inf/inf = nan causes problems
    return ws  # ws.reshape(1,-1,1) # required for scikit-learn 0.13

def inv_weights(dists):
    '''Weight for (x,y) coordinates by inverse distance
    
    Returns a (n,2) array when given n weights
    '''
    with np.errstate(divide='ignore'):
        ws = 1. / dists
    ws[np.isinf(ws)] = 1e3  # replace inf with an arbitrary large value. inf/inf = nan causes problems
    return ws  # ws.reshape(1,-1,1) # required for scikit-learn 0.13

# disabled for now
def normalize_add(sigs, target=-50):
    '''
    Adjust each signal tuple by adding a number so that the maximum signal strengths are the same
    '''
    return sigs + (target - np.max(sigs, axis=sigs.ndim-1).reshape(-1,1))


# Testing code
if __name__=="__main__":
    data2 = load_data("../trainingdata.txt")
    pts, sigs = mean_by_coords(*data2)
    sigs = normalize_add(sigs)
    
    nei = KNeighborsRegressor(weights=invsq_weights, algorithm='kd_tree')
    nei.fit(sigs, pts)

    def random_test(count = 10):
        pt_count = len(data2[0])
        for i in random.sample(xrange(pt_count), count):
            print data2[0][i], nei.predict(normalize_add(data2[1][i]))
    
    # print nei.predict(sigs[0])
    print random_test()
    
    def tune():
        '''Tune kNN regressor using grid-search CV'''
        param_grid = {'n_neighbors':range(1,7), 'weights':[invsq_weights, inv_weights] }
        clf = grid_search.GridSearchCV(nei, param_grid, n_jobs=2)
        clf.fit(sigs, pts)
        return clf