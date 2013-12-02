# 
# Wi-Fi fingerprinting using k-Nearest Neighbors Regression
# TODO: In progress.

import random
from sklearn.neighbors import KNeighborsRegressor

from loadcsv import *

def inv_dist_weights(dists):
    '''Weight for (x,y) coordinates by inverse-square distance
    
    Returns a (n,2) array when given n weights
    '''
    with np.errstate(divide='ignore'):
        ws = 1. / dists**2
    ws[np.isinf(ws)] = 1e3  # replace inf with an arbitrary large value. inf/inf = nan causes problems
    return ws  # ws.reshape(1,-1,1) # required for scikit-learn 0.13

# disabled for now
def normalize_add(sigs, target=-50):
    '''
    Adjust each signal tuple by adding a number so that the maximum signal strengths are the same
    '''
    # return sigs + (-50 - np.max(sigs, axis=sigs.ndim-1).reshape(-1,1))
    return sigs

# Testing code
if __name__=="__main__":
    data2 = load_data("../trainingdata.txt")
    pts, sigs = mean_by_coords(*data2)
    sigs = normalize_add(sigs)
    
    nei = KNeighborsRegressor(weights=inv_dist_weights)
    nei.fit(sigs, pts)

    def random_test(count = 10):
        pt_count = len(data2[0])
        for i in random.sample(xrange(pt_count), count):
            print data2[0][i], nei.predict(normalize_add(data2[1][i]))
    
    # print nei.predict(sigs[0])
    print random_test()