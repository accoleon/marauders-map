#
# Trilateration algorithm
#

import numpy as np
from numpy import linalg as LA

# Quickref for Python:
# For Python coding style guidelines, see PEP 8: http://www.python.org/dev/peps/pep-0008/
# Docstring conventions: http://www.python.org/dev/peps/pep-0257/

from scipy.sparse import *
from scipy import io
from scipy.optimize import curve_fit, leastsq

from itertools import *
from functools import partial

from math import *

import sys

from data1 import data1


# ===================
# = Distance metric =
# ===================

def dist(a, b):
    """Distance metric: Euclidean distance"""
    return LA.norm(a - b)
    
def distances(refs, pt, dtype=np.float64):
    # return asarray([dist(ref, pt) for ref in refs])
    return np.fromiter( imap(partial(dist, pt), refs), dtype=dtype, count=len(refs) )

# map_distances = np.vectorize(distances, excluded=['refs'])
def map_distances(refs, pts, dtype=np.float64):
    # order in list comp is important: for each point, iterate over refs
    return np.fromiter( (dist(ref, pt) for pt in pts for ref in refs), dtype=dtype, count=len(refs)*len(pts))
    # return [imap(partial(distances, refs), pts)]
    # return asarray([distances(refs, pt)])

# ====================
# = Future interface =
# ====================

# -- Globals
_station_pts = [];

# -- Interface
def configure(station_pts):
    """Specify the capture station coordinates in an ordered sequence (capture #0 at index 0)."""
    _station_pts = station_pts

def add_calibration_pt(src_pt, sig_strengths):
    return 1

def calibrate(src_pt, sig_strengths):
    """Calibrate using a known location and set of signal strengths.
    
    Make sure to 'configure' before running 'calibrate'.
    
    Keyword arguments
    -----------------
    src_pt -- transmitter location
    sig_strengths -- ordered sequence of signal strengths measured at each capture station
    """

def locate__(sig_strengths):
    """Estimate a location.
    
    For this result to have any meaning, 'configure' and 'calibrate' beforehand.
    
    Keyword arguments
    -----------------
    sig_strengths -- ordered sequence of signal strengths measured at the capture stations
    """
    # cast to double, to prevent weird int division problems

# ===================
# = Matrix creation =
# ===================
def coef_matrix(n, tn):
    """
    n is the number of stations, tn is the number of calibration points
    
    Produce the matrix A used in the least-squares solver to solve
        Ax = b
    for x = (k_1, k_2 ... k_n, T_1, T_2 ... T_pts).
    
    k are the antenna gain/loss constants
    T are the transmit power values of the calibration packets
    
    Example: n = 3, pts = 2
    In [1]: coef_matrix(3,2)
    Out[1]: 
    array([[ 1.,  0.,  0., -1., -0.],
           [ 0.,  1.,  0., -1., -0.],
           [ 0.,  0.,  1., -1., -0.],
           [ 1.,  0.,  0., -0., -1.],
           [ 0.,  1.,  0., -0., -1.],
           [ 0.,  0.,  1., -0., -1.]])
    """
    return np.hstack(( np.tile(np.identity(n), (tn,1)), np.repeat(-1*np.identity(tn), n, axis=0) ))

# FIXME: d = 0 => log(0) is error
def y_matrix(stations, pts, sigs):
    logds = 20*np.log10(map_distances(stations, pts))
    rs = sigs.flatten()
    return np.transpose(-(rs + logds))

def fx_coef(n, tn):
    """
    n is the number of stations, tn is the number of calibration points
    
    Produces the matrix used in f(x), where
        x = (k_1, k_2 ... k_n, T_1, T_2 ... T_pts)^T.
    
    In [1]: fx_coef(3,2)
    Out[1]: 
    array([[-1., -0., -0.,  1.,  0.],
           [-0., -1., -0.,  1.,  0.],
           [-0., -0., -1.,  1.,  0.],
           [-1., -0., -0.,  0.,  1.],
           [-0., -1., -0.,  0.,  1.],
           [-0., -0., -1.,  0.,  1.]])
    """
    return np.hstack(( np.tile(-1*np.identity(n), (tn,1))
                     , np.repeat(np.identity(tn), n, axis=0)
                    ))

# =====================================
# = Target functions for optimization =
# =====================================

def weighting(ds):
    # k = 1.4
    # return (8**k)*1.0/(ds**k)
    return 1.0

# http://blogs.aerohive.com/blog/the-network-revolution/apple-iphone-5-wi-fi-specs
def penalize(sigs, factor=0.04):
    diff = sigs - 15  # iPhone 5 transmits ~16dBm
    return factor*np.dot(diff, diff)  # Squared L2 norm; ridge regression; Tikhonov regularization

# TODO: Penalty scales with number of points
def est_k_weighted(n, a_matrix, ds, recvs, est_params):
    # weight * (actual distance - estimated distance)
    # 1/distance^2 * (distance - e^((T - R - k)/20))
    est_ds = np.exp((np.dot(a_matrix, est_params) - recvs)/20)
    penalty = penalize(est_params[n:])
    return weighting(ds) * (ds - est_ds) + penalty

def epsilon_dist(station_infos, sigs, estimate):
    refs, ks = station_infos
    (x,y,transmit) = estimate
    coord_est = np.array((x,y))
    
    calc_ds = distances(refs, coord_est)
    sig_ds = np.exp((transmit - sigs - ks)/20)
    penalty = penalize(transmit, 1)
    
    return weighting(calc_ds) * (calc_ds - sig_ds) + penalty

# ===============
# = Interim API =
# ===============

# http://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.leastsq.html
def solve_calibrate(stations, pts, sigs):
    """
    Finds the least-squares solutions for the station antenna gain/loss constants
    
    stations - station coordinates
    pts - coordinates of calibration points
    sigs - signal strengths for each calibration point, in order by station
    """
    n = len(stations)  # number of stations
    tn = len(pts)  # number of calibration points
    
    a_matrix = fx_coef(n, tn)
    ds = map_distances(stations, pts)
    recvs = sigs.flatten()
    x0 = [1.0] * (n+tn)  # initial estimate: antenna constants
    
    return leastsq(partial(est_k_weighted, n, a_matrix, ds, recvs), x0=x0)#[:n]

def locate(stations, ks, sigs):
    """
    Estimate a source point's location using trilateration.
    
    stations - station coordinates
    ks - station antenna constants
    sigs - signal strengths, in order by station
    """
    # return curve_fit(est_recv_sig, (stations, ks), sigs, p0=(1,1,-15))  # minimize: sigs - est_recv_sig(xdata, *params)
    n = len(stations)  # number of stations
    x0 = (1,1,15)  # initial estimate: coordinate, signal strength
    
    return leastsq(partial(epsilon_dist, (stations, ks), sigs), x0=x0)[0]
