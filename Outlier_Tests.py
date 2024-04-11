#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 10 15:53:06 2024

@author: jkosnoff
"""

import numpy as np

def IQR_Outlier_test(data):
    """
    A test for outliers based on interquartiles
    See https://online.stat.psu.edu/stat200/lesson/3/3.2
    
    Parameters:
    - x: array-like, input data
    
    Returns:
    - outliers: array-like, an array of boolean indices for whether or not the data
    at index i falls outside of the interquartile range
    """
    Q1 = np.percentile(data, 25, method = 'midpoint') 
    Q3 = np.percentile(data, 75, method = 'midpoint')
    IQR = Q3 - Q1 
    
    outliers = [(datum > Q3 + 1.5 * IQR) or (datum < Q1 - 1.5 * IQR) for datum in data]
    
    return outliers


def double_mad(x, zero_mad_action="warn"):
    """
    Calculate the Double Median Absolute Deviation (DoubleMAD) for a given array x.
    
    Converted to Python from Peter Rosenmai's Eureka Statistcs blog post
    https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
    
    Parameters:
    - x: array-like, input data
    - zero_mad_action: str, determines the action in the event of an MAD of zero.
                       Possible values: "stop", "warn", "na", and "warn and na".

    Returns:
    - left_mad: float, left median absolute deviation
    - right_mad: float, right median absolute deviation
    """
    x = np.array(x)
    x = x[~np.isnan(x)]
    m = np.median(x)
    abs_dev = np.abs(x - m)
    left_mad = np.median(abs_dev[x <= m])
    right_mad = np.median(abs_dev[x >= m])
    
    if left_mad == 0 or right_mad == 0:
        if zero_mad_action == "stop":
            raise ValueError("MAD is 0")
        if zero_mad_action in ["warn", "warn and na"]:
            print("MAD is 0")
            left_mad += 1e-12
            left_mad += 1e-12
        if zero_mad_action in ["na", "warn and na"]:
            if left_mad == 0:
                left_mad = np.nan
            if right_mad == 0:
                right_mad = np.nan
    
    return left_mad, right_mad

def double_mads_from_median(x, zero_mad_action="warn"):
    """
    Calculate the Double Median Absolute Deviation (DoubleMAD) distances from the median for a given array x.
    
    Converted to Python from Peter Rosenmai's Eureka Statistcs blog post
    https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
      
    Parameters:
    - x: array-like, input data
    - zero_mad_action: str, determines the action in the event of an MAD of zero.
                       Possible values: "stop", "warn", "na", and "warn and na".

    Returns:
    - mad_distance: array, MAD distances from the median
    """
    two_sided_mad = double_mad(x, zero_mad_action)
    m = np.median(x)
    x_mad = np.repeat(two_sided_mad[0], len(x))
    x_mad[x > m] = two_sided_mad[1]
    mad_distance = np.abs(x - m) / (x_mad + 1e-12)
    mad_distance[x == m] = 0
    
    return mad_distance
