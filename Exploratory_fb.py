# -*- coding: utf-8 -*-
"""
Created on Wed Sep  5 12:37:56 2018

@author: pedzenon
"""

import pandas as pd
import matplotlib.pylab as plt 
from datetime import datetime
import datetime as dt
import scipy.signal as sig
from scipy.interpolate import interp1d


# Exploratory analysis:

data_fb = pd.read_csv(".\\data\\daily_fb_bq_hva_updated.csv")

data_fb["date"] = pd.to_datetime(data_fb["date"], format='%Y-%m-%d', errors='coerce') 
# Chequeo:
times = pd.date_range(start = '2018-01-01', end = '2018-09-15').get_values()
aux = pd.DataFrame([],columns = ["date"])
aux["date"] = times
data_fb = pd.merge(aux,data_fb,on = ["date"],how = "left")
import numpy as np
Fs = 1
f = 1/44   # n puntos -> 0.0227 Hz
sample = data_fb.shape[0]
x = np.arange(sample)
y = 4000*np.sin(2 * np.pi * f * x / Fs)
plt.plot(data_fb.landing_page_view,'r')
plt.plot(y,'b')
data_fb.to_csv(".\\data\\daily_fb_bq_hva_updated_full.csv")

f, Pxx  = sig.periodogram(signal.form_fills, fs=1.0,nfft = 256)

plt.plot(f,Pxx)

data_fb.to_csv(".\\data\\daily_fb_bq_hva_updated_full.csv")

