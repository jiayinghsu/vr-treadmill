#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Apr 10 12:29:52 2017

@author: oscartgiles

Various plotting functions for summarising MCMC chains

Plots should be compatible with seaborn plots by using the hue_offsets() function from seaborn

BAYES PLOTTING FILE FOR THE PSYC SCI PAPER
"""

import numpy as np
import seaborn as sns
import matplotlib.pylab as plt
import pdb
import pandas as pd
from pymc.utils import hpd
import itertools

def hue_offsets(n_hue_levels, width = 0.8):
    """A list of center positions for plots when hue nesting is used.
    Taken from Seaborn repository. THis code is borrowed from the seaborn github repository"""
    n_levels = n_hue_levels
    
    each_width = width / n_levels
    offsets = np.linspace(0, width - each_width, n_levels)
    offsets -= offsets.mean()
    
    return offsets

def point_plot(x = None, y = None, hue = None, order = None, hue_order = None, ax = None, 
               estimator = np.mean, data = None, width = 0.8, **kwargs):
    """Draw a point plot
    Pass a pandas data frame and plot using x, y and (optionally) hue.
    Estimator: Pass a function to provide the summary statistic.
    If a hue is passed a hue order can be given"""
    
    if (x == None):
        raise TypeError("Missing x label")
        
    if (y == None):
        raise TypeError("Missing y label")        
        
    if ax == None:
        ax = plt.gca()        
    
    
    if ('marker' in kwargs.keys()):      
        
        marker = kwargs['marker'] #Get the marker object
        del kwargs['marker'] #Remove marker from the kwargs dictionary   
    
    else:
        marker = 'o'        
        
        
    if hue == None:
        
        #Order data by x
        if order:
            data[x] = pd.Categorical(data[x], order)
            data.sort_values(x, inplace = True)
        
        summary_vals = data.groupby([x]).agg({y: estimator}).reset_index()   
 
        n_x = range(len(summary_vals[x].unique()))          

        ax.plot(range(summary_vals.shape[0]), summary_vals[y], marker = marker, **kwargs)
    
    else:            
        
        summary_vals = data.groupby([x, hue]).agg({y: estimator}).reset_index() 
        
        if order:            
            summary_vals[x] = pd.Categorical(summary_vals[x], order)
            summary_vals.sort_values(x, inplace = True)
            
        if hue_order:            
            summary_vals[hue] = pd.Categorical(summary_vals[hue], hue_order)
            summary_vals.sort_values([x, hue], inplace = True)   
            
    
        n_x = range(len(summary_vals[x].unique()))
        offsets = hue_offsets(len(summary_vals[hue].unique()), width = width) 
        
        i = 0
        for inner in summary_vals[hue].unique():

            if isinstance(marker, str):
                marker_ = marker
                
            elif isinstance(marker, list):
                marker_ = marker[i]
            
            ax.plot(n_x + offsets[i], summary_vals[summary_vals[hue] == inner][y], marker = marker_, label = str(inner),  **kwargs) 
            
            i += 1
        
    ax.set_xticks(n_x)
    ax.set_xticklabels(summary_vals[x].unique())
#        ax.xticks(n_x, summary_vals[x].unique(), rotation='vertical')

def error_plot(x = None, y = None, hue = None, order = None, hue_order = None, 
               ax = None, estimator = np.mean, hpd_alpha = 0.05, data = None, stride = 0.8, **kwargs):  
    """Draw a point plot
    Pass a pandas data frame and plot using x, y and (optionally) hue.
    hpd_alpha: 1-hpd_alpha is the percentage HDI to plot with error bars
    If a hue is passed a hue order can be given"""
        

    if (x == None):
        raise TypeError("Missing x label")
        
    if (y == None):
        raise TypeError("Missing y label")        
        
    if ax == None:
        ax = plt.gca()
        
    if hue == None:        
        #Order data by x
        if order:
            data[x] = pd.Categorical(data[x], order)
            data.sort_values(x, inplace = True)
        
        summary_vals = data.groupby([x]) 
        y_err = np.empty((2, len(summary_vals)))
        
        i = 0
        for name, val in summary_vals:
#            print(val[y])
            y_err[:, i] = hpd(val[y], hpd_alpha)
            i += 1

#        pdb.set_trace()
        ax.errorbar(x = range(len(summary_vals)), y = y_err.mean(0), 
                    yerr =  y_err[0] - y_err.mean(0), **kwargs)   
        
    else:
                
        offsets = hue_offsets(len(data[hue].unique()), width = stride)
       
        if order:        
            data[x] = pd.Categorical(data[x], order)
            data = data.sort_values(x)
            
        if hue_order:            
            data[hue] = pd.Categorical(data[hue], hue_order)
            data = data.sort_values([x, hue])               
#        pdb.set_trace()
        summary_vals = data.groupby([x, hue])
        
        
#        

        if isinstance(hpd_alpha, float):
            y_err = np.empty((2, len(summary_vals)))
            
    #        
            i = 0
            for name, val in summary_vals:            
                y_err[:, i] = hpd(val[y], hpd_alpha)
                i += 1
            y_err = y_err.reshape((2, len(data[x].unique()), len(data[hue].unique()) ))           
        
        elif callable(hpd_alpha):
        
            y_err = np.empty((len(summary_vals)))
            y_trend = np.empty((len(summary_vals)))
            i = 0
            for name, val in summary_vals:                  
                y_err[i] = hpd_alpha(val[y])
                y_trend[i] = np.mean(val[y])
                i += 1
            y_err = y_err.reshape((len(data[x].unique()), len(data[hue].unique()) ))  
            y_trend = y_trend.reshape((len(data[x].unique()), len(data[hue].unique()) ))  
            
            

        n_x = range(len(data[x].unique()))    
        

    
        i = 0
        for inner in data[hue].unique():           
            
         
            if callable(hpd_alpha):
#                pdb.set_trace()
                y_err_ = np.zeros((2, y_err.shape[0]))
                y_err_[1] = y_err[:,i]
                ax.errorbar(n_x + offsets[i],  y_trend[:,i], 
                        yerr =  y_err_, **kwargs) 
            else:
                ax.errorbar(n_x + offsets[i], y_err.mean(0)[:,i], 
                            yerr =  (y_err[0] - y_err.mean(0))[:,i], **kwargs) 
            
            i += 1
    
def bar_plot(x = None, y = None, hue = None, order = None, hue_order = None, ax = None, 
               estimator = np.mean, data = None, stride = 0.8, **kwargs):
    """Draw a point plot
    Pass a pandas data frame and plot using x, y and (optionally) hue.
    Estimator: Pass a function to provide the summary statistic.
    If a hue is passed a hue order can be given"""
    
    if (x == None):
        raise TypeError("Missing x label")
        
    if (y == None):
        raise TypeError("Missing y label")        
        
    if ax == None:
        ax = plt.gca()            
    
    if ('color' in kwargs.keys()):      
        
        color = kwargs['color'] #Get the marker object
        del kwargs['color'] #Remove marker from the kwargs dictionary   
    
    else:
        color = 'k'    

        
    if hue == None:
        
        #Order data by x
        if order:
            data[x] = pd.Categorical(data[x], order)
            data.sort_values(x, inplace = True)
        
        summary_vals = data.groupby([x]).agg({y: estimator}).reset_index()   
 
        n_x = range(len(summary_vals[x].unique()))          

        ax.bar(range(summary_vals.shape[0]), summary_vals[y], color = color, **kwargs)
    
    else:            
        
        summary_vals = data.groupby([x, hue]).agg({y: estimator}).reset_index() 
        
        if order:            
            summary_vals[x] = pd.Categorical(summary_vals[x], order)
            summary_vals.sort_values(x, inplace = True)
            
        if hue_order:            
            summary_vals[hue] = pd.Categorical(summary_vals[hue], hue_order)
            summary_vals.sort_values([x, hue], inplace = True)   

        n_x = range(len(data[x].unique()))
        
        offsets = hue_offsets(len(summary_vals[hue].unique()), width = stride) 

        i = 0
        for inner in summary_vals[hue].unique():

            if isinstance(color, str):
                color_ = color
                
            elif isinstance(color, list):
                color_ = color[i]
        
            ax.bar(n_x + offsets[i], summary_vals[summary_vals[hue] == inner][y], label = str(inner), color = color_, **kwargs) 
            
            i += 1
        
        
    ax.set_xticks(n_x)
    ax.set_xticklabels(summary_vals[x].unique())
    
    
def plot_violin(x = None, y = None, hue = None, order = None, hue_order = None, ax = None, data = None, palette = None, max_width = 0.5, vert = False,  **kwargs):


    if (x == None):
        raise TypeError("Missing x label")
        
    if (y == None):
        raise TypeError("Missing y label")        
        
    if ax == None:
        ax = plt.gca()            
    
    
 
    if hue == None:
        
        plot_conds = order
        
    else:
        plot_conds = list(itertools.product(order, hue_order)) #A list of every condition
        
        if palette:
            palette = palette * len(order)
    

    #conds = list(itertools.product(names, outcome)) #A list of every condition
    if hue == None:        
        violin_data = [data[(data[x] == i)][0].values for i in plot_conds]      
        violin_x_pos = range(len(order))
        
    else:
        violin_data = [data[(data[x] == i[0]) & (data[hue] == i[1])][0].values for i in plot_conds]    

        violin_x_pos = np.repeat(range(len(order)), 3) + np.tile(hue_offsets(3), 4)
    
       
    parts = ax.violinplot(violin_data, violin_x_pos, showmeans= False, showextrema=False, widths = max_width, vert = vert)
    
    for i, pc in enumerate(parts['bodies']):
        pc.set_facecolor(palette[i])
        pc.set_edgecolor('black')
            


    return parts