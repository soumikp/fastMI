#!/usr/bin/env python
# coding: utf-8

# In[ ]:


from fastkde import fastKDE
import numpy as np
import pandas as pd

def fastkde_estim(x, y):
    x = np.array(x)
    y = np.array(y)
    
    joint = fastKDE.pdf_at_points(var1 = x, var2 = y)
    joint = joint[joint > 0]
    margin_x = fastKDE.pdf_at_points(var1 = x)
    margin_x = margin_x[margin_x > 0]
    margin_y = fastKDE.pdf_at_points(var1 = y)
    margin_y = margin_y[margin_y > 0]

    h_xy = -np.mean(np.log(joint))
    h_x = -np.mean(np.log(margin_x))
    h_y = -np.mean(np.log(margin_y))

    mi = np.abs(h_x + h_y - h_xy)
    
    return(mi)
