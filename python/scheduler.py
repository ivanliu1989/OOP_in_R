# -*- coding: utf-8 -*-
#import numpy as np
import datetime as dtm
#import pandas as pd
#import matplotlib.pyplot as plt
#import matplotlib.dates as dt
#import os
import csv
import collections
#from functools import reduce
#import sys
import unittest
#import rpy
from rpy2.robjects import r



class Operation:
    def __init__(self, opNme, duration): 
        self.opNme = opNme
        self.duration = duration
        self.startTime = None
        self.endTime = None
        
    def __str__(self):
        return "(" + str(self.opNme) + ": " + str(self.startTime) + ", " + str(self.endTime) + ")"     
        
    def getStartTime(self):
        self.startTime = self.endTime - self.duration

