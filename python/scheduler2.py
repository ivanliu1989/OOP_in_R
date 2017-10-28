# -*- coding: utf-8 -*-
import datetime as dtm
import csv
import collections
import unittest
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


class Job:
    def __init__(self, oven, endTime, batchSize):
        self.endTime = dtm.timedelta(hours = int(endTime[:2]), minutes = int(endTime[2:]))
        self.batchSize = batchSize
        self.jobId = None
        self.oven = oven
        self.ops = [Operation("unload", dtm.timedelta(minutes=15)),
                    Operation("cook", dtm.timedelta(minutes=oven.cookTime)),
                    Operation("load and sprinkle", dtm.timedelta(minutes=15)), 
                    Operation("rinse", dtm.timedelta(minutes=oven.washCycleLength))]
        self.__getOpEndTimes()
                     
    def __str__(self):
        return '\n' + '\n'  + "(job: " + str(self.jobId) + ", oven ID: " + str(self.oven.ovenId) + ", oven capacity: " + str(self.oven.cty) + ", batch size: " + str(self.batchSize) + ")"     
    
    def getOpEndTimes(self): 
        prevOp = self.endTime
        
        for i in self.ops:
            i.endTime = prevOp if i.endTime == None else i.endTime            
            i.getStartTime()
            prevOp = i.startTime
            
    def getDuration(self):
        tm = dtm.timedelta(minutes = 0)
        for i in self.ops:
            tm+=i.duration
        return(tm)
    
    def getStartTime(self):
        self.startTime = self.ops[len(self.ops)-1]
    
    def jobFollowsJob(self, prevJob):
        if self.oven == prevJob.oven:
            return(self.ops[len(self.ops)-1].startTime >= prevJob.endTime)
        else: return True
    
    __getOpEndTimes = getOpEndTimes   # private copy of original getOpEndTimes method which is called every time this class is instantiated 


class Oven:
    def __init__(self, cty, cookTime, washCycleLength):
        self.ovenId = None
        self.cty = cty
        self.cookTime = cookTime
        self.washCycleLength = washCycleLength
        
        
class Schedule:
    def __init__(self):
        self.jobs = []

    def addJob(self, oven, endTime, batchSize):
        newJob = Job(oven, endTime, batchSize)
        newJob.jobId = len(self.jobs)
        self.jobs.append(newJob)

    def meetsConstraints(self, nxt, current):
        if nxt == 0: #base case: stop when there is no next job
            return True
        elif self.jobs[nxt].jobFollowsJob(self.jobs[current]): #complex case
            return(self.meetsConstraints(nxt-1, current-1))
        else: # base case
            return False
        
    def getMakespan(self):
        tm = dtm.timedelta(minutes = 0)
        for i in self.jobs:
            tm+=i.getDuration()
        return(tm)        
        
    def printSchedule(self):
        for jb in reversed(self.jobs):
            print(jb)
            for op in reversed(jb.ops):
                print(op)


class Store:
     def __init__(self, storeId, profile):
         self.storeId = storeId
         self.ovens = []
         self.schedules = []
         self.profile = profile
         self.forecast = 0
         self.optSch = None
    
     def addOven(self, cty, cookTime, washCycleLength):
         newOven = Oven(cty, cookTime, washCycleLength)
         newOven.ovenId = len(self.ovens)
         self.ovens.append(newOven)    
         
     def notCompliesWithShelfLife(self, startTime, endTime):
        endTime = dtm.timedelta(hours = int(endTime[:2]), minutes = int(endTime[2:]))
        startTime = dtm.timedelta(hours = int(startTime[:2]), minutes = int(startTime[2:]))
        return(endTime - startTime > dtm.timedelta(hours=4, minutes = 0))
     
     def planBatchSize(self, subsetSum, ovenIndx, indx, indxEnd, startingValue):
         ks = list(self.profile.keys())
         val = self.profile[ks[indx]]
         
         if self.notCompliesWithShelfLife(ks[indx], ks[indxEnd]):
             return(eval(str([indx+1, ks[indx+1], startingValue-self.profile[ks[indx+1]]])))
         elif subsetSum == 0: #base case 1: if the subset sum exactly matches the capacity 
             #print("1st")
             return(eval(str([indx, ks[indx], startingValue])))  
         elif subsetSum < 0: #base case 2 if the subset sum exceeds the oven capacity
             #print("2nd")
             return(eval(str([indx+1, ks[indx+1], startingValue-self.profile[ks[indx+1]]])))
         elif indx == 0:  #base case 3 the current index equals zero 
             #print("3rd")
             return(eval(str([indx, ks[indx], startingValue])))       
         elif val > self.ovens[ovenIndx].cty and subsetSum == self.ovens[ovenIndx].cty: #base case 4if a measurement exceeds the oven capacity and the subsetSum equals oven capacity
             #print('else')
             return(eval(str([indx-1, ks[indx-1], self.ovens[ovenIndx].cty]))) 
         else: #complex case
             #print(subsetSum)
             return(self.planBatchSize(subsetSum-val, ovenIndx, indx-1, indxEnd, startingValue+val))     

     def planOvens(self, startingValue, sol, indx):  
        
         if startingValue >= sum(self.profile.values()) or indx == 0: # 
             self.schedules.append(eval(str(sol)))
             #print(self.schedules)
         else:
             for i in range(len(self.ovens)):
                var = self.planBatchSize(float(self.ovens[i].cty), i, indx, indx, 0)
                sol.append([i, var[1], var[2]])
                self.planOvens(startingValue + var[2], sol, var[0])
                del sol[-1]   #backtrack 

     def trySchedules(self):
         drtn = dtm.timedelta.max
         for s in self.schedules:
             sch = Schedule()
             for jb in s:
                 sch.addJob(self.ovens[jb[0]], jb[1], jb[2])              
             mkSpn = sch.getMakespan()
             lenSch = len(s)-1
             if mkSpn <= drtn:
                 if sch.meetsConstraints(lenSch-1, lenSch):
                     drtn = mkSpn
                     self.optSch = sch
                     #print(mkSpn)
            
def readOvenInfo():
    file = []
    with open('Files/ovenCapacityByStore.csv', "r") as csvfile:
        spamreader = csv.reader(csvfile, delimiter = ',', quotechar = '|')
        for row in spamreader:
            file.append(row)
    return(file)


def readProfile():
    file = []
    with open('Files/storeProfile.txt', "r") as txtfile:
        spamreader = csv.reader(txtfile, delimiter = '\t', quotechar = '|')
        for row in spamreader:
            file.append(row)
    return(file)

        
def readForecast():
    file = []
    with open('Files/forecastStores.csv', "r") as csvfile:
        spamreader = csv.reader(csvfile, delimiter = ',', quotechar = '|')
        for row in spamreader:
            file.append(row)
    return(file)


def readStores():
    file = []
    with open('Files/stores.csv', "r") as csvfile:
        spamreader = csv.reader(csvfile, delimiter = ',', quotechar = '|')
        for row in spamreader:
            file.append(row)
    return(file)


class StoreManager():
    def __init__(self):
        self.stores = []
        self.maxTime = dtm.timedelta.max

    def addStore(self, storeId, profile):
        newStore = Store(storeId, profile)
        self.stores.append(newStore)  
     

def main(): #this function reads all the files
    
    # print command line arguments
    # for arg in sys.argv[1:]:
    #    print(arg)    
    
    updateStoreList = True
    updateOvenInfo = True
    #updateForecast = False
    
    sm = StoreManager()  
    
    if updateStoreList:
        stores = readStores()[0]
                
        for store in stores:
            profile = readProfile()
            
            #r.assign('remotestore', store)
            #profile = r('''
            #    config = list(
            #        loc_idnt = remotestore
            #    )
	#		
        #        print(config$loc_idnt)
	#		ds = read.table("./Files/storeProfile.txt", sep = "\t")
        #        #conCIW = connectCIW() #AA Package not installed on innvation server
        #        #readSQLFileGetResults(conCIW, 'Files/getProfile.sql', dataConfig = config) #AA Package not installed on innovation server
	#	return(ds)
        #        ''')            
            
        #    print(profile)
            prof = collections.OrderedDict()
            for time in profile:
                prof[time[1]] = float(time[2])
            sm.addStore(store, prof)
            prof = collections.OrderedDict()
           
    if updateOvenInfo or updateStoreList:
        ovenInfo = readOvenInfo() 
        for store in sm.stores:
            for o in ovenInfo:
                if store.storeId == o[0]:
                    store.addOven(float(o[11]), float(o[12]), float(o[13]))     
                   
    for store in sm.stores:
        lenProf = len(store.profile)-1
        store.planOvens(0, [], lenProf)
        store.trySchedules()
        store.optSch.printSchedule()

  
if __name__ == "__main__": #might have to be changed if script is called from a different environment
    main()




#Unit test
#class TestOperation(unittest.TestCase):
    
#    def testOpGetStartTime(self):
#        op = Operation("just a test", dtm.timedelta(minutes=15))
#        op.endTime = dtm.datetime(2017, 7, 3, 14, 9, 2, 896189)
#        op.getStartTime()
#        self.assertEqual(op.startTime, dtm.datetime(2017, 7, 3, 13, 54, 2, 896189))
#tst = TestOperation()



#==============================================================================
# 
# data = []
# for i in tst.jobs:
#     for j in i.ops:
#         data.append([i.jobId, j.opNme, i.oven.ovenId, j.startTime, j.endTime])
#  
# 
# df = pd.DataFrame(data, columns = ['jobId', 'opNme', 'ovenId', 'startTime', 'endTime'])
# res = df.sort_values(by=['jobId', 'startTime'], ascending=[True, True])
# 
# res['opId'] = range(len(df))
# 
# color_mapper = np.vectorize(lambda x: {0: 'red', 1: 'blue'}.get(x))
# 
# 
# res.startTime = pd.to_datetime(res.startTime).astype(dtm.datetime)
# res.endTime = pd.to_datetime(res.endTime).astype(dtm.datetime)
# 
# fig = plt.figure()
# #fig, ax = plt.subplots(2,2,figsize=(5,5))
# ax = fig.add_subplot(111)
# ax = ax.xaxis_date()
# ax = plt.hlines(res.opId, 
#                 dt.date2num(res.startTime), 
#                 dt.date2num(res.endTime), 
#                 colors=color_mapper(res.ovenId),
#                 linewidth=8)
#  
# with open('H:\My Projects\BBQ Chicken\ovenCapacityByStore.csv', 'rb') as csvfile:
#      spamreader = csv.reader(csvfile, delimiter=' ', quotechar='|')
# #     for row in spamreader:
# #         print ', '.join(row)
# 
#==============================================================================
