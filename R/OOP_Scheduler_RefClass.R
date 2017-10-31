# If you don't want to specify what variable type a field should have, pass "ANY" as the value in the list of fields.
# Any constructor logic needs to be in an optional function called initialize.
# If the first line of a method is a string, it is interpreted as documentation for that method.
# Inside a method, if you want to assign to a field, use global assignment (<<-).
# http://adv-r.had.co.nz/OO-essentials.html#rc
# http://adv-r.had.co.nz/R5.html


rm(list = ls()); ls()
# Operation ---------------------------------------------------------------
Operation <- setRefClass(
  "Operation",
  fields = list(
    opNme = "character",
    duration = "numeric",
    startTime = "ANY",
    endTime = "ANY"
  ),
  methods = list(
    initialize = function(opNme, duration)
    {
      "This method is called when you create an instance of the class."
      opNme <<- opNme
      duration <<- duration
      endTime <<- NA
      startTime <<- NA
      # print(paste0(opNme, ": ", startTime, ", ", endTime))
    },
    getStartTime = function()
    {
      print("Get Start Time of the Operation")
      startTime <<- endTime - duration
    }
  )
)
# Test
# obj1 <- Operation$new(opNme = "testOps", duration = 3600)
# obj1
# obj1$endTime = 1800
# obj1
# obj1$getStartTime()
# obj1


# Store manager -----------------------------------------------------------
StoreManager <- setRefClass(
  "StoreManager",
  fields = list(
    stores = "ANY",
    maxTime = "numeric"
  ),
  methods = list(
    initialize = function()
    {
      "This method is called when you create an instance of the class."
      stores <<- list()
      maxTime <<- 9999999
    },
    
    addStore = function(storeId, profile){
      newStore <- Store$new(storeId, profile)
      stores[[length(stores)+1]] <<- newStore
    }
  )
)
# storemanager1 <- StoreManager$new()


# Oven --------------------------------------------------------------------
Oven <- setRefClass(
  "Oven",
  fields = list(
    ovenId = "ANY",
    cty = "numeric",
    cookTime = "numeric",
    washCycleLength = "numeric"
  ),
  methods = list(
    initialize = function(cty, cookTime, washCycleLength)
    {
      "This method is called when you create an instance of the class."
      ovenId <<- NA
      cty <<- cty
      cookTime <<- cookTime
      washCycleLength <<- washCycleLength
    }
  )
)
# oven1 <- Oven$new(cty = 1, cookTime = 120, washCycleLength = 10)


# Job ---------------------------------------------------------------------
Job <- setRefClass(
  "Job",
  fields = list(
    oven = "ANY",
    endTime = "numeric",
    batchSize = "numeric",
    jobId = "ANY",
    ops = "list"
  ),
  methods = list(
    initialize = function(oven, endTime, batchSize)
    {
      endTime <<- endTime
      batchSize <<- batchSize
      jobId <<- NA
      oven <<- oven
      ops <<- list(unload = Operation$new(opNme = "unload", duration = 15),
                   cook = Operation$new(opNme = "cook", duration = oven$cookTime),
                   load_and_sprinkle = Operation$new(opNme = "load and sprinkle", duration = 15),
                   rinse = Operation$new(opNme = "rinse", duration = oven$washCycleLength)
      )
      
      # ops <- list(unload = Operation$new(opNme = "unload", duration = 15),
      #             cook = Operation$new(opNme = "cook", duration = 300),
      #             load_and_sprinkle = Operation$new(opNme = "load and sprinkle", duration = 15),
      #             rinse = Operation$new(opNme = "rinse", duration = 300)
      # )
      getOpEndTimes()
    },
    getOpEndTimes = function()
    {
      prevOp <- endTime
      for(i in ops){
        if(is.na(i$endTime)){
          i$endTime = prevOp 
        }
        i$getStartTime()
        prevOp <- i$startTime
      }
    },
    
    getDuration = function()
    {
      tm <- 0
      for(i in ops){
        tm = i$duration + tm
      }
      return(tm)
    },
    
    getStartTime = function()
    {
      startTime = ops[[length(ops)]] 
      return(startTime)
    },
    
    jobFollowsJob = function(prevJob)
    {
      # if(oven == prevJob$oven){
      # store$schedules[[1]]
      # sch$jobs[[12]]$jobFollowsJob(sch$jobs[[11]])
      # prevJob = sch$jobs[[11]]
      # curJob = sch$jobs[[12]]
      # if(curJob$oven$cty == prevJob$oven$cty & curJob$oven$cookTime == prevJob$oven$cookTime & curJob$oven$washCycleLength == prevJob$oven$washCycleLength){
      #   return(curJob$ops[[length(curJob$ops)]]$startTime >= prevJob$endTime)
      # }else{
      #   return(TRUE)
      # }
      
      if(oven$cty == prevJob$oven$cty & oven$cookTime == prevJob$oven$cookTime & oven$washCycleLength == prevJob$oven$washCycleLength){
        return(ops[[length(ops)]]$startTime >= prevJob$endTime)
      }else{
        return(TRUE)
      }
    }
  )
)


# Schedule ----------------------------------------------------------------
Schedule <- setRefClass(
  "Schedule",
  fields = list(
    jobs = "list"
  ),
  methods = list(
    initialize = function()
    {
      "This method is called when you create an instance of the class."
      jobs <<- list()
    },
    addJob = function(oven, endTime, batchSize){
      newJob = Job$new(oven, endTime, batchSize)
      newJob$jobId = length(jobs)
      jobs[[length(jobs)+1]] <<- newJob
    },
    meetsConstraints = function(nxt, current){
      if(nxt == 0){ #base case: stop when there is no next job
        return(TRUE)
      }else if(jobs[[nxt]]$jobFollowsJob(jobs[[current]])){ #complex case
        return(meetsConstraints(nxt-1, current-1))
      }else{
        return(FALSE)
      }
    },
    getMakespan = function(){
      tm = 0
      for(i in jobs){
        tm <- i$getDuration() + tm
      }
      return(tm)
    },
    printSchedule = function(){
      for(jb in length(jobs):1){
        jb = jobs[[jb]]
        print(jb)
        for(op in length(jb$ops):1){
          op = jb$ops[[op]]
          print(op)
        }
      }
    }
  )
)


# Store -------------------------------------------------------------------
Store <- setRefClass(
  "Store",
  fields = list(
    storeId = "ANY",
    profile = "list",
    ovens = "list",
    schedules = "list",
    forecast = "numeric",
    optSch = "ANY"
  ),
  methods = list(
    initialize = function(storeId, profile){
      storeId <<- storeId
      ovens <<- list()
      schedules <<- list()
      profile <<- profile
      forecast <<- 0
      optSch <<- NA
    },
    
    addOven = function(cty, cookTime, washCycleLength){
      newOven = Oven$new(cty, cookTime, washCycleLength)
      newOven$ovenId <- length(ovens)
      ovens[[length(ovens)+1]] <<- newOven
    },
    
    notCompliesWithShelfLife = function(startTime, endTime){
      endTime = as.numeric(endTime)
      startTime = as.numeric(startTime)
      return((endTime - startTime) > 4 * 60)
    },
    
    planBatchSize = function(subsetSum, ovenIndx, indx, indxEnd, startingValue){
      ks = names(profile)
      val = profile[[ks[indx]]] # time & its forecast
      
      if(notCompliesWithShelfLife(ks[indx], ks[indxEnd])){ # if cook time over 4 hours
        print("1st")
        return(c(indx+1, ks[indx+1], startingValue - as.numeric(profile[[ks[indx+1]]])))
      }else if(subsetSum == 0){ #base case 1: if the subset sum exactly matches the capacity 
        print("2nd")
        return(c(indx,ks[indx],startingValue))
      }else if(subsetSum < 0){ #base case 2 if the subset sum exceeds the oven capacity
        print("3rd")
        return(c(indx+1, ks[indx+1], startingValue - as.numeric(profile[[ks[indx+1]]])))
      }else if(indx == 1){ #base case 3 the current index equals zero
        print("4th")
        return(c(indx,ks[indx],startingValue))
      }else if(val > ovens[[ovenIndx]]$cty & subsetSum == ovens[[ovenIndx]]$cty){ #base case 4 if a measurement exceeds the oven capacity and the subsetSum equals oven capacity
        print("5th")
        return(c(indx-1,ks[indx-1],ovens[[ovenIndx]]$cty)) #???
      }else{ #complex case
        print("6th")
        return(planBatchSize(subsetSum-val, ovenIndx, indx-1, indxEnd, startingValue+val))
      }
    },
    
    planOvens = function(startingValue, sol, indx){
      
      # length(store$ovens)
      # indx = lenProf
      
      if(startingValue >= sum(unlist(profile)) | indx == 1){ 
        # if start val over total demands (forecast) OR time index (every 15 mins) is 1
        schedules[[length(schedules)+1]] <<- sol
        # print(schedules)
      }else{
        for(i in 1:length(ovens)){
          print(paste0("Planning for Oven: ", i))
          var = planBatchSize(ovens[[i]]$cty, i, indx, indx, 0) # ovenCapacity, ovanIdx, timeIdx, timeEndIdx, startVal
          # return c(time idx, time name, remaining demands)
          
          sol[[length(sol)+1]] <- c(i, as.numeric(var[2]), as.numeric(var[3])) # ovenId, time, remaining demands
          # print(var[3])
          planOvens(startingValue+as.numeric(var[3]), sol, as.numeric(var[1]))
          sol[[length(sol)]] = NULL # backtrack
        }
      }
    },
    
    trySchedules = function(){
      # schedules = store$schedules
      drtn = 999999999
      for(s in 1:length(schedules)){
        s = schedules[[s]]
        sch = Schedule$new()
        for(jb in 1:length(s)){
          jb = s[[jb]]
          # sch$addJob(store$ovens[[jb[1]]], jb[2], jb[3]) # <=================
          sch$addJob(ovens[[jb[1]]], jb[2], jb[3]) 
        }
        mkSpn = sch$getMakespan() # start Time integer negative <============================
        lenSch = length(s)
        if(mkSpn <= drtn){
          if(sch$meetsConstraints(lenSch - 1, lenSch)){
            drtn = mkSpn
            optSch <<- sch
            # print(mkSpn)
          }
        }
      }
    }
  )
)
# Store$new()



# Read Data ---------------------------------------------------------------
readOvenInfo = function(){
  file = data.table::fread("Files/ovenCapacityByStore.csv")
}

readProfile = function(){
  file = data.table::fread("Files/storeProfile.csv")
}

readForecast = function(){
  file = data.table::fread("Files/forecastStores.csv")
}

readStores = function(){
  file = data.table::fread("Files/stores.csv")
}


# Main Optimizer ----------------------------------------------------------
mainOptimizer = function(updateStoreList = TRUE, updateOvenInfo = TRUE, updateForecast = TRUE){
  
  sm = StoreManager$new()
  
  # Add Stores and their profiles to Store Manager
  if(updateStoreList){
    stores = readStores()[1]
    for(store in stores$V1){
      # store = stores$V1[1]
      profile = readProfile()
      profile[, V1 := 525] # Impute hours with no sales
      profile = profile[V1 == store]
      
      prof = list()
      for(time in profile$V2){
        time = profile[V2==time]
        prof[as.character(time$V2)] = as.numeric(time$V3)
      }
      sm$addStore(store, prof)
    }
  }
  
  # Add Oven Info for each store
  if(updateOvenInfo){
    ovenInfo = readOvenInfo()
    for(store in 1:length(sm$stores)){
      store = sm$stores[[store]]
      ovenInfoSt = ovenInfo[Store == store$storeId]
      for(o in 1:nrow(ovenInfoSt)){
        o = ovenInfoSt[o]
        store$addOven(as.numeric(o$Capacity), as.numeric(o$AvgCookTime), as.numeric(o$CleanBetweenCooks))
      }
    }
  }
  
  # Plan out schedules for each store
  for(store in 1:length(sm$stores)){
    # store = sm$stores[[1]]
    store = sm$stores[[store]]
    lenProf = length(store$profile)
    store$planOvens(startingValue = 0, sol = list(), indx = lenProf)
    
    # to be reviewed
    store$trySchedules()
    store$optSch$printSchedule()
  }
  
}

mainOptimizer()
