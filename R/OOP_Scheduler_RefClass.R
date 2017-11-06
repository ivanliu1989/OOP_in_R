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
      "Operations of cooking - unload, cook, load and sprinkle, rinse and etc."
      opNme <<- opNme
      duration <<- duration
      endTime <<- NA
      startTime <<- NA
    },
    getStartTime = function()
    {
      print(paste0("Get Start Time of the Operation - ", opNme, ": ", startTime, ", ", endTime))
      startTime <<- endTime - duration
    }
  )
)


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
      "Store manager"
      stores <<- list()
      maxTime <<- 9999999
    },
    
    addStore = function(storeId, profile){
      newStore <- Store$new(storeId, profile)
      stores[[length(stores)+1]] <<- newStore
    }
  )
)


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
      "An oven with maximum capacity, average cooking time and wash time"
      ovenId <<- NA
      cty <<- cty
      cookTime <<- cookTime
      washCycleLength <<- washCycleLength
    }
  )
)


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
      "A job includes a series of operations for cooking by an oven at the certain time"
      endTime <<- endTime
      batchSize <<- batchSize
      jobId <<- NA
      oven <<- oven
      ops <<- list(unload = Operation$new(opNme = "unload", duration = 15),
                   cook = Operation$new(opNme = "cook", duration = oven$cookTime),
                   load_and_sprinkle = Operation$new(opNme = "load and sprinkle", duration = 15),
                   rinse = Operation$new(opNme = "rinse", duration = oven$washCycleLength)
      )
      
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
      "A schedule includes cooking jobs for all ovens within a store for a given day"
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
      library(data.table)
      fnlSch = rbindlist(lapply(length(jobs):1, function(x){
        jb = jobs[[x]]
        schJob = rbindlist(lapply(length(jb$ops):1, function(x){
          op = jb$ops[[x]]
          return(data.table(opNme = op$opNme,
                            opDuration = op$duration,
                            opStartTime = getMinToTime(op$startTime),
                            opEndTime = getMinToTime(op$endTime)))
        }))
        schJob[, jobId := jb$jobId]
        schJob[, batchSize := jb$batchSize]
        schJob[, jobEndTime := getMinToTime(jb$endTime)]
        schJob[, ovenId := jb$oven$ovenId]
        schJob[, ovenCty := jb$oven$cty]
        schJob[, ovenCookTime := jb$oven$cookTime]
        schJob[, ovenWashCycleLength := jb$oven$washCycleLength]
        
        schJob[, c(5:11,1:4), with = F]
      }))
      print(fnlSch)
      fnlSch
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
      "A store has multiple ovens and sales profile/forecast for a given day and will have a optimal schedule of cooking for that day"
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
      endTime = getTimeToMin(endTime)
      startTime = getTimeToMin(startTime)
      return((endTime - startTime) > 4 * 60)
    },
    
    planBatchSize = function(subsetSum, ovenIndx, indx, indxEnd, startingValue){
      ks = names(profile)
      val = profile[[ks[indx]]] # time & its forecast
      
      if(notCompliesWithShelfLife(ks[indx], ks[indxEnd])){ # if cook time over 4 hours
        print("1st")
        return(c(indx+1, getTimeToMin(ks[indx+1]), startingValue - as.numeric(profile[[ks[indx+1]]])))
      }else if(subsetSum == 0){ #base case 1: if the subset sum exactly matches the capacity 
        print("2nd")
        return(c(indx,getTimeToMin(ks[indx]),startingValue))
      }else if(subsetSum < 0){ #base case 2 if the subset sum exceeds the oven capacity
        print("3rd")
        return(c(indx+1, getTimeToMin(ks[indx+1]), startingValue - as.numeric(profile[[ks[indx+1]]])))
      }else if(indx == 1){ #base case 3 the current index equals zero
        print("4th")
        return(c(indx,getTimeToMin(ks[indx]),startingValue))
      }else if(val > ovens[[ovenIndx]]$cty & subsetSum == ovens[[ovenIndx]]$cty){ #base case 4 if a measurement exceeds the oven capacity and the subsetSum equals oven capacity
        print("5th")
        return(c(indx-1,getTimeToMin(ks[indx-1]),ovens[[ovenIndx]]$cty)) #???
      }else{ #complex case
        print("6th")
        return(planBatchSize(subsetSum-val, ovenIndx, indx-1, indxEnd, startingValue+val))
      }
    },
    
    planOvens = function(startingValue, sol, indx){
      if(startingValue >= sum(unlist(profile)) | indx == 1){  # ???
        # if start val over total demands (forecast) OR time index (every 15 mins) is 1
        schedules[[length(schedules)+1]] <<- sol
        # print(schedules)
      }else{
        for(i in 1:length(ovens)){
          # print(paste0("Planning for Oven: ", i))
          var = planBatchSize(ovens[[i]]$cty, i, indx, indx, 0) # ovenCapacity, ovenIdx, timeIdx, timeEndIdx, startVal
          sol[[length(sol)+1]] <- c(i, as.numeric(var[2]), as.numeric(var[3])) # ovenId, time, remaining demands
          
          planOvens(startingValue+as.numeric(var[3]), sol, as.numeric(var[1])) # <======== to fix only one oven is used
          sol[[length(sol)]] = NULL # backtrack (?)
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
          sch$addJob(ovens[[jb[1]]], jb[2], jb[3]) 
        }
        mkSpn = sch$getMakespan() # start Time integer negative <============================
        lenSch = length(s)
        if(mkSpn <= drtn){
          if(sch$meetsConstraints(lenSch - 1, lenSch)){
            drtn = mkSpn
            optSch <<- sch
          }
        }
      }
    }
  )
)


# Read Data ---------------------------------------------------------------
# These all need to be read from BIW later
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


# Time Conversion ---------------------------------------------------------
# getCookTimeDiff = function(start, end){
#   if(!is.character(start) | !is.character(end) | nchar(start) != 4 | nchar(end) != 4)  
#     stop("Wrong time format")
#   start.min = as.numeric(substr(start, 1,2)) * 60 + as.numeric(substr(start, 3,4))
#   end.min = as.numeric(substr(end, 1,2)) * 60 + as.numeric(substr(end, 3,4))
#   
#   time.diff = end.min - start.min
#   if(time.diff<0)
#     stop("Wrong time")
#   time.diff
# }

getTimeToMin = function(cooktime){
  # if(!is.character(cooktime) | nchar(cooktime) != 4)  
  #   stop("Wrong time format")
  # cooktime.min = as.POSIXct("00:00", format = "%H:%M") + as.numeric(cooktime) * 60
  cooktime.min = as.numeric(substr(cooktime, 1,2)) * 60 + as.numeric(substr(cooktime, 3,4))
  cooktime.min
}

getMinToTime = function(cooktime){
  # if(!is.numeric(cooktime) | cooktime<0)  
  # stop("Wrong number provided")
  # cooktime.min = paste0(round(cooktime / 60), cooktime%%60)
  cooktime.min = as.POSIXct("00:00", format = "%H:%M") + as.numeric(cooktime) * 60
  cooktime.min
}
# getMinToTime(getTimeToMin("2215"))


# Main Optimizer ----------------------------------------------------------
mainOptimizer = function(updateStoreList = TRUE, updateOvenInfo = TRUE, updateForecast = TRUE){
  
  sm = StoreManager$new()
  
  # Add Stores and their profiles to Store Manager
  if(updateStoreList){
    stores = readStores()[1]
    for(store in stores$V1){
      # store = stores$V1[1]
      profile = readProfile() # <=== This need to be read from BIW later
      profile[, V1 := 525] # Impute hours with no sales
      profile = profile[V1 == store]
      profile[, timeMin := gsub(" ", "", paste(ifelse(4-nchar(V2) > 0, paste(rep(0, 4-nchar(V2)), collapse = ""), ""), V2, collapse = "")), by = .(V1,V2)]
      
      prof = list()
      for(time in profile$timeMin){
        time = profile[timeMin==time]
        prof[as.character(time$timeMin)] = as.numeric(time$V3)
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
  
  return(store)
}



# Test & Run --------------------------------------------------------------
optimal_store = mainOptimizer()
optimal_sch = optimal_store$optSch$printSchedule()
View(optimal_sch)

optional_jobs = unique(optimal_sch[, .(jobId, jobEndTime, batchSize)])
optional_jobs[, batchSizeCum := cumsum(batchSize)]
plot(cumsum(unlist(optimal_store$profile)), 
     x = getMinToTime(getTimeToMin(names(unlist(optimal_store$profile)))), 
     type = 'l')
points(optional_jobs$batchSizeCum, x = optional_jobs$jobEndTime)
