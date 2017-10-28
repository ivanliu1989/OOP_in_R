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
obj1 <- Operation$new(opNme = "testOps", duration = 1.5 * 3600)
obj1
obj1$endTime = Sys.time() + 1800
obj1
obj1$getStartTime()
obj1



# Oven --------------------------------------------------------------------
Oven <- setRefClass(
  "Oven",
  fields = list(
    cty = "numeric",
    cookTime = "numeric",
    washCycleLength = "numeric"
  ),
  methods = list(
    initialize = function(cty, cookTime, washCycleLength)
    {
      "This method is called when you create an instance of the class."
      cty <<- cty
      cookTime <<- cookTime
      washCycleLength <<- washCycleLength
    }
  )
)
oven1 <- Oven$new(cty = 1, cookTime = 120, washCycleLength = 10)

# Schedule ----------------------------------------------------------------
Schedule <- setRefClass(
  "Schedule",
  fields = list(
    jobs = "ANY"
  ),
  methods = list(
    initialize = function(jobs)
    {
      "This method is called when you create an instance of the class."
      jobs <<- list()
    },
    addJob = function(oven, endTime, batchSize){
      newJob = Job$new(oven, endTime, batchSize)
      newJob$jobId = length(jobs)
      jobs[length(jobs)+1] = newJob
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
        tm <<- i$getDuration + tm
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
    profile = "ANY",
    ovens = "list",
    schedules = "list",
    forecast = "numeric",
    optSch = "ANY"
  ),
  methods = list(
    initialize = function(storeID, profile){
      storeId <<- storeId
      ovens <<- list()
      schedules <<- list()
      profile <<- profile
      forecast <<- 0
      optSch <<- NA
    },
    
    addOven = function(cty, cookTime, washCycleLength){
      newOven = Oven(cty, cookTime, washCycleLength)
      newOven$ovenId = length(ovens)
      ovens[length(ovens)+1] = newOven$ovenId
    },
    
    notCompliesWithShelfLife = function(startTime, endTime){
      endTime = as.POSIXct(endTime)
      startTime = as.POSIXct(startTime)
      return((endTime - startTime) > 4 * 3600)
    },
    
    planBatchSize = function(subsetSum, ovenIndx, indx, indxEnd, startingValue){
      ks = list(profile$keys)
      val = profile[ks[indx]]
      
      if(notCompliesWithShelfLife(ks[indx], ks[indxEnd])){
        return()
      }else if(subsetSum == 0){ #base case 1: if the subset sum exactly matches the capacity 
        
      }else if(subsetSum < 0){ #base case 2 if the subset sum exceeds the oven capacity
        
      }else if(indx == 0){ #base case 3 the current index equals zero
        
      }else if(val > ovens[ovenIndx]$cty & subsetSum == ovens[[ovenIndx]]$cty){ #base case 4 if a measurement exceeds the oven capacity and the subsetSum equals oven capacity
        
      }else{ #complex case
        
      }
    },
    
    planOvens = function(startingValue, sol, indx){
      if(startingValue >= sum(profile$values) | indx == 0){
        schedules[length(schedules) + 1] <- sol
        # print(schedules)
      }else{
        for(i in 1:length(ovens)){
          var = planBatchSize(ovens[[i]]$cty, i, indx, indx, 0)
          sol[length(sol)+1] <- list(i, var[2], var[3])
          planOvens(startingValue+var[3], sol, var[1])
          sol[[length(sol)]] = NULL # backtrack
        }
      }
    },
    
    trySchedules = function(){
      drtn = 999999999
      for(s in schedules){
        sch = Schedule$new()
        for(jb in s){
          sch$addJob(ovens[[jb[1]]], jb[2], jb[3])
        }
        mkSpn = sch.getMakespan()
        lenSch = length(s) - 1
        if(mkSpn <= drtn){
          if(sch$meetsConstraints(lenSch - 1, lenSch)){
            drtn = mkSpn
            optSch <<- sch
            print(mkSpn)
          }
        }
      }
    }
  )
)

Store$new()

# Job ---------------------------------------------------------------------
Job <- setRefClass(
  "Job",
  fields = list(
    oven = "ANY",
    endTime = "POSIXct",
    batchSize = "numeric",
    jobId = "ANY"
  ),
  methods = list(
    initialize = function(oven, endTime, batchSize)
    {
      endTime <<- endTime
      jobId <<- NA
      oven <<- oven
      ops <<- list(unload = Operation$new(opNme = "unload", duration = 15),
                   cook = Operation$new(opNme = "cook", duration = oven$cookTime),
                   load_and_sprinkle = Operation$new(opNme = "load and sprinkle", duration = 15),
                   rinse = Operation$new(opNme = "rinse", duration = oven$washCycleLength)
                   )
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
      startTime = ops[[length(ops)-1]] 
      return(startTime)
    },
    
    jobFollowsJob = function()
    {
      if(oven == prevJob$oven){
        return(ops[[length(ops)-1]]$startTime >= prevJob$endTime)
      }else{
        return(TRUE)
      }
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
    initialize = function(stores, maxTime)
    {
      "This method is called when you create an instance of the class."
      stores <<- list()
      maxTime <<- 99999
    },
    
    addStore = function(){
      newStore <<- Store$new(storeId, profile)
      stores[length(stores)+1] <- newStore
    }
  )
)
oven1 <- Oven$new(cty = 1, cookTime = 120, washCycleLength = 10)

