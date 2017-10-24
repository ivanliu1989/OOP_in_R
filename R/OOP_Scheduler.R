
# Operation ---------------------------------------------------------------
Operation <- setClass(
  
  "Operation",
  
  slots = c(
    opNme = "character",
    duration = "numeric",
    startTime = "POSIXct",
    endTime = "POSIXct"
  ),
  
  prototype=list(),
  
  validity=function(object)
  {
    if(is.character(object@opNme) & is.numeric(object@duration) & is.na.POSIXlt(object@startTime) & is.na.POSIXlt(object@endTime)){
      cat(paste0(object@opNme, ": ", object@startTime, ", ", object@endTime))
      return(TRUE)  
    }else{
      return(cat(paste0(object@opNme, ": ", object@startTime, ", ", object@endTime)))  
    }
  }
)

# Func - getStartTime
setGeneric(name="getStartTime",
           def=function(theObject)
           {
             standardGeneric("getStartTime")
           }
)
setMethod(f="getStartTime",
          signature="Operation",
          definition=function(theObject)
          {
            print("Get Start Time of the Operation")
            theObject@startTime <- theObject@endTime - theObject@duration
            validObject(theObject)
            return(theObject)
          }
)

x = Operation(opNme = "testOps", duration = 1.5 * 3600, startTime = Sys.time(), endTime = Sys.time() + 3600)
x = getStartTime(x)



# Job ---------------------------------------------------------------------
Job <- setClass(
  
  "Job",
  
  slots = c(
    oven = "Oven",
    endTime = "POSIXct",
    batchSize = "numeric",
    ops = "Operation"
  ),
  
  prototype = list(),
  
  validity = function(object)
  {
    
  }
)

