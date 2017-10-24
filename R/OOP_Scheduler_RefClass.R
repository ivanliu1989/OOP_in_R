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
    startTime = "POSIXct",
    endTime = "POSIXct"
  ),
  methods = list(
    initialize = function(opNme, duration, endTime)
    {
      "This method is called when you create an instance of the class."
      opNme <<- opNme
      duration <<- duration
      endTime <<- endTime
      startTime <<- getStartTime()
      print(paste0(opNme, ": ", startTime, ", ", endTime))
    },
    getStartTime = function()
    {
      print("Get Start Time of the Operation")
      startTime <<- endTime - duration
    }
  )
)

# obj1 <- Operation$new()
obj1 <- Operation$new(opNme = "testOps", duration = 1.5 * 3600, endTime = Sys.time() + 3600)
obj1
obj1$endTime = Sys.time() + 1800
obj1
obj1$getStartTime()
obj1


