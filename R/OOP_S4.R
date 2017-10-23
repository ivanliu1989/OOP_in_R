######################################################################
# Create the first quadrant class
#
# This is used to represent a coordinate in the first quadrant.
FirstQuadrant <- setClass(
  # Set the name for the class
  "FirstQuadrant",
  
  # Define the slots
  slots = c(
    x = "numeric",
    y = "numeric"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    x = 0.0,
    y = 0.0
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if((object@x < 0) || (object@y < 0)) {
      return("A negative number for one of the coordinates was given.")
    }
    return(TRUE)
  }
)

x <- FirstQuadrant()
y <- FirstQuadrant(x = 5, y = 7)
y@x
y <- FirstQuadrant(x = 5, y = -7)


# create a method to assign the value of a coordinate
setGeneric(name="setCoordinate",
           def=function(theObject,xVal,yVal)
           {
             standardGeneric("setCoordinate")
           }
)

setMethod(f="setCoordinate",
          signature="FirstQuadrant",
          definition=function(theObject,xVal,yVal)
          {
            theObject@x <- xVal
            theObject@y <- yVal
            return(theObject)
          }
)

z <- FirstQuadrant(x=2.5,y=10)
z <- setCoordinate(z,-3.0,-5.0)







######################################################################
# Create the base Agent class
#
# This is used to represent the most basic agent in a simulation.
Agent <- setClass(
  # Set the name for the class
  "Agent",
  
  # Define the slots
  slots = c(
    location = "numeric",
    velocity   = "numeric",
    active   = "logical"
  ),
  
  # Set the default values for the slots. (optional)
  prototype=list(
    location = c(0.0,0.0),
    active   = TRUE,
    velocity = c(0.0,0.0)
  ),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>100.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  }
)
a = Agent()
a
is.object(a)
isS4(a)
slotNames(a)
getSlots("Agent")
slotNames("Agent")
getClass(a)

slot(a,"location")
slot(a,"location") <- c(1,5)
a@location <- c(2,3)


# create a method to assign the value of the location
setGeneric(name="setLocation",
           def=function(theObject,position)
           {
             standardGeneric("setLocation")
           }
)

setMethod(f="setLocation",
          signature="Agent",
          definition=function(theObject,position)
          {
            theObject@location <- position
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the location
setGeneric(name="getLocation",
           def=function(theObject)
           {
             standardGeneric("getLocation")
           }
)

setMethod(f="getLocation",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@location)
          }
)


# create a method to assign the value of active
setGeneric(name="setActive",
           def=function(theObject,active)
           {
             standardGeneric("setActive")
           }
)

setMethod(f="setActive",
          signature="Agent",
          definition=function(theObject,active)
          {
            theObject@active <- active
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of active
setGeneric(name="getActive",
           def=function(theObject)
           {
             standardGeneric("getActive")
           }
)

setMethod(f="getActive",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@active)
          }
)


# create a method to assign the value of velocity
setGeneric(name="setVelocity",
           def=function(theObject,velocity)
           {
             standardGeneric("setVelocity")
           }
)

setMethod(f="setVelocity",
          signature="Agent",
          definition=function(theObject,velocity)
          {
            theObject@velocity <- velocity
            validObject(theObject)
            return(theObject)
          }
)

# create a method to get the value of the velocity
setGeneric(name="getVelocity",
           def=function(theObject)
           {
             standardGeneric("getVelocity")
           }
)

setMethod(f="getVelocity",
          signature="Agent",
          definition=function(theObject)
          {
            return(theObject@velocity)
          }
)

a <- Agent()
getVelocity(a)
a <- setVelocity(a,c(1.0,2.0))

# create a method to reset the velocity and the activity
setGeneric(name="resetActivity",
           def=function(theObject,value)
           {
             standardGeneric("resetActivity")
           }
)

setMethod(f="resetActivity",
          signature=c("Agent","logical"),
          definition=function(theObject,value)
          {
            theObject <- setActive(theObject,value)
            theObject <- setVelocity(theObject,c(0.0,0.0))
            return(theObject)
          }
)

setMethod(f="resetActivity",
          signature=c("Agent","numeric"),
          definition=function(theObject,value)
          {
            theObject <- setActive(theObject,TRUE)
            theObject <- setVelocity(theObject,value)
            return(theObject)
          }
)

a <- resetActivity(a,FALSE)
getActive(a)
a <- resetActivity(a,c(1,3))
getVelocity(a)
