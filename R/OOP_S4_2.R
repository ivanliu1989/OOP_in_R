######################################################################
# Create the Prey class
#
# This is used to represent a prey animal
Prey <- setClass(
  # Set the name for the class
  "Prey",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>70.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Agent"
)



######################################################################
# Create the Bobcat class
#
# This is used to represent a smaller predator
Bobcat <- setClass(
  # Set the name for the class
  "Bobcat",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>85.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Agent"
)

######################################################################
# Create the Lynx class
#
# This is used to represent a larger predator
Lynx <- setClass(
  # Set the name for the class
  "Lynx",
  
  # Define the slots - in this case it is empty...
  slots = character(0),
  
  # Set the default values for the slots. (optional)
  prototype=list(),
  
  # Make a function that can test to see if the data is consistent.
  # This is not called if you have an initialize function defined!
  validity=function(object)
  {
    if(sum(object@velocity^2)>95.0) {
      return("The velocity level is out of bounds.")
    }
    return(TRUE)
  },
  
  # Set the inheritance for this class
  contains = "Bobcat"
)


# create a method to move the agent.
setGeneric(name="move",
           def=function(theObject)
           {
             standardGeneric("move")
           }
)

setMethod(f="move",
          signature="Agent",
          definition=function(theObject)
          {
            print("Move this Agent dude")
            theObject <- setVelocity(theObject,c(1,2))
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Prey",
          definition=function(theObject)
          {
            print("Check this Prey before moving this dude")
            theObject <- callNextMethod(theObject)
            print("Move this Prey dude")
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Bobcat",
          definition=function(theObject)
          {
            print("Check this Bobcat before moving this dude")
            theObject <- setLocation(theObject,c(2,3))
            theObject <- callNextMethod(theObject)
            print("Move this Bobcat dude")
            validObject(theObject)
            return(theObject)
          }
)

setMethod(f="move",
          signature="Lynx",
          definition=function(theObject)
          {
            print("Check this Lynx before moving this dude")
            theObject <- setActive(theObject,FALSE)
            theObject <- callNextMethod(theObject)
            print("Move this Lynx dude")
            validObject(theObject)
            return(theObject)
          }
)

robert <- Bobcat()
robert <- move(robert)
lionel <- Lynx()
lionel <- move(lionel)
