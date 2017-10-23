summary
show
UseMethod("show")

methods(summary)

# S4
setClass("rectangle", representation(length = "numeric", width = "numeric"))
rect <- new("rectangle",length=10,width=5)
rect



NorthAmerican <- function(eatsBreakfast=TRUE,myFavorite="cereal")
{
  
  ## Get the environment for this
  ## instance of the function.
  thisEnv <- environment()
  
  hasBreakfast <- eatsBreakfast
  favoriteBreakfast <- myFavorite
  
  ## Create the list used to represent an
  ## object for this class
  me <- list(
    
    ## Define the environment where this list is defined so
    ## that I can refer to it later.
    thisEnv = thisEnv,
    
    ## Define the accessors for the data fields.
    getEnv = function()
    {
      return(get("thisEnv",thisEnv))
    },
    
    getHasBreakfast = function()
    {
      return(get("hasBreakfast",thisEnv))
    },
    
    setHasBreakfast = function(value)
    {
      return(assign("hasBreakfast",value,thisEnv))
    },
    
    
    getFavoriteBreakfast = function()
    {
      return(get("favoriteBreakfast",thisEnv))
    },
    
    setFavoriteBreakfast = function(value)
    {
      return(assign("favoriteBreakfast",value,thisEnv))
    }
    
  )
  
  ## Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)
  
  ## Set the name for the class
  class(me) <- append(class(me),"NordAmericain")
  return(me)
}

bubba <- NorthAmerican(myFavorite="oatmeal")
bubba$getFavoriteBreakfast()

bubba$setFavoriteBreakfast("plain toast")
bubba$getFavoriteBreakfast()




makeCopy <- function(elObjeto)
{
  print("Calling the base makeCopy function")
  UseMethod("makeCopy",elObjeto)
  print("Note this is not executed!")
}

makeCopy.default <- function(elObjeto)
{
  print("You screwed up. I do not know how to handle this object.")
  return(elObjeto)
}


makeCopy.NorthAmerican <- function(elObjeto)
{
  print("In makeCopy.NordAmericain and making a copy")
  newObject <- NordAmericain(
    eatsBreakfast=elObjeto$getHasBreakfast(),
    myFavorite=elObjeto$getFavoriteBreakfast())
  return(newObject)
}

bubba <- NorthAmerican(eatsBreakfast=FALSE,myFavorite="oatmeal")
louise <- makeCopy(bubba)
louise$getFavoriteBreakfast()
louise$setFavoriteBreakfast("eggs")
louise$getFavoriteBreakfast()
bubba$getFavoriteBreakfast()


Mexican <- function(eatsBreakfast=TRUE,myFavorite="los huevos")
{
  
  me <- NorthAmerican(eatsBreakfast,myFavorite)
  
  ## Add the name for the class
  class(me) <- append(class(me),"Mexican")
  return(me)
}
mexican = Mexican()
