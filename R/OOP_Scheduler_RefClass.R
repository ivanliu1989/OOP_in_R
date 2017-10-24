MyClass <- setRefClass(
  "MyClass",
  fields = list(
    x = "ANY",
    y = "numeric",
    z = "character"
  ),
  methods = list(
    initialize = function(x = NULL, y = 1:10, z = letters)
    {
      "This method is called when you create an instance of the class."
      x <<- x
      y <<- y
      z <<- z
      print("You initialized MyClass!")
    },
    hello = function()
    {
      "This method returns the string 'hello'."
      "hello"
    },
    doubleY = function()
    {
      2 * y
    },
    printInput = function(input)
    {
      if(missing(input)) stop("You must provide some input.")
      print(input)
    }
  )
)