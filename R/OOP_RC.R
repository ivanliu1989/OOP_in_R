# If you don't want to specify what variable type a field should have, pass "ANY" as the value in the list of fields.
# Any constructor logic needs to be in an optional function called initialize.
# If the first line of a method is a string, it is interpreted as documentation for that method.
# Inside a method, if you want to assign to a field, use global assignment (<<-).
# http://adv-r.had.co.nz/OO-essentials.html#rc

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

obj1 <- MyClass$new()
obj1$hello()
obj1$doubleY()

obj2 <- MyClass$new(x = TRUE, z = "ZZZ")
obj2$printInput("I'm printing a line!")
