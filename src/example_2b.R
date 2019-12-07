#Example 2b - Debugging type errors

x <- 1
y <- 1:10
x <- x + y
if (x == 2) {
  print ("x is 2")
} else {
  print ("x is not 2")
}