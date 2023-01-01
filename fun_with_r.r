# Matrix of lists
# This doesn't create a matrix of lists as you would expect...
m <- matrix(list(a = 1, b = 2), nrow=5, ncol=5)

#     [,1] [,2] [,3] [,4] [,5]
#[1,] 1    2    1    2    1   
#[2,] 2    1    2    1    2   
#[3,] 1    2    1    2    1   
#[4,] 2    1    2    1    2   
#[5,] 1    2    1    2    1 
#Warning message:
#In matrix(list(a = 1, b = 2), nrow = 5, ncol = 5) :
#  data length [2] is not a sub-multiple or multiple of the
# number of rows [5]

# You have to wrap the list with another list (??)
m <- matrix(list(list(a = 1, b = 2)), nrow=5, ncol=5)

#     [,1]   [,2]   [,3]   [,4]   [,5]  
#[1,] list,2 list,2 list,2 list,2 list,2
#[2,] list,2 list,2 list,2 list,2 list,2
#[3,] list,2 list,2 list,2 list,2 list,2
#[4,] list,2 list,2 list,2 list,2 list,2
#[5,] list,2 list,2 list,2 list,2 list,2


# Iterating through a string with strsplit
my_str <- "hello"
for (ch in strsplit(my_str, "")) {
  print(ch)
  print("---")
}
# This prints:
# [1] "h" "e" "l" "l" "o"
# [1] "---"
# That's not what I wanted...

# To actually iterate through the string, add a [[1]] at the end of
# strsplit (??)
# see: https://stackoverflow.com/q/26721340
# It has something to do with the fact that my_str is
# "a character vector of length 1" according to the stack overflow
# answer linked above
# "Another instance of R making something needlessly complex"
# - Quantum7, Stack Overflow user
for (ch in strsplit(my_str, "")[[1]]) {
  print(ch)
  print("---")
}
#[1] "h"
#[1] "---"
#[1] "e"
#[1] "---"
#[1] "l"
#[1] "---"
#[1] "l"
#[1] "---"
#[1] "o"
#[1] "---"

# Return
# Why is return a function(!) and not a keyword?
# It's cool that the last evaluated expression is returned, e.g:
fun <- function(x) {
  x + 3 / 2
}
# But I prefer the python way "explicit is better than implicit"
fun2 <- function() {
  return(
  	"Since return is a function, the
  	returned vars *must* be wrapped in parenthesis...
  	Also, I just learned that you can split your strings into
  	new lines and R will automatically set the \\n,
  	that's cool"
  )
}

# R, as far as I know, doesn't have a way to pass variables by reference,
# so that leads to ugly code where I pass the variable as a parameter,
# modify it and then return it and save the changes in the original
# variable

a <- list(x = 3, y = 4)
mod_a <- function(a) {
  a$x <- a$x + 1
  a$y <- a$y + 1
}
mod_and_ret_a <- function(a) {
  a$x <- a$x + 1
  a$y <- a$y + 1
  return(a)
}
mod_a(a)
#$x
#[1] 3
#$y
#[1] 4
a <- mod_and_ret_a(a)
#$x
#[1] 4
#$y
#[1] 5

# If the function modifies more than one variable, I have to return
#a list with those variables
# R is not the only language that does this, I remember in Racket
# you also couldn't pass as reference


# There are some built-in functions with very ugly names, e.g:
# paste -> turns its arguments into strings and concatenates them
# runif -> RandomUNIForm
# c -> "generic function which combine its arguments"