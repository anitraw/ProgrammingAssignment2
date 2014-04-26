##  R Programming Assignment 2
#
## The aim of this program was to write a function to store and retrieve a 2x2 
#  matrix in order to use it repeatedly in a loop.  Caching is a way of saving 
#  time when computing larger vectors.  
#
## Note:  use of <<- operator which is used to assign a value to an object 
#  in an environment which is different to the current environment.
#
##   MAKE CACHE FOR MATRIX
#
# Create a function to make cache, then an empty vector (mtx) to store each
# matrix.  Next create a function to set the value of (y) and define the output
# in a list.
#
makeCacheMatrix <- function(x = matrix()) {   
            mtx  <-  NULL                 
            set  <-  function(y) {        
                        x <<- y               
                        mtx <<- NULL
}
        get  <-  function()x          
        setmatrix  <-  function(matrix) mtx <<- matrix
        getmatrix  <-  function ()mtx
        list(set = set, get = get,
                  setmatrix = setmatrix,
                  getmatrix = getmatrix)
}
#
##   Testing the function and loop: by using a 2x2 matrix with 4 numbers.
#
a  <- makeCacheMatrix(matrix(1:4, 2))  
class(a)                       
[1] "list"
#
a$get()                        # returns a 2x2 matrix - so this part is corrrect.  
[,1] [,2]
[1,]    1    3
[2,]    2    4
#
##  MAKE A FUNCTION TO SOLVE THE INVERSE OF ABOVE MATRIX
#
## I now want to write a function to return the inverse of the matrix above. 
#  In R this is done by using the "solve()" function.
#
cacheSolve <- function(x, ...) {
            mtx <- x$getmatrix()            #  query x vector's cache
# 
if(!is.null(mtx)) {                   
            
# if there is a cache found in previous line it will state this message and 
# will return to the cache (mtx).
#                            
            message("getting cached data")    
            return(mtx)                 
}
#
# However, if there is no cache, the function will move down to this loop which
# will compute the inverse using the solve() function, then save it back to x's
# cache and return the result to (mtx).
#
            data <- x$get()                  
            mtx <- solve(data, ...)           
            x$setmatrix(mtx)                  
            mtx                               
}
#
##  Testing the function and loop: by using a 2x2 matrix with 4 numbers.
#
a$set(matrix(5:8,2))                  # setting a new matrix of 2x2 and 4 numbers
a$get()                               # returns a 2x2 matrix - good!
[,1] [,2]
[1,]    5    7
[2,]    6    8
#
cacheSolve(a)                         # now try for the inverse of a$get
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
cacheSolve(a)                         # and repeat to check that the message appears.
getting cached data
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
#
#  And lastly, just a quick check that I have the correct inverse matrix
a$getmatrix()                         
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
#
#
### Finally I can double check that the functions and loops are working appropriately 
#   by testing mathematically. Create a new matrix (b) and then use '%*%' to 
#   multiply the two matrices.  (b) will become an identity matrix by default.
#
b  <- a$getmatrix()
a$get() %*% b                 
[,1]         [,2]
[1,]    1 3.552714e-15
[2,]    0 1.000000e+00
#
# Matrix multiplication gives the 0,1/1,0 identity configuration.  
# I'm happy it is working so now I can trial it out on larger matrices and data.
#
## *****************************************************************************
##  With special thanks to fellow students whose posts on the forum were of 
#   great assistance:
##  Fu Sheng Wang, David Burton and Richard Ambler (and of course, Fido the dog). 
#
##  Refs:  "Darren Wilkinson's Research Blog: Lexical Scoping and function closures in R"
##  sourced from: http://darrenjw.wordpress.com/2011/11/23/lexical-scope-and-function-closures-in-r/
##  Wickham, Hadley:  "Advanced R Programming" sourced from: http://adv-r.had.co.nz/

