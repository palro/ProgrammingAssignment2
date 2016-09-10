## setx creates a function to receive the matrix and cache it in variable x and 
## initialize an empty inverted matrix visible globally
## getx is a function that simply gets the data or matrix stored in x
## setinv stores the invertedmatrix into inv
## getinv gets the inverted matrix set earlier in setinv

## makeCacheMatrix contains functions needed to cache a matrix and store& return the inverted matrix in a new variable 

makeCacheMatrix <- function(x = matrix()) {
                
                inv <- NULL 
                setx <- function(x1) {
                  x <<- x1
                  inv <<- NULL
                }
                getx <- function()x
                
                setinv <- function(invmat) inv <<- invmat
                getinv <- function()inv
                
                list(setx = setx, getx = getx, setinv = setinv, getinv = getinv)
}


## Function to invert the cached matrix if it is not already inverted in makeCacheMatrix

cacheSolve <- function(x, ...) {
  
         y <- x$getx()
         inv <- x$getinv() 
         if(is.null(inv)) {
            
            message("getting cached matrix")
            inv <- solve(y)
            x$setinv(inv)
            inv
          }
          
        ## Return a matrix that is the inverse of 'x'
}
