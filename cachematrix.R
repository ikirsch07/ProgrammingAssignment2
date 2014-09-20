## General Description:
##
## makeCacheMatrix maps an invertible matrix 'A' to a 
## list of functions with names "set", "get", "setsolve",
## and "getsolve". Let's denote this list by 'mat'.
## cacheSolve takes the list of functions 'mat' and 
## returns the inverse of 'A'.
##
## They should be used as follows:
##
##  A<-matrix(c(2,1,1,2), ncol=2, nrow=2) #define some invertible matrix A
##  mat<-makeCacheMatrix(A)               #generate the list of functions for A 
##  cacheSolve(mat)                       #computes and returns the inverse of A
##  cacheSolve(mat)                       #when called a second time, cacheSolve
##                                        #returns the cached inverse of A

## makeCacheMatrix:
## makeCacheMatrix generates the list of functions (symbolically)
## ( set(), get(), setsolve(),and getsolve() ) to
##  1. set the matrix [set()]
##  2. get the matrix [get()]
##  3. set the inverse matrix [setsolve()] 
##  4. get the inverse matrix [getsolve()]
## Comment:
## The opererator <<- in "m <<- solve" means that the variable m 
## of the parent environment (that is the environment of the function
## makeCacheMatrix) will receive the value of solve.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve:
## The variable 'm' will contain the inverse matrix. 'x' is the
## list of functions produced by makeCacheMatrix and handed over to 
## cacheSolve. The routine first checks whether the inverse matrix 
## (stored in x$getsolve()) has already been computed. If so, it returns 
## the cached inverse matrix. If not, it assigns the original matrix 
## stored in x$get() to the variable 'data' and then computes the 
## inverse matrix using the solve() command. It finally stores the
## result in x$setsolve() and returns the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

