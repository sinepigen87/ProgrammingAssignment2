
## R Programmming course, August 2015.
## Programming Assignment 2: Lexical Scoping.

## This file contains two functions that, when used in combination, cache a the inverse of a matrix. 
## If the matrix passed to the functions has been inversed already and thus is cached, the function retrieves the inversed matrix rather than re-computing it.


## The makeCacheMatrix function creates a special "matrix" object that caches its inverse. 
## The matrix, x, is inversed using the solve() function; it is required that the matrix passed to the function is inversible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # the value of the inverse matrix is initially set to NULL
        set <- function(y){ # the value of the matrix is set
                x <<- y 
                m <<- NULL # if the matrix is changed a previously computed inverse matrix is reset to NULL
                }
        get <- function() x # gets the value of the matrix
        setinverse <- function(solve) m <<- solve # sets the value of the inverse
        getinverse <- function() m # gets the value of the inverse 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # passes the values of the function to a list
}


## The special "matrix" returned by makeCacheMatrix is then passed to the cacheSolve, which computes the inverse of the special "matrix". 
## If the inverse has already been computed, the cacheSolve retrieves the inverse from the cache, otherwise it computes the inverse and retrieves it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){ # if the inverse have already been cached, m is no longer preset to NULL and the inverse is retrieved from the cache 
                message("getting cached data...")
                return (m)
        }
        data <- x$get() # if the inverse haven't already been computed, i.e. is set to NULL, the inverse is computed and cached to m
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Example on how the functions can be used; 
#mat<-matrix(c(1:7, 11, 4), 3, 3) # the matrix mat is the input parameter for makeCacheMatrix
#mat
#m1<-makeCacheMatrix(mat) # the special "matrix" m1, returned by makeCacheMatrix, is passed to cacheSolve
#cacheSolve(m1) # the inverse is computed and cached
#cacheSolve(m1) # the inverse has been computed and is retrieved from cache!
