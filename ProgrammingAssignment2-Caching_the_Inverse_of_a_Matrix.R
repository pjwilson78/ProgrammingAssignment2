## Programming Assignment 2
## The purpose of this assignment is to write two functions in the R programming language to cache potentially
## time-consuming computations.  If the contents of a compuation are not changing, it may make sense to cache the value
## of the computation so that when it is needed again, it can be looked up in the cache rather than recomputed.
##
## This assignment also takes advantage of the scoping rules of the R language and how they can be manipulated to preserve
##  state inside of an R object.
##
## The function will use the <<- operator.  The <<- operator assigns a value to an object in an environment that is 
## different from the current environment.
##
## For this assignment,
## Use assignment stub R files obtained from GitHub
## Assume that the matrix x is always invertible.
## Use solve to compute the inverse of a square matrix, solve(x) returns its inverse.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(i) m <<- solve(x)
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m))
        {
                return(m)
        }
        m <- solve(x$get())
        x$setInverse(m)
        m
}

