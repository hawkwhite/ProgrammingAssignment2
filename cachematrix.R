## Put comments here that give an overall description of what your
## functions do

## This function will create a special matrix which is a list with functions to :
## 1 - set the value of the matrix
## 2 - get the value of the matrix
## 3 - set the value of the solved (inverted) matrix
## 4 - get the value of the solved (inverted) matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setsolved <- function(solve) m <<- solve
        getsolved <- function() m
        list(set = set, get = get,
             setsolved = setsolved,
             getsolved = getsolved)

}


## This function solves (inverts) a matrix, but checks beforehand if that value is 
## cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolved()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolved(m)
        m
}
