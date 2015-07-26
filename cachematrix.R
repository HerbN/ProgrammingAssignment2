## This is a pair of functions to create a container object for large matrices
## and allow certain values for them, initially just the inverse, to be precomputed
## allowing higher programming speed by avoiding repeated calculation

## makeCacheMatrix creates a list container with:
##	1. A matrix
##	2. Four functions:
##    a. Get and Set for the matrix
##    b. Get and Set for the matrix inverse
##	3. A cached version of the inverse

makeCacheMatrix <- function(x = matrix()) {
   ## Storage for pre-computed inverse of the matrix
   ## Note, evaluation is lazy...the cached version is only computered
   ## the first time it is requested
	inverted <- NULL

   ## Getter and setter for the matrix itself
   get <- function() x
   set <- function(y) {
      x <<- y
      inverted <<- NULL
      }

   ## Getter and setter for the inverse
   get_inverse <- function() inverted
   set_inverse <- function(inverse) inverted <<- inverse

   ## Create the containing list and return it
   list ( set = set,
          get = get,
          set_inverse = set_inverse,
          get_inverse = get_inverse
        )
}


## This function takes a cached matrix created by makeCacheMatrix
## and returns the inverse.  If the inversed has already been cached
## it returns the cached value.  Otherwise it calculates it, caches it,
## and returns the cached value.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inverted <- x$get_inverse()
   if ( ! is.null(inverted) ) {
      ## We have a cached value so just return it
      message("getting cached inverse")
      return(inverted)
      }

   ## The inverse has not been calculated so calculate it, cache it,
   ## and return it.
   data <- x$get()
   inverted <- solve(data, ...)
   x$set_inverse(inverted)

   ## If we are not returning the result of the last operation
   ## I prefer an explict return over merely referencing the value
   ## to be returned
   return(inverted)
}
