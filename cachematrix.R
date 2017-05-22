## The two pairs of functions cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        makeCacheMatrix <- function(x = matrix()) {
                mat_inv <- NULL
                set <- function(y) {
                        x <<- y
                        mat_inv <<- NULL
                }
                get <- function() x
                set_inverse <- function(inverse) mat_inv <<- inverse
                get_inverse <- function() mat_inv
                list(set = set, get = get,
                     set_inverse = set_inverse,
                     get_inverse = get_inverse)
        }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        cacheSolve <- function(x, ...) {
                mat_inv <- x$getInverse()
                if(!is.null(mat_inv)) {
                        message("getting cached data")
                        return(mat_inv)
                }
                mat <- x$get()
                mat_inv <- solve(mat, ...)
                x$setInverse(mat_inv)
                mat_inv
        }