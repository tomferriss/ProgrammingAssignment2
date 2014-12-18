## Put comments here that give an overall description of what your
## functions do: These functions create matrix-like objects that can store/cache
# its own inverse to avoid unnecessary computations.
# the other function calculates the inverse (if this hasn't been done yet)
# or simply returns the cached inverse if it has already been calculated.

## Write a short comment describing this function: This function returns an object
#that can be thought of as a matrix but is actually a list of four functions
# the matrix itself is stored in the get function, so that M$get() returns the
# originally inputted matrix M. The other functions allow rewriting the matrix 
# and storing (caching) and retrieving the inverse of the matrix, once it has been
# calculated. The setInv function uses the <<- operator to store the inverse in 
# this object's environment even when called from other environments

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function: this function takes a cacheMatrix
#object created with the first function and retrieves what it has stored as its inverse
# if the stored inverse isn't null, it just returns that inverse without any real
# further computational work. Otherwise it inverts the matrix and stores the inverse
# and returns the newly calculated inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}




