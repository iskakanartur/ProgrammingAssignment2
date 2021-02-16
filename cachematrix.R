## Put comments here that give an overall description of what your
## functions do

## Makes a matrix, with properties (methods as in Python) to get the matrix itself as well as the reverse of it

makeCacheMatrix <- function(x = matrix()) {
  reversed <- NULL
  set <- function(y) {
    x <<- y
    reversed <<- NULL
  }
  get <- function() x
  setrev <- function(inverse) reversed <<- inverse
  getrev <- function() reversed
  list(set = set, get = get, setrev= setrev, getrev= getrev)

}


## If the reversed matrix is present the function just returns it without computing it again, Otherwise computes the reverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  reversed<- x$getrev ()
  if (!is.null(reversed)) {
    message("Getting your reversed matrix from cache")
    return(reversed)
  }
  mt <- x$get ()
  reversed <- solve(mt, ...)
  x$setrev(reversed)
  reversed
}
