## To test this function :
## -------------------------
##    > A <- matrix( c ( 1, 2, 2, 1 ), nrow=2, byrow=TRUE) 
##    # Note : A can be any invertible matrix you want
##    #        To be really useful, try it with a large matrix,
##    #        for which computing the inverse of the matrix is long

##    > B <- makeCacheMatrix(A)
##    # Note : Initialization of the object

##    > cacheSolve(B)
##    [,1]       [,2]
##    [1,] -0.3333333  0.6666667
##    [2,]  0.6666667 -0.3333333
##    # Note : Compute inverse of the matrix a first time
##

##    > cacheSolve(B)
##    getting cached data
##    [,1]       [,2]
##    [1,] -0.3333333  0.6666667
##    [2,]  0.6666667 -0.3333333## Return a matrix that is the inverse of 'x'
##    # Note : Used the inverse matrix which was cached
##



## This function creates a special "matrix" object that can cache its inverse
#-------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinversematrix <- function(im) inverse_matrix <<- im
  getinversematrix <- function() inverse_matrix
  list(set = set, 
       get = get, 
       setinversematrix = setinversematrix, 
       getinversematrix = getinversematrix)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache
#-------------------------------------------
cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}