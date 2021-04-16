## The following code is used to create a special matrix that stores a matrix and caches its inverse.
## The makeCacheMatrix contains a function that first sets the value of the matrix and assumes the matrix is invertible.
## Then makeCacheMatrix will get the value of the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}
## The CacheSolve matrix computes the inverse of the matrix created by makeCacheMatrix
## If X is a square invertible matrix, then the solve(x) function should return its inverse.
## If the inverse has already been calculated, then the CacheSolve function should retrieve the inverse from the cache.

CacheSolve <- function (x,...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat,...)
  x$setinverse(inv)
  inv
}