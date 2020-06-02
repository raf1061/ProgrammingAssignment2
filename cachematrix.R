##We will assume that the matrix given is always invertible that is determinant(matrix)!=0
##here two functions are written that will cache the inverse of the given matrix


##makeCacheMatrix() function will create a special "matrix" type object 
##that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setInverse <- function(inverse){
    j <<- inverse
  }
  
  getInverse <- function() {
    j
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


##cacheSolve() function will compute the inverse of the special "matrix" that is returned by makeCacheMatrix() function above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve() should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting the cached data")
    return(j)
  }
  mat <- x$get()
  ##inverse of a square matrix can be computed using solve() function. 
  ##For example, if z is a square invertible matrix, then solve(z) returns its inverse
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
