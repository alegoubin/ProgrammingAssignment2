## Function to handle cached matrix
makeCacheMatrix <- function(mat = matrix()) {
  ## mat has to be a square matrix to work
  
  ## initialize inverted matrix to null in current scope
  matinv <- NULL
  
  ## set value to calling environment
  set <- function(y) {
    mat <<- y
    matinv <<- NULL
  }
  
  ## retrieve object
  get <- function() mat
  
  ## set inverted matrix once calculated
  setinv <- function(solve) matinv <<- solve
  
  ## attempt to retrieve stored inverted matrix
  getinv <- function() {matinv}
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##function that either retrieves from cache or computes inverted matrix
cacheSolve <- function(mat, ...) {
  ## mat is the matrix used by makeCacheMatrix function
  
  ## retrieve inverted matrix from cache if exists
  invertedM <- mat$getinv()
  
  if(!is.null(invertedM)) {
    message("getting cached inverted matrix")
    return(invertedM)
  }
  else
  {
    
    ## otherwise extract stored matrix
    matget <- mat$get()
    
    ## and compute inverted matrix
    invertedM <- solve(matget)
    mat$setinv(invertedM)
    
    message("stored inverted matrix in cache")
    
  }  
  
  
  
}