##These functions work togeter to store the inverse of a matrix so 
##that it doesn't need to be recalculated even if the result is 
##required in a different environment from the one in which the 
##matrix was first solved


##makeCacheMatrix takes a matrix called x, assinges it to the parent
## environment and clears any previously assigned mean (m) value.
##Then it defines the 'getter' function which takes x from the parent
## environment, and the 'setter' function which passes the matrix
## inverse and assignes it to m. Finally, it outputs a list of
##functions for easy access

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {   
    x <<- y
    m <<- NULL
  }
  get <- function() x     
  setInverseMatrix <- function(solve) {
    m <<- solve  
  }
  getInverseMatrix <- function() m
  list(set = set, 
       get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
  
}


## cacheSolve takes the output of makeCacheMatrix. It checks whether
## we have a previously stored variable m and if so, returns it. 
## otherwise it calculates the inverse of the input matrix (X) and
##uses the getInverseMatrix function already defined to set m to
##that value

cacheSolve <- function(x, ...) {
  
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
  
}


