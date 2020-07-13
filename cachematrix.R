## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##We create a function for cache matrix, we set some empty object "inv" 
##for te inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y){
                    x <<- y
                    inv <<- NULL
          }

##First we took the matrix and set the inverse of that matrix and get the inverse
          
          get <- function()x
          setInverse <- function(inverse) inv <<- inverse
          getInverse <- function() inv 
          list(set = set, get = get, 
               setInverse = setInverse, 
               getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          ##We took now the inverse "inv" and give the answer to the problem
          inv <- x$getInverse()
          if(!is.null(inv)){
                    return(inv)
          }
          mat <- x$get()
          inv <- solve(mat,...)
          x$setInverse(inv)
          inv
}
