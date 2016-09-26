## Put comments here that give an overall description of what your
## functions do

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
## If the inverse has already been calculated (and the matrix has not changed),  
## then cacheSolve will retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) 
    {
     inv <- NULL                                      ## inv as NULL; will hold value of matrix inverse   
     
     set <- function(y)                               ## function to assign new value of matrix in parent environment 
            {
             x <<- y
             inv <<- NULL                             ## if there is a new matrix, reset inv to NULL  
            }
     
     get <- function() x                              ## returns value of the matrix argument
     
     setInverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environmen
     getInverse <- function() inv                     ## gets the value of inv where called
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse
         )
    }


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
    {
     inv <- x$getInverse()
     if (!is.null(inv))                               ## check if inv holds cached of matrix inverse
     {
         message("getting cached data")               ## if cache of matrix inverse exist, print a message
         return(inv)                                  ## if cache of matrix inverse exist, print the matrix inverse value
     }
     mat <- x$get()                                   ## assign a metrix to variable
     inv <- solve(mat, ...)                           ## find the matrix inverse using solve() function
     x$setInverse(inv)                                ## assign matrix inverse value to variable inv to cache
     inv                                              ## print the matrix inverse value
    }
