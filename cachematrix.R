## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
        invrs <- NULL
        set <- function(y)
        {
                x <<- y
                invrs <<- NULL
        }
        }
        get <- function () {x}
        {
        setInverse <- function (inverse)(invrs <<- inverse)
        getInverse <- function (invrs)
        list (set =set, get = get, setInverse = setInverse, getInverse = getInverse)
        }
}

## Function that cache the inverse

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if(!is.NULL(invrs))
        {
                message ("getting cached data")
                return(invrs)
        }
        matx <- x$get()
        invrs <- solve (matx, ...)
        x$setInverse(invrs)
        invrs
}
