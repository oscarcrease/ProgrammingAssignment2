#these functions save time on matrix inversion by caching the inverse of a matrix, so you dont have to compute it repeatedly


#makeCacheMatrix creates a special "matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   set <- function(y){
           x <<- y
           m <<- NULL
   }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}
#cachesolve computes the inverse of the special "matrix" retunred by makeCacheMatrix above.
#if the inverse has already been calculated then the cachesolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

