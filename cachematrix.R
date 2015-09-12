## Functions provided below serve the goal to cache an inverse matrix. 
## The first function create a matrix and can cache the inverse.
## The second function computes the inverse matrix, or, is able to retrieve this
## inverse matrix from cache, if it has been calculated previously.

## Function makeCacheMatrix serves the goals of (1) setting/getting a matrix and
## (2) setting/getting the inverse matrix of the first one

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## Function cacheSolve serves the goals of (1) calculating the inverse matrix 
## provided or (2) in case this calculation was done so previously, retrieving
## this calculated inverse matrix from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
