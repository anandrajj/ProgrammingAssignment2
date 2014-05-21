## Below 2 functions perform the tasks for calcualting inverse of matrix and 
## caching the result. If inverse of same matrix is calculated again, then
## result from cache is returned instead of recalulating.

## Below makeCacheMatrix() function takes a matrix as input and return functions
## list that can be performed on that matrix like savaing to chace and
## retreiving from cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Below function uses the function list returned by makeCacheMatrix() to gets 
## the cached inverse of matrix. If inverse matrix is not available in cache 
## then it calcualtes the inverse using solve(x) functions, sets the same to
## cache using setmatrix() before returning it.

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmatrix(m)
    m
}
