## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Creates Matrix cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#If Inverse for the passed matrix is already available then return it from cache, else calculates,stores in cache and returns it
#Error handling to identify if the passed object is not a valid matrix OR non-inversible matrix (singular matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("Getting cached data ...")
        return(m)
    }
    data <- x$get()
    m <- tryCatch( solve(data, ...) , error = function(err) { print("ERR: Passed object is not a valid matrix or it is a singular matrix, so no inverse")})
    x$setinv(m)
    m
}
