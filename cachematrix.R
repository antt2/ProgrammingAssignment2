##A pair of functions that find the matrix inverse and cache the result.


##Makes a pseudo vector which holds matricies and their calculated inverses. 
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




##Finds the inverse of a matrix pseudo vector and stores the inverse.
cacheSolve <- function(x, ...) {
        ## Return a    cachemean <- function(x, ...) {
            m <- x$getinv()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data)
            x$setinv(m)
            m
}
