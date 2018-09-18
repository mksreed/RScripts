makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
x=1:6
xx=makeVector(x)
cachemean(xx)
##########################################################
makeCacheMatrix <- function(x = numeric()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
        minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached inverse")
                return(minv)
        }
        invdata <- x$get()
        minv <- solve(invdata, ...)
        x$setinverse(minv)
        minv
}

x=matrix(1:4,nrow=2)
xx=makeCacheMatrix(x)
cacheSolve(xx)
