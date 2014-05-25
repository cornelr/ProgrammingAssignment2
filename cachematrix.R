
## Create a special object that stores a numeric vector and caches its mean.


## makeCacheMatrix:
##   1. set the value of the vector
##   2. get the value of the vector
##   3. set the value of the mean
##   4. get the value of the mean

makeCacheMatrix <- function(x = numeric()) {
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


## Calculate the mean of a special "vector" 
## Check to see if the mean has already been calculated. 
##  If so, it gets the mean from the cache and skips the computation. 
##  Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve<- function(x, ...) {
    m <- x$getmean()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}