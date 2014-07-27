## Computing inverse martix with caching result.
## Example for test in console:
##  m <- matrix(1:4, nrow=2, ncol=2)
##  buff <- makeCacheMatrix(m)
##  cacheSolve(buff) ##calculate and cache inverse martix, then return result
##  cacheSolve(buff) ##return inverse matrix from cache 
##  m %*% cacheSolve(buff) ##check the inverse martix: should be identity matrix
##  buffNew <- makeCacheMatrix(m) ##make a new buffer
##  cacheSolve(buffNew) ##calculate result for new object
##  cacheSolve(buffNew) ##return inverse matrix from cache
##  c ##check if old cache still valid 
## Although buffNew gets the same matrix as buff, cacheSolve(buffNew) will
## calculate the inverse matrix one more time.     

## Function makeCacheMatrix return list of four funcions. 
## This list should be used as a argument of cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Function cacheSolve get
##  x: list of four function, which should be obtained by makeCacheMatrix call 
##  ...: extra arguments for solve() functioin
## and return an inverse matrix. 
## If inverse matrix has already been calculated (cacheSolve was called with)
## current argument x, then cacheSolve return inverse martix from chache.
cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")        
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
