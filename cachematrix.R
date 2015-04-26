## First function creates the list of 4 functions. These are set (the matrix), get(the matrix),
## setinverse (of the matrix), getinverse (of the matrix). So if I have a matrix A and use
## it as an argument in this function I will get the possibility of using get, set, getinverse, setinverse
##setinverse.
## The second function simply return the value of inevrse matrix to the user.

## makeCacheMatrix is a "bunch" of functions: First function SET sets the matrix (I can give my matrix as an argument)
## and at the same time inversion of the matrix is globally reset to NULL (important for the second function). Second function GET
## is for "repeating" the matrix which was given earlier by SET or as an argument.
## Third function SETINVERSE takes the inverse matrix if exists 
## 4th function can be used to "reproduce" calculated before inversion


makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL			
        set <- function(y) {
                x <<- y
                inv <<- NULL		
        }
        get <- function() x	
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## # if the inverse matrix exists - was calculated before in the first run of the function - then the inverse matrix
## from cache is taken by the getinverse function from the above list of functions. If the inverse matrix does not exist yet 
## it is caluclated now and set for future use by the function setinverse from the above list of functions


cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)	
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv	
}