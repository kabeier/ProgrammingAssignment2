## Make matrix inverse cacheable using makeCacheMatrix
## Retrieve cache data using cacheSolve

## Get and store inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #make inverse settable
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #make inverse gettable
        get <- function() x
        #solve for inverse using solve function
        setinverse<- function(solve) i <<- solve
        getinverse <- function() i
        #make list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Check for cached inverse else calculate inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that  the inverse of 'x'
        i <- x$getinverse()
        ##if its cached return the cache
        if(!is.null(i)) {
                message("getting cached data for matrix inverse")
                return(i)
        }
        #get the matrix
        data <- x$get()
        #solve for inverse using solve
        i <- solve(data, ...)
        #set inverse
        x$setinverse(i)
        #return the inverse
        i
}


