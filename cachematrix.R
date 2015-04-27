## makeCacheMatrix is a function that stores a list of functions that operate
## on the matrix given as its argument x.
## Cachesolve stores the inverse of the matrix x in the cache

## Takes the argument x, outputs a list of four functions (set, get, setinverse
## and getinverse)

makeCacheMatrix <- function(x = matrix()) {
        
                m <- NULL
                set <- function(y) {
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



## Takes the output of makeCacheMatrix as its input.
## Returns the inverse of matrix x, by either using the solve() function or
## retrieving the stored inverse calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setinverse(m)
        m
        
}


## Test code to confirm that the functions work
m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()

inv <- cacheSolve(x)    ## Calculates the inverse using solve()
inv

inv <- cacheSolve(x)    ## Retrieves the cached solution
inv


