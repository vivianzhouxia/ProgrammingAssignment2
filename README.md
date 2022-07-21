
### Assignment: Caching the Inverse of a Matrix

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

    cacheSolver <- function(x, ...) {
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

my_matrix4<-makeCacheMatrix(matrix(5:8,2,2))

my_matrix4$get()

my_matrix4$getinverse()

cacheSolve(my_matrix4)


