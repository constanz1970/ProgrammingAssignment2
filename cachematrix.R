## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv.m <- NULL
        
        # Function allows set matrix. To change x value is used <<- operator to have access from another environment
        set <- function(y) {
                x <<- y
                inv.m <<- NULL
        }
        
        # Function return matrix 
        get <- function() x
        
        # Function allows set inverse matrix. We also use <<- operator to have access from another environment
        setInverse <- function(inverse.m) inv.m <<- inverse.m
        
        # Function return inverse matrix
        getInverse <- function() inv.m
        
        # List 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        # Get inverse matrix using getInverse() function
        inv.m <- x$getInverse()
        
        # Check inv.m is not null && in that case, return inv.m
        if(!is.null(inv.m)) {
                message("getting cached data")
                return(inv.m)
        }
        # Get matrix
        matr <- x$get()
        # Compute inverse matrix
        inv.m <- solve(matr, ...)
        #Set inverse matrix 
        x$setInverse(inv.m)
        
        inv.m
        
}
