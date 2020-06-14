##With this functions you can insert a matrix and it will return it´s inverse
#Additionally, it will store said inverse, when you submit another matrix, if
#the inverse is the same, it will provide it from the cached data

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){   #This function sets the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function(){    #Gets the value of the matrix
                x
        }
        
        setinv <- function(inverse){   #Sets the inverse of the matrix
                inv <<- inverse
        }  
        getinv <- function(){          #Gets the value of the inverse of the matrix
                inv
        }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){
                message("Retrieving cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

