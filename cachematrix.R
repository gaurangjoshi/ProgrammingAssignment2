## The following program consists of two functions namely makeCacheMatrix and cacheSolve


##This function creates a special vector that can cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {
        z <- NULL
        set <- function(y){ ## changes the matrix which is stored in makeCacheMatrix
                x <<- y
                z <<- NULL
}
get <- function() x ##returns a matrix x stored in makeCacheMatrix

        setinverse <- function(solve) z <<- solve ##storing the input value in variable z

                getinverse <- function() z ##return the value of variable z

                        list(get=get, set=set, 
      
                             getinverse=getinverse, setinverse=setinverse) ##storing the four functions in makeCacheMatrix
}
## cacheSolve solves the inverse of special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        z <- x$getinverse()
        if(!is.null(z)){
                message("getting cached data")
                return(z)
                }
        
        mymatrix <- x$get()
        z <- solve(mymatrix) %*% mymatrix ##computes the inverse using matrix multiplication
        x$setinverse(z)
        z ##return the matrix
}

