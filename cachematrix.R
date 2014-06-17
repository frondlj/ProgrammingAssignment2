## Function "makeCacheMatrix" initialize variables and methods used by function "cacheSolve".
## 
## variables: 
##      - x => matrix; for initial input of matrix to solve 
##      - inv_matrix => matrix; for storing inversed matrix 
##      - cached_matrix => matrix; for storing the original matrix to compare with next input
##      - y => matrix; for input of next matrix to solve
## methods:
##      - setmatrix => sets variable x to insertet value, resets variables inv_matrix and cached_matrix to NULL
##      - setinverse => compute the inverse matrix to x matrix (called by cacheSolve function)
##      - getmatrix => getter for actually solved matrix
##      - getcache => getter for actually cached matrix
##      - getinverse => getter fo inverse matrix
##      - compare => chech equality of old and newly inputed matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv_matrix <- NULL
        cached_matrix <- NULL
        
        setmatrix <- function(y) {
                x <<- y
                inv_matrix <<- NULL
                cached_matrix <<- NULL
        }
        setinverse <- function() {
                inv_matrix <<- solve(x)
                cached_matrix <<- x
        }
        getmatrix <- function() x
        getcache <- function() cached_matrix
        getinverse <- function() inv_matrix
        compare <- function() {
                identical(x, cached_matrix)
        }
        
        list(setmatrix = setmatrix, 
             getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse,
             getcache = getcache,
             compare = compare)
}



## Function "cacheSolve" returns inverse matrix to matrix introduced by "makeCacheMarix" function.
## If inverse matrix already exist and original matrix doesn´t changed, funcion return cached matrix
## Funcition has new function "compute", which is used inside the cacheSolve in control flow process.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_matrix <- x$getinverse()
        cache <- x$getcache()
        
        compute <- function() {
                message("computing data")
                data <- x$getmatrix()
                inv_matrix <- solve(data)
                x$setinverse()
                return(inv_matrix)
        }
        
        if(is.null(cache)) {
                compute()
        }
        else if(x$compare()) {
                if(!is.null(inv_matrix)) {
                        message("getting cached data")
                        return(inv_matrix)
                }
        }
        else {
                compute()
        }
}

