

## Creates a square matrix

makeCacheMatrix <- function(x=matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinvmat <- function(invmat) inverse <<-  invmat
    getinvmat <- function() inverse
    list(set = set, get = get,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}

## Returns inverse matrix and chaches inverse matrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinvmat()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse<- solve(data,)
    x$setinvmat(inverse)
    inverse
}
        
##example:
## x = matrix(c(4,7,2,6), nrow = 2, byrow = TRUE)

## mat1 = makeCacheMatrix(x)
## mat1$get()
##[,1] [,2]
##[1,]    4    7
##[2,]    2    6


##using the function cacheSolve:

##cacheSolve(mat1)
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4

##runing it again:

##cacheSolve(mat1)
##getting cached data
##[,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4
