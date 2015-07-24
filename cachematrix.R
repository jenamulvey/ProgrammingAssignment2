
### Matrix inversion is usually a costly computation and there may be 
# some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


## 1.   makeCacheMatrix()   this function creates a list containing a function to

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y = matrix()){
                x <<- y
                inv <<- NULL
        }
        
        get <- function(){
                x
        }
        
        setinv <- function(i){
                inv <<- i
        }
        
        getinv <- function(){
                inv
        }
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}



##2. cacheSolve()  - this funtion computes the inverse of the matrix returned by 'makeCacheMatrix'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        
}

## I am not sure if we are supposed to shoow an example with results.  
## The directions do not state that and I'm not adding.