## This both functions enable to count inverse matrix. It counts inverse matrix only once and save to list. 
## If you ask for the result more than once, you don´t overload the computer memory.

makeCacheMatrix <- function(x = matrix()) {     ## makeCacheMatrix is a function, which create a list with 1 stored matrix and 4 functions
m <- NULL                                       ## value of variable m is NULL at the beginning, in this variable will be stored inverse matrix                                                        
  set <- function(y) {                          ## this function improves to set new matrix (new numbers)
    x <<- y
    m <<- NULL
  }
  get <- function() x                           ## this function will print stored matrix
  setinv <- function(solve) m <<- solve         ## setinv will set inverse matrix to variable m
  getinv <- function() m                        ## this prints inverse matrix on screen
  list(set = set, get = get,                    
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinv()
  if(!is.null(m)) {                             ## If inverse matrix was counted, it will return cached matrix
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                         ## this will count the inverse matrix
  x$setinv(m)                                   ## set the inverse matrix to variable x
  m
}
