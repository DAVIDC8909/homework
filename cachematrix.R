

makeCacheMatrix <- function(x = matrix()) {
  
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinver <- function(inverse) inversa <<- inverse
  getinver <- function() inversa
  list(set = set, get = get, setinver = setinver, getinver = getinver)

}

cacheSolve <- function(x, ...) {
  inversa <- x$getinver()
  if(!is.null(inversa)) {
    message("Resultado es")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinver(inversa)
  inversa
}

#Pruebas
xx <- matrix(c(1,2,3,4),2,2)
xx1 <- makeCacheMatrix(xx)
cacheSolve(xx1)


  data <- matrix(rnorm(9),3,3)
 data1 <- makeCacheMatrix(data)
 cacheSolve(data1)
  
 