# This function first takes a matrix and saves it on the cache.
# The matrix is assumed to be invertible as the assignment says.
# Then it caches functions to get the matrix and return the inverse.
# It is all stored con the cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve(x)
  getinv <- function() s
  list(set = set, get = get, setinv=setinv, getinv= getinv)
}

# On the second function, the cached functions from before are called.
# The solved matrix is taken from the cache and returned.

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

# Honestly I'm not sure if I understood it right, I mostly followed the example
# and tried to do my own version that only took 7 lines and used very little scoping.
# I also looked around to learn about how to cache something on R and inverse a matrix
# and I am still confused about some things. I wrote above the functions what
# I understand they do, so getting corrections on this 4 days before the deadlines
# are my best hope.