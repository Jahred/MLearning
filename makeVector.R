makeVector <- function(x=numeric()){
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m 
  list(set=set,
       get = get,
       setmean=setmean ,
       getmean=getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

  makeMatrix <- function( mat = matrix()){
    inverse <-NULL
    set <- function( matrixToSet){
      mat <<- matrixToSet
      inverse <<-NULL
    }
    get <-function() mat
    
    setInverse <- function(inverse1) inverse <<- inverse1
    
    getInverse <- function() inverse
    
    if(!is.null(inverse)){
    proof <- mat%*%inverse
    }else{
      proof <- NULL
    }
    list(set=set, setInverse = setInverse,get=get ,getInverse = getInverse,proof)
  }
  
solveInverse <- function(mat,...){
  inverse <- mat$getInverse()
  if(!is.null(inverse)){
    message("matrix inverse already solved. Getting from cache.")
   return( inverse)
    
  }
  message("-----not in cache---calculating....")
  mm <- mat$get()
  message("-----not in cache---MMMMMM....")
  inverse1 <- solve(mm)
  message("-----not in cache--SOLV...")
  mat$setInverse(inverse1)
  return( inverse1)
  

 
}

