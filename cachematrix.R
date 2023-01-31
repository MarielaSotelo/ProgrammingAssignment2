## Hay dos funciones para poder generar la inversa de una matriz:
## makeCacheMatrix y cacheSolve

## Esta funcion genera la matriz de la cual se obtendrá la inversa
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inversa<-NULL
  set<-function(y){
    x<<-y
    inversa<<-NULL
  }
  get<-function() x
  setinversa<-function(inverse) inversa<<-inverse
  getinversa<-function()
  {
    inver<-ginv(x)
    inver%*%x
  } 
  
  list(set=set,get=get,setinversa=setinversa,getinversa=getinversa)
}


## Esta función regresa la matriz inversa

cacheSolve <- function(x, ...) {
    inversa<-x$getinversa()
  if(!is.null(inversa)){
    message("Obteniendo matriz")
    return(inversa)
  }
  matriz<-x$get()
  inversa<-solve(matriz,...)
  x$setinversa(inversa)
  inversa
}
