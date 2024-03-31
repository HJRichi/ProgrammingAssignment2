## Das ist die komplette Funktion zum Speichern und Abrufen der
## Inversen einer Matrix

# Die erste Funktion, makeCacheMatrix, hat die Aufgabe
# - den Wert der Matrix zu setzen
# - den Wert der Matrix abfragen
# - setzt den Wert der Inversion
# - liefert den Wert der Inversion
makeCacheMatrix <- function(x = matrix()) {
  ## Initialisiere den Cache für die Inverse
  inv <- NULL
  
  ## Setze die Matrix und lösche den gespeicherten Inversen-Cache
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Hole die gesetzte Matrix
  get <- function() x
  
  ## Setze die berechnete Inverse im Cache
  setinverse <- function(inverse) inv <<- inverse
  
  ## Hole die gespeicherte Inverse aus dem Cache
  getinverse <- function() inv
  
  ## Gib eine Liste der Funktionen zurück
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# In dieser Funktion wird die Inversion berechnet, wenn sie nicht im
# Cache vorhanden ist: inv <- solve(data, ...)
cacheSolve <- function(x, ...) {
  ## Zuerst wird versucht, die gespeicherte Inverse aus dem Cache zu holen
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## Wenn die Inverse bereits im Cache vorhanden ist, wird eine Nachricht
    ## ausgegeben und die Inverse zurückgegeben
    message("getting cached inverse")
    return(inv)
  }
  ## Wenn die Inverse nicht im Cache ist, wird die Matrix 'data' aus
  ## dem 'x' Objekt geholt
  data <- x$get()
  ## Dann wird die Inverse der Matrix 'data' mit der Funktion 'solve' berechnet
  inv <- solve(data, ...)
  ## Die berechnete Inverse wird im 'x' Objekt gespeichert für
  ## zukünftige Verwendung
  x$setinverse(inv)
  ## Schließlich wird die Inverse zurückgegeben
  inv
}

myMatrix <- makeCacheMatrix(matrix(data = c(7, 28, 3, 4), nrow = 2))
cacheSolve(myMatrix) 
cacheSolve(myMatrix) 
