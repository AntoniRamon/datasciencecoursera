makeCacheMatrix <- function(targetMatrix=matrix()){
    solveMatrix <- NULL
    setMatrix <- function(newMatrix){
        targetMatrix <<- newMatrix
        solveMatrix <<- NULL
    }
    getMatrix <- function() targetMatrix
    setRandomMatrix <- function(numRows){
        numRows <- 4
        values <- floor(runif(numRows * numRows, 0, 100))
        targetMatrix <<- matrix(values, numRows, numRows)
        solveMatrix <<- NULL
    }
    setSolveMatrix <- function(newSolveMatrix) solveMatrix <<- newSolveMatrix
    getSolveMatrix <- function() solveMatrix
    list(set=setMatrix, setRandom=setRandomMatrix, get=getMatrix,
         setSolve=setSolveMatrix, getSolve=getSolveMatrix)
}

cacheSolve <- function(matrixObject){
    solveMatrix <- matrixObject$getSolve()
    if(is.null(solveMatrix)){
        print("hola")
        targetMatrix <- matrixObject$get()
        solveMatrix <- solve(targetMatrix)
        matrixObject$setSolve(solveMatrix)
    } else {
        message("Getting cached data")
    }
    solveMatrix
}
