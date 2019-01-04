# Options
options(warn=-1)

# Constants
csvname <- "/Users/toni/Desktop/Coursera/R/data/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"
outcomesColumns <- c(11, 17, 23) 
names(outcomesColumns) <- c("heart attack", "heart failure", "pneumonia")

# Functions
readOutcomeData <- function(outcome, state=NA, filename=csvname){
    dataOutcome <- read.csv(filename, colClasses = "character")
    outcomeColumn <- outcomesColumns[outcome]
    ## Check that state and outcome are valid
    invalidOutcome <- is.na(outcomeColumn)
    if (invalidOutcome) {
        stop("invalid outcome")
    }
    if (is.na(state)) {
        dataOutcome <- dataOutcome[, c(2, 7, outcomeColumn)]
        dataOutcome[, 3] <- as.numeric(dataOutcome[, 3])
        return(dataOutcome)
    }
    validState <- state %in% unique(dataOutcome$State)
    if (!validState){
        stop("invalid state")
    }
    dataOutcome <- dataOutcome[dataOutcome$State == state, c(2, outcomeColumn)]
    dataOutcome[, 2] <- as.numeric(dataOutcome[, 2])
    dataOutcome
}

best <- function(state, outcome){
    data <- readOutcomeData(outcome, state)
    data <- data[order(data[, 2]), ]
    data[1, 1]
}

rankhospital <- function(state, outcome, num="best", data=NA){
    orderFactor <- 1
    if (num == "best"){
        num <- 1
    }
    else if (num == "worst"){
        num <- 1
        orderFactor <- -1
    } 
    if (is.na(data)){
        data <- readOutcomeData(outcome, state)    
    }
    data <- data[order(orderFactor * data[, 2], data[, 1]), ]
    data[num, 1]
}

rankall <- function(outcome, num="best"){
    response <- data.frame(matrix(ncol=2, nrow=0))
    names(response) <- c("hospital", "state")
    data <- readOutcomeData(outcome)
    for (state in sort(unique(data$State))){
        stateData <- data[data$State == state, c(1, 3)]
        hospitalName <- rankhospital(state, outcome, num, stateData)
        result <- data.frame(hospitalName, state)
        response <- rbind(response, result)
    }
    response
}
