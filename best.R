best <- function (state, outcome)
{
	##Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with lowest 30-day death rate
	
	dt <- read.csv ("JHU/Programming Assignment 3/outcome-of-care-measures.csv", colClasses = "character")
	states <- unique (dt$State)
	outcomes <- c ("heart attack", "heart failure", "pneumonia")
	
	if (!(state %in% state)) {
		stop ("invalid state")
	}
	if (!(outcome %in% outcomes)) {
		stop ("invalid outcome")
	}
	
	x <- dt[dt$State == state, c(1, 2, 7, 11, 17, 23)]
	x[, c(4, 5, 6)] <- sapply (x[, c (4, 5, 6)], as.numeric)
	if (outcome == "heart attack") {
		i <- order (x[, 4], x[, 2])
	} else if (outcome == "heart failure") {
		i <- order (x[, 5], x[, 2])
	} else {
		i <- order (x[, 6], x[, 2])
	}
	return (x[i[1], 2])
}