rankhospital <- function(state, outcome, num = "best")
{
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
	x[, c(4, 5, 6)] <- sapply (x[, c(4, 5, 6)], as.vector)
	x[, c(4, 5, 6)] <- sapply (x[, c (4, 5, 6)], as.numeric)
	if (outcome == "heart attack") {
		i <- order (x[, 4], x[, 2])
	} else if (outcome == "heart failure") {
		i <- order (x[, 5], x[, 2])
	} else {
		i <- order (x[, 6], x[, 2])
	}
	if (num == "best") {
		k <- 1
	} else if (num == "worst") {
		if (outcome == "heart attack") {
			k <- 4
		} else if (outcome == "heart failure") {
			k <- 5
		} else {
			k <- 6
		}
		j <- is.na (x[i[1:370], k])
		k <- length (j) - sum (j)
	} else {
		k <- as.numeric (num)
	}
	return (x[i[k], 2])
}