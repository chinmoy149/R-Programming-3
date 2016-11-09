rankall <- function(outcome, num = "best")
{
	## Read outcome data
	## Check that state and outcome are valid
	## Return hospital name in that state with the given rank
	## 30-day death rate
	df <- read.csv ("JHU/Programming Assignment 3/outcome-of-care-measures.csv", colClasses = "character")
	states <- unique(df$State)
	states <- sort(states)
	ranks <- data.frame(hospital=NA, state=NA)
	for (i in 1:length(states)) {
		ranks[i, ] <- c(rankhospital(states[i], outcome, num), states[i])
	}
	ranks
}