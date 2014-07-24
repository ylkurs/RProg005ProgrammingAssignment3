best <- function(state, outcome) {
	## Check state
	if ( !(state %in% state.abb) ) {
		stop("invalid state")
	}

	## Check outcome
	diseases = c("heart attack", "heart failure", "pneumonia")
	if ( !(outcome %in% diseases) ) {
		stop("invalid outcome")
	}

	## Read outcome data
	odata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	hnames <- odata[, "Hospital.Name"]
	harate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
	hfrate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
	pnrate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])

	## Check that state and outcome are valid


	## Return hospital name in that state with lowest 30-day death
	## rate
}
