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
	states <- odata[, "State"]
	harate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
	hfrate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
	pnrate <- as.numeric(odata[, "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])

	## Check that state and outcome are valid
	sis <- which(states == state)
	state_hnames <- hnames[sis]
	state_rate <- vector()
	if (outcome == "heart attack") {
		state_rate <- harate[sis]
	}
	else if (outcome == "heart failure") {
		state_rate <- hfrate[sis]
	}
	else {
		state_rate <- pnrate[sis]
	}
	
	## Return hospital name in that state with lowest 30-day death rate
	mi1 <- which( state_rate == min(state_rate, na.rm = TRUE) )
	best_hname = min(state_hnames[mi1])
	best_hname
}
