rankall <- function(outcome, num = "best") {
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

	## For each state, find the hospital of the given rank
	df<-NULL
	state_abb <- sort(unique(states))
	for (curr_state in sort(state_abb)) {
		sis <- which(states == curr_state)
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

		## Check num
		ranking = 0
		if (num == "best") {
			ranking = 1
		}
		else if (num == "worst") {
			ranking = -1
		}
		else if ((num %% 1 == 0) & (num > 0)) {
			ranking = num
		}
		else {
			stop("invalid ranking")
		}

		hname = NA
		indexes1 <- order(state_rate, na.last = NA, decreasing = FALSE)
		if (ranking == -1) {
			ranking = length(indexes1)
		}
		if (ranking <= length(indexes1)) {
			state_rate_subset <- state_rate[indexes1]
			state_hnames_subset <- state_hnames[indexes1]
			val <- state_rate[indexes1[ranking]]
			indexes2 <- which(state_rate_subset==val)
			hname <- min(state_hnames_subset[indexes2])
		}

		rbind(df, data.frame(hospital=hname,state=curr_state)) -> df
	}
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	rownames(df) <- state_abb
	df
}
