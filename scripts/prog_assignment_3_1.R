# read in the outcome data

outcome_file <- "/Users/jtang/Projects/R/self-learning/Cousera - Data Science specialization from Johns Hopkins/jhu_r_programming/data/rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv"
outcome <- read.csv(outcome_file)

# plot the 30-day mortality rates for heart attack

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# remove the NA first to get rid of the warning message
outcome_filtered <- outcome[outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack != "Not Available",]
outcome_filtered[,11] = as.numeric(outcome_filtered[,11])
hist(outcome_filtered[,11])