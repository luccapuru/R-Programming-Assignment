rankhospital <- function(state, outcome, num = "best"){
      ## Read outcome data
      hospitaldata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      ## Check that state and outcome are valid
      fulloutcome <- c("heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
      if(!any(outcome == names(fulloutcome))){
            stop("Invalid outcome")
            #return 
      }
      #print("Chegou aqui")
      if(!any(state == hospitaldata[["State"]])){
            stop("Invalid state")
            #return 
      }
      if(is.character(num)){
            if(!any(num == c("best", "worst"))){
                  stop("Invalid rank")
            }
      }
      outcome <- fulloutcome[outcome]
      statevector <- hospitaldata["State"] == state
      subsetdata <- hospitaldata[statevector, c("State", "Hospital.Name", outcome)]
      subsetdata[,outcome] = as.numeric(subsetdata[,outcome])
      isnna <- !is.na(subsetdata[outcome])
      subsetdata <- subsetdata[isnna,]
      ## Return hospital name in that state with the given rank
      rankdata <- subsetdata[order(subsetdata[outcome], subsetdata["Hospital.Name"]), ]
      if(num == "best"){
            rankedHospital <- rankdata[1, "Hospital.Name"]
            rankedHospital
      }
      else if(num == "worst"){
            rankedHospital <- rankdata[nrow(rankdata), "Hospital.Name"]
            rankedHospital
      }
      else if(is.numeric(num)){
            rankedHospital <- rankdata[num, "Hospital.Name"]
            rankedHospital
      }
      ## 30-day death rate
}