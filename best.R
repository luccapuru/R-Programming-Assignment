best <- function(state, outcome){
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
      outcome <- fulloutcome[outcome]
      statevector <- hospitaldata["State"] == state
      subsetdata <- hospitaldata[statevector, c("State", "Hospital.Name", outcome)]
      subsetdata[,outcome] = as.numeric(subsetdata[,outcome])
      isnna <- !is.na(subsetdata[outcome])
      subsetdata <- subsetdata[isnna,]
      ## Return hospital name in that state with lowest 30-day death
      ## rate
      minrow <- which(subsetdata[[outcome]] == min(subsetdata[[outcome]]))
      bestHospitals <- subsetdata[minrow,]
      if(nrow(bestHospitals > 1)){
         print("Entrou no if")
         alphebetic <- bestHospitals[order(bestHospitals["Hospital.Name"]), ]
         alphebetic[1, "Hospital.Name"]
      }
      else bestHospitala$Hospital.Name
}