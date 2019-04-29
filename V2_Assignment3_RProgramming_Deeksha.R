getwd()
setwd('C:\\Users\\DeekshaS\\Desktop\\Adsoft\\R_Adsoft\\Assignment 3')

#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
## 1. Plot the 30-day mortality rates for heart attack##
outcome_measure <-read.table('outcome-of-care-measures.csv',sep=',',header = TRUE)
Hospital_data <- read.table('hospital-data.csv',sep=',')
typeof(outcome_measure)
mortality_heartattack =as.numeric(outcome_measure[,11])
typeof((mortality_heartattack))
length(mortality_heartattack)
hist(mortality_heartattack)

library(naniar)
#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
##2.Finding the best hospital in the state

best<-function(state,outcome){
  outcome_measure <-read.table('outcome-of-care-measures.csv',sep=',',header = TRUE)
#Validating the state and outcome. The lines can be reduced using %in%
  valid_usstates<-c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  Record_outcome <-c("heart attack","Heart Attack","heart failure","Heart Failure","Pneumonia","pneumonia")
  row<-grep(state,valid_usstates)
  out<-grep(outcome,Record_outcome)
  if(length(row)==0) stop("invalid state")
  if(length(out)==0) stop("invalid outcome")
#Deciding in which outcome we want to continue working with
  if(outcome== Record_outcome[1] | outcome==Record_outcome[2]){
    mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcome_measure$State==state]
    mortal[mortal=="Not Available"]<-NA
  }else if(outcome == Record_outcome[3] | outcome==Record_outcome[4]){
    mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcome_measure$State==state]
    mortal[mortal=="Not Available"]<-NA
  }else{
    mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcome_measure$State==state]
    mortal[mortal=="Not Available"]<-NA
  }
#Cleaning data i.e drop missing value rows.(complete.cases is yet to be tried on this)
  hos_data <- outcome_measure[outcome_measure$State==state,]
  new_data = data.frame(hos_data$Hospital.Name,hos_data$State,as.numeric(mortal))
  new_data<-na.omit(new_data)
#Returning the hospital name based on the outcome ranking
  hos_name <-new_data$hos_data.Hospital.Name[new_data$as.numeric.mortal.==min(new_data$as.numeric.mortal.)]
  return(hos_name)
  
}
state = "AZ"
outcome = "Heart Attack"

hospital_name = best(state,outcome)
print(hospital_name)



#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________

##3.Ranking hospitals by outcome in state
rankhospital <- function(state,outcome,num){

  outcome_measure <-read.table('outcome-of-care-measures.csv',sep=',',header = TRUE)
#Validating the state and outcome. The lines can be reduced using %in%
  valid_usstates<-c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
  Record_outcome <-c("heart attack","Heart Attack","heart failure","Heart Failure","Pneumonia","pneumonia")
  row<-grep(state,valid_usstates)
  out<-grep(outcome,Record_outcome)
  if(length(row)==0) stop("invalid state")
  if(length(out)==0) stop("invalid outcome")
#checking whether the num is a number or a charcter. 
#If num =="best" => num=1 else if num =="worst" then the last value else num = num
  if(is.character(num)){
    if(num=="best"){
      num = 1
    }else if(num=="worst"){
      num = length(outcome_measure$State==state)
    }
  }else{
    num = num
  } 
# Validating the number and cleaning the data
  hos_data <- outcome_measure[outcome_measure$State==state,]
  if(num>length(hos_data$State)){
    return("NA")
  } else{
    if(outcome== Record_outcome[1] | outcome==Record_outcome[2]){
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcome_measure$State==state]
      mortal[mortal=="Not Available"]<-NA
    }else if(outcome == Record_outcome[3] | outcome==Record_outcome[4]){
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcome_measure$State==state]
      mortal[mortal=="Not Available"]<-NA
    }else{
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcome_measure$State==state]
      mortal[mortal=="Not Available"]<-NA
    }
    new_data = data.frame(hos_data$Hospital.Name,hos_data$State,as.numeric(mortal))
    new_data<-na.omit(new_data)
    ranking <-new_data[order(new_data$as.numeric.mortal.),]
# Due to data cleaning, there's reduction in size of the data row wise
  #so change the num value if it refers to "worst" wrt new data frame
    if(num==length(outcome_measure$State==state)){
      num = length(new_data)
    }else{
      num = num
    }
    return(ranking$hos_data.Hospital.Name[num])
  }
   
}

#state = "AZ"
#outcome = "Heart Attack"
num = as.numeric(c(5))

hospital_name = rankhospital(state,outcome,num)
print(hospital_name)




#____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
##4. Ranking hospitals in all states

rankall<-function(outcome,num){
  outcome_measure <-read.table('outcome-of-care-measures.csv',sep=',',header = TRUE)
#Validating the outcome. The lines can be reduced using %in%
  Record_outcome <-c("heart attack","Heart Attack","heart failure","Heart Failure","Pneumonia","pneumonia")
  out<-grep(outcome,Record_outcome)
  if(length(out)==0) stop("invalid outcome")
  #checking whether the num is a number or a charcter. 
  #If num =="best" => num=1 else if num =="worst" then the last value else num = num
  if(is.character(num)){
    if(num=="best"){
      num = length(outcome_measure$State==state)/2
    }else if(num=="worst"){
      num = length(outcome_measure$State==state)
    }
  }else{
    num = num
  } 
# Validating the number and cleaning the data
  if(num>length(outcome_measure$State)){
    return("NA")
  } else{
    if(outcome== Record_outcome[1] | outcome==Record_outcome[2]){
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
      mortal[mortal=="Not Available"]<-NA
    }else if(outcome == Record_outcome[3] | outcome==Record_outcome[4]){
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
      mortal[mortal=="Not Available"]<-NA
    }else{
      mortal <- outcome_measure$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
      mortal[mortal=="Not Available"]<-NA
    }
  }
  rank_all <- data.frame(outcome_measure$Hospital.Name,outcome_measure$State,as.numeric(mortal))
  rank_all<-na.omit(rank_all)
  rank_all <- rank_all[order(rank_all$as.numeric.mortal.),]
# Due to data cleaning, there's reduction in size of the data row wise
#so change the num value if it refers to "worst", and "best" wrt new data frame
  if(num==length(outcome_measure$State==state/2)){
    num = length(rank_all/2)
  }else if(num==length(outcome_measure$State==state)){
    num = length(rank_all)
  }else{
    num = num
  }
  required_dataFrame <- data.frame(rank_all[1:num,1:2])
  return(required_dataFrame)
}
num =5
outcome = "Heart Attack"
required_dataFrame <- rankall(outcome,num)
print(required_dataFrame)
