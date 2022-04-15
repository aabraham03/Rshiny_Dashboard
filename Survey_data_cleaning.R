####DATA CLEANING####
rm(list=ls())
##Loading Library and dataset
library(tidyverse)
results <- read.csv("C:/Users/ACER/Downloads/survey.csv", header = TRUE, sep = ",")
results = data.frame(lapply(results, tolower))

##Remove Timestamp and comments
results <- results[,-c(1,27)]

##Age filtered for above 18 and below 75.NA values replaced with median value
results$Age = as.integer(results$Age)
results$Age[(results$Age < 18) | (results$Age > 75)] = NA
results$Age = replace_na(results$Age, median(results$Age, na.rm=TRUE))

###Gender being classified as Female,Male or Non-Binary.NA value replaced as Non-binary
results$Gender = gsub('^m$|^man$|.*\\bma.{1,3}\\b.*|cis.*\\bm.*|^mail$|.*mas.*|.*dude.*', 'male', results$Gender)
results$Gender = gsub('^f$|.*woman.*|.*fem.*|cis.*\\bfemale|fm|.*she.*', 'female', results$Gender)
# results$gender = gsub('.*queer.*|non.*binary|.*trans.*|none|.*fluid.*|.*andro.*|.*bi.*', 'non-binary', results$gender)
results$Gender[(results$Gender!='male') & (results$Gender!='female')] = 'non-binary'
results$Gender = replace_na(results$Gender, 'non-binary')
unique(results$Gender)
##correcting Country values
results$Country = gsub('united states.*', 'united states of america', results$Country)
results$Country = replace_na(results$Country, 'united states of america')
unique(results$state)

##Binning no_of_employees

results$Agency_size <-replace(results$Agency_size,results$no_employees=="6-25","Small")
results$Agency_size <-replace(results$Agency_size,results$no_employees=="26-100","Small")
results$Agency_size <-replace(results$Agency_size,results$no_employees=="1-5","Small")
results$Agency_size <-replace(results$Agency_size,results$no_employees=="100-500","Medium")
results$Agency_size <-replace(results$Agency_size,results$no_employees=="500-1000","Medium")
results$Agency_size <-replace(results$Agency_size,results$no_employees=="more than 1000","Big")
unique(results$Agency_size)

## Text scrubbing for all yes/no questions
yn_cols = c('leave','benefits', 'wellness_program',
            'seek_help', 'anonymity','coworkers','mental_health_interview',
            'phys_health_interview', 'mental_vs_physical', 'mental_health_consequence','phys_health_consequence','supervisor')

##Converting all of them to Yes/No/Not sure
results[yn_cols] = lapply(results[yn_cols], gsub,
                          pattern='some of them', replacement='yes')
results[yn_cols] = lapply(results[yn_cols], gsub,
                          pattern='.*don.t know', replacement='not sure')
results[yn_cols] = lapply(results[yn_cols], gsub,
                          pattern='possibly|maybe', replacement='not sure')


##Full Name of States in USA
results$state <- toupper(results$state)
library(usdata)
results$state <- abbr2state(results$state)


## Graph results to verify data is clean.
headers = names(results)
for (i in 1:length(headers)){
  barplot(table(results[i]), main = headers[i])
}


library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)

employee_quest <- c(
  "Have sought treamtent for mental condition?" = "treatment",
  "Does your employer provide mental health benefits?" = "benefits",
  "Possible consequence if you discuss a mental health issue with employer?" = "mental_health_consequence",
  "Would you bring up a physical health issue with a potential employer in an interview?" = "phys_health_interview",
  "Do you feel that your employer takes mental health as seriously as physical health?" = "mental_vs_physical",
  "Have you heard of or observed negative consequence for coworkers with metnal health condition at workplace?" = "obs_consequence"
)

employer_quest <- list(
  "treatment" = "Have sought treamtent for mental condition?" ,
  "benefits" ="Do employers provide mental health benefits?",
  "mental_health_consequence" ="Possible consequence from disucssing mental health with employers?" ,
  "phys_health_interview" = "Would you bring up a physical health issue in an interview?" ,
  "mental_vs_physical" ="Does employer takes mental health as seriously as physical health?" ,
  "obs_consequence" = "Any negative consequence with mental health condition at workplace?"
)

lot_labeller <- function(variable,value){
  if (variable=='question') {
    return(str_wrap(employer_quest[[value]],width = 40))
  } 
  else
    return("")
}

# extract country list choices
countries<-unique(sort(results$Country))
country_choices = data.frame(
  var = countries, # need to change it to the values inside the "Question" Column
  num = countries
)
# List of choices for selectInput
countryList <- as.list(country_choices$num)
# Name it
names(countryList) <- country_choices$var


# extract state values for choices 
states <- unique(sort(results$state))
state_choices = data.frame(
  var = states,
  num = states
)
# List of choices for selectInput
stateList <- as.list(state_choices$num)
# Name it
names(stateList) <- state_choices$var
