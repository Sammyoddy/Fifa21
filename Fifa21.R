#installed R packages necessary for data cleaning and reading csv
install.packages("tidyverse")
install.packages("tidyr")
install.packages('readr')
install.packages('dplyr')
install.packages("stringr")
install.packages("janitor")

#instantiated the libraries for use
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
data()
#set working directory
setwd('C:/Users/SAMMY/Desktop/DataCleaningR')

#imported the fifa 21 csv
fifa21raw<- read_csv("fifa21 raw data v2.csv")
fifa21raw2<-fifa21raw
#Accessed the fifa data to get familiar with the data
#and examine the data types for correction where necessary
View(fifa21raw)
glimpse(fifa21raw)

#renamed ColumnNames for easy readability
fifa21raw<-fifa21raw %>% rename(Overall_Analysis=`↓OVA`,PlayerPotential=POT,
                                Best_Overall=BOV,Joined_National=Joined,
                                Skill_Moves=SM,AttackingWorkRate=`A/W`,
                                DefensiveWorkRate=`D/W`,WeakFootRating=`W/F`,
                                InjuryRating=IR,Shooting=SHO,Pace=PAC,
                                Weight_kg=Weight)
#Removed nextline characters found in the club column
fifa21raw <-fifa21raw %>% mutate(Club=str_remove_all(Club,"\n"))

#Checked for Missing values (NA values were spotted in 'Loan Date End' column)
fifa21raw %>% select(ID:Hits) %>% filter(complete.cases(.)) %>% View()

#Checked for duplicate values
unique(fifa21raw$ID)

unique(fifa21raw$Contract)

#Split Contract Column to Start Year and End Year
fifa21raw<-separate(fifa21raw,col = Contract,into = c("start_year","end_year"),sep = "~",remove = F)

fifa21raw<-fifa21raw %>% 
  mutate(Contract_type=case_when(str_detect(start_year,"Free")~"Free", 
                                 str_detect(start_year,"On Loan")~ "Loan",
                                 TRUE~ "Full-Time"),
         start_year=case_when(str_detect(Contract,"Free")~ NA_character_,
                              str_detect(Contract,"On Loan")~NA_character_,
                              TRUE~start_year),
         end_year=case_when(str_detect(Contract,"On Loan")~ str_extract(Contract,"\\d{4}"),
                            TRUE~end_year))

fifa21raw <-fifa21raw %>% mutate(across(c(start_year,end_year),as.numeric))


#---------------------------------------------------------------
#replaced all instances of "'" to "" and 
#all instances of '\"' to "" in Height column

  fifa21raw<- fifa21raw %>% mutate(Height=str_replace(fifa21raw$Height,"cm",""))
  fifa21raw<- fifa21raw %>% mutate(Height=str_replace(fifa21raw$Height,"\"",""))
  
#Split Height cell into Feet and Inches column                   
fifa21raw<- separate(fifa21raw,col = Height, into = c("Feet","Inches"), sep="'",remove = FALSE,convert = TRUE)
#All Height values that are duplicated in Feet Column as a result of the split  values are replaced with NA 
#Observations in Height column
fifa21raw %>% mutate(Feet=if_else(Height==Feet,NA,Feet))


#Created a new Height Column off the Feet and Inches Column Convert All Height in Ft to cm
fifa21raw<-fifa21raw %>% mutate('Height_cm'=
                                  if_else(!is.na(Feet) & !is.na(Inches),round((Feet*30.48)+Inches*2.54,digits = 0),
                                          as.integer(Height)))

fifa21raw %>% View()
#---------------------------------------------------------------



#Convert All Weight in lbs to kg

counter<<-1
for(row in fifa21raw$Weight_kg){
if(any(str_detect(row,"lbs"))){
  row<-str_remove(row,"lbs")
  fifa21raw$Weight_kg[counter]<-round(as.double(row)*0.453592,digits=0)
}else if(any(str_detect(row,"kg"))){
  row<-str_remove(row,"kg")
  fifa21raw$Weight_kg[counter]<-row
}else{
  fifa21raw <- fifa21raw$Weight_kg
}
  counter<-counter+1
}


#fifa21raw$Value<-fifa21raw %>% mutate(Value=str_remove(Value,"€"))
counter<-1
for(row in fifa21raw$Value){
  if(any(str_detect(row,"M"))){
    row<-str_remove(row,"€")
    row<-str_remove(row,"M")
    fifa21raw$Value[counter]<-as.numeric(row)*1000000
  }else if(any(str_detect(row,"K"))){
    row<-str_remove(row,"€")
    row<-str_remove(row,"K")
    fifa21raw$Value[counter]<-as.numeric(row)*1000
  }else{
    fifa21raw <- fifa21raw$Value
  }
  counter<-counter+1
}



getwd()


fifa21raw<-fifa21raw %>% mutate(across(c(Value,Wage,`Release Clause`),
 function(col) {case_when(
  str_detect(col, "M") ~ as.double(str_replace_all(col, "(€|M)", "")) * 1000000,
  str_detect(col, "K") ~ as.double(str_replace_all(col, "(€|K)", "")) * 1000,
  TRUE ~ as.double(str_replace(col, "€", "")))}))

fifa21raw %>% select(22:24)filter(!complete.cases(.)) %>% view()