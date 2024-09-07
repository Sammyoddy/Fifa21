library(tidyverse)
library(stringr)
library(janitor)


#Set Working Directory
setwd('C:/Users/SAMMY/Desktop/Fifa21')


#Read in Fifa21 Dataset
fifa21raw2<-read_csv('fifa21 raw data v2.csv')

#View Fifa Dataset to get familiar with the structure and content of the dataset 
fifa21raw %>% View()
glimpse(fifa21raw)

#Tidy column names for easy readability and usability 
fifa21raw<-clean_names(fifa21raw)
fifa21raw %>% View()

#
fifa21raw %>% select(club)
#Remove nextline characters in column Club
fifa21raw<-fifa21raw %>% mutate(club=str_remove_all(club,"\n"))
glimpse(fifa21raw$club)

#Check for Cells with NA
fifa21raw %>% select(id:hits) %>% filter(complete.cases(.)) %>% View()

#Determine Contract type of players
fifa21raw<-fifa21raw %>% mutate(contract_type = case_when(
  str_detect(contract,"Loan") ~ 'Loan',
  str_detect(contract,"Free") ~ 'Free Agent',
  TRUE ~ 'Permanent'
))

#function to convert height in ft and inches to cm
convert_height_fn<-function(height) {
  counter<<-1
  for (row in height) {
    if(str_detect(row,"'"))
             {
               row<-strsplit(row,"'")[[1]]
               feet<-as.numeric(row[[1]])
               inches<-as.numeric(str_remove(row[[2]],"\""))
               cm<-round(((feet*12)+ inches) * 2.54,digits=0)
               height[counter]<-cm
             }
           else{
             height[counter]<- as.numeric(str_remove(row,"cm"))
           }
    counter<- counter+1
  }
  return(height)
  
}

fifa21raw<- mutate(fifa21raw,height_cm=as.numeric(convert_height_fn(height)))


#function to convert Weight in lbs to cm
convert_weight_fn<- function(weight){
  counter<<-1
    for (row in weight) {
      if (str_detect(row,"lbs")) {
        row<-str_remove(row,"lbs")
        row<- round(as.numeric(row)/2.20462,digits=0)
        weight[counter]<-row
      }
      else{
        row<-str_remove(row,"kg")
      row<- as.numeric(row)
      weight[counter]<-row
      }
      counter<-counter+1
    }
 return(weight) 
}

fifa21raw<- mutate(fifa21raw,weight_kg=as.numeric(convert_weight_fn(weight)))

#Convert Column loan_date_end to Date datatype
fifa21raw<-fifa21raw %>% mutate_at(vars(Joined_National,`Loan Date End`),
                                   ~as.Date(.,format="%b %d, %Y"))

# Remove star character from columns w_f, sm, and ir and 
# Rename columns w_f, sm, and ir to weak_foot, skill_move, and injury_rating respectively
fifa21raw <- fifa21raw %>%
  mutate_at(vars(w_f, sm, ir),
            ~if_else(str_detect(., "★"),
                     as.numeric(str_remove(., "★")),
                     as.numeric(str_remove(., " |★"))
            )
  ) %>%
  
  rename(
    weak_foot = w_f,skill_move = sm,injury_rating = ir
  )

# Remove €, M, and K symbols from columns value, wage, and release_clause
fifa21raw <- mutate_at(fifa21raw, vars(value, wage, release_clause),
                       ~if_else(str_detect(., "M"),
                                as.numeric(str_remove_all(., "[€M]")) * 1000000,
                                if_else(str_detect(., "K"),
                                        as.numeric(str_remove_all(., "[€K]")) * 1000,
                                        as.numeric(str_remove_all(.,"€"))
                                )
                       )
)

# Remove K symbol from column hits column and convert to numeric
fifa21raw <- fifa21raw %>% mutate(fifa21raw, hits = ifelse(str_detect(hits, "K"),
                                             as.numeric(str_remove(hits, "K")) * 1000,
                                             ifelse(is.na(as.numeric(hits)), NA_real_, as.numeric(hits))
))

#rename a_w and d_w to attacking_workrate and defending_workrate respectively
fifa21raw<- fifa21raw %>% rename(attacking_workrate=a_w,defending_workrate=d_w)

#Sub-setting columns after Data Cleaning
fifa21raw <- fifa21raw %>%
  select(ID:Contract, Contract_type, Positions, height_cm, Weight_kg, `Preferred Foot`:Hits) 

#Save CSV file
write_csv(as.data.frame(fifa21raw),"fifa21_cleaned.csv")


                                
