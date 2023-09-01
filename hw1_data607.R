#importing the data set


library(readr)

rep_candidates=read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/primary-project-2022/rep_candidates.csv"))

View(rep_candidates)

head(rep_candidates)

dim(rep_candidates)

#rep_candidates is a 1599 x 27 dataframe

#Getting a subset of the data that we are interested in (NY)
#using dplyr because its easier to filter rows with it in my opinion

library(dplyr)
ny_rep_candidates = rep_candidates %>% filter(State == 'New York')

dim(ny_rep_candidates)

#There are 43 Republican Candidates running in new york

#Getting rid of columns that we dont need
ny_rep_candidates$Race.2=NULL
ny_rep_candidates$Race.3=NULL
ny_rep_candidates$Primary.Date=NULL
ny_rep_candidates$Club.for.Growth=NULL
ny_rep_candidates$Party.Committee=NULL
ny_rep_candidates$Renew.America=NULL
ny_rep_candidates$VIEW.PAC=NULL
ny_rep_candidates$Maggie.s.List=NULL
ny_rep_candidates$Winning.for.Women=NULL
ny_rep_candidates$E.PAC=NULL
ny_rep_candidates$Candidate=NULL
ny_rep_candidates$Trump.Date=NULL
ny_rep_candidates
dim(ny_rep_candidates)

#Now we have 43 rows and 17 columns

#Now lets rename all of the column into more useful names
head(ny_rep_candidates)
colnames(ny_rep_candidates) <- c("gender", "race", "inc", "inc chal", "st", "pos","dist", "n_prim","rat_prim","out_prim",
                                 "n_run","rat_run","out_run","insurrectionist","trump_endorse")
head(ny_rep_candidates)


#Now  will replace the values in the col insurrectionist
#Column now is going to be boolean

unique(ny_rep_candidates$insurrectionist)

if (any(ny_rep_candidates$insurrectionist == "Fully accepted")) {
  ny_rep_candidates$insurrectionist=FALSE
} else {
  ny_rep_candidates$insurrectionist = TRUE
}

ny_rep_candidates

#We only care about the winners. Filtering only winners.

ny_rep_candidates = ny_rep_candidates %>% filter(out_prim == 'Won')

#now some columns are useless so removing
ny_rep_candidates$st=NULL
ny_rep_candidates$out_prim=NULL
ny_rep_candidates$n_run=NULL
ny_rep_candidates$rat_run=NULL
ny_rep_candidates$out_run=NULL
ny_rep_candidates

#Now  will replace the values in the trump endorse
#To True or false

if (any(ny_rep_candidates$trump_endorse == "Yes")) {
  ny_rep_candidates$trump_endorse=TRUE
} else {
  ny_rep_candidates$trump_endorse = FALSE
}


#now lets analyze and visualize
ny_rep_candidates

#race distribution of NY Republican candidates that won in 2020

pie(table(ny_rep_candidates$race))

#Was Candidate incumbent?

barplot(table(ny_rep_candidates$inc))

#Want to measure the mean ratio of votes one
#first going to actually convert those values to ratio from str
without_percent=c()

for(i in ny_rep_candidates$rat_prim){
  i=substr(i, start=1, stop=nchar(i)-1)
  without_percent=append(without_percent,i)
}
without_percent

#Now that the % sign is removed, lets turn them into float 
ny_rep_candidates$rat_prim=as.numeric(without_percent)

#Now lets make this an actual ratio

ny_rep_candidates$rat_prim=(ny_rep_candidates$rat_prim)/100
ny_rep_candidates

#finally, lets measure the mean ratio of votes one during the primary
mean(ny_rep_candidates$rat_prim)

#NY Republican candidates wone 85% of the primary vote in 2020 for districts in which republicans won in