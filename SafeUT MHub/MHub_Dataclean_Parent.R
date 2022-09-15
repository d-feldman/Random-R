#SafeUT MHUB Data Cleaning
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/SafeUT MHub/')

D_Raw <- read.csv('MHub<18.csv', header=T)

##select the important variables

D1 <- D_Raw %>% select(Participant.ID, age, grade,
  starts_with('Do.any'), Thoughts.of.purposely.hurting.myself.without.wanting.to.die,
  Thoughts.of.killing.myself.or.suicide, Communicated.about.hurting.myself.without.wanting.to.die,
  Communicated.about.killing.myself, Hurt.myself.on.purpose.without.wanting.to.die)

###rename columns to reasonable things###
colnames(D1) <- c('participant_id', 'age', 'grade', 'barriers_Availability', 'barriers_Cost',
                  'barriers_Insurance', 'barriers_Transportation','barriers_Unsure_Access',
                  'barriers_PG_not_communicate', 'barriers_PG_unable_help', 'barriers_PG_not_help',
                  'barriers_anonymity','barriers_stigma','barriers_lack_of_time','barriers_overwhelming',
                  'barriers_not_helpful','barriers_quality_service','barriers_other','barriers_no_issue',
                  'Toughts_of_NSSI', 'Thoughts_of_Suicide', 'Communicating_NSSI', 'Communicating_Suicidality', 'NSSI')

###turn to binary format, make no factors###
D_binary <- D1

D_binary[,] <- sapply(D_binary[,], as.character)

##create NA then 1 for positive##
D_binary[D_binary == 'Yes'] <- NA
D_binary[D_binary == 'Checked'] <- NA
D_binary <- D_binary %>% replace(is.na(.), 1)

##now make NA then 0 for negative
D_binary[D_binary == 'No'] <- NA
D_binary[D_binary == 'Unchecked'] <- NA
D_binary <- D_binary %>% replace(is.na(.), 0)


##convert binary dataframe to numeric
D_binary[,] <- sapply(D_binary[,], as.numeric)







