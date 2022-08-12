library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

##read in data##
setwd('/Users/danielfeldman/Desktop/Lab Demo Data/')

DM <- read.csv('MICACTSI3014-Demo2022_DATA_2022-07-21_1255.csv', header=T)

DR <- read.csv('RuMeChangeCTSI306-Demo2022_DATA_2022-07-21_1258.csv', header=T)

### Clean DM###
DM <- DM %>% filter( redcap_event_name == 'baseline_arm_1')

DM <- DM %>% drop_na(c('child_age'))

DM_Clean <- DM %>% select(record_id, child_age, child_sex, child_gender, child_sexual, child_gender, child_race___1, child_race___2, child_race___3,
                          child_race___4, child_race___5, child_race___6, child_race___7, child_ethnicity, child_grade, child_income)

DM_Clean$Study <- 'MICA' 

DM_Clean <- DM_Clean %>% relocate(Study, .before = child_age)

DM_Clean$Race <- NA
DM_Clean <- DM_Clean %>% relocate(Race, .before = child_race___1)

DM_Clean <- DM_Clean %>% mutate(Race = case_when( child_ethnicity == 1 ~'Hispanic/Latinx',
                                     child_race___1 == 1 ~ 'African American/Black',
                                     child_race___2 == 1 ~ 'Indigenous Peoples',
                                     child_race___3 == 1 ~ 'Asian',
                                     child_race___4 == 1 ~ 'White',
                                     child_race___5 == 1 ~ 'Native Hawaiian or Other Pacific Islander',
                                     child_race___6 == 1 ~ 'MultiCultural',
                                     child_race___7 == 1 ~ 'Other',
                                     ))

DM_Clean_1 <- DM_Clean %>% select(record_id, child_age, child_sex, child_gender, child_grade, Race, child_income)

Mica_data <- DM_Clean_1 %>% mutate (child_income = case_when(child_income == 1 ~ '<$21,000',
                                                              child_income == 2 ~ '$21,000 to $40,000',
                                                              child_income == 3 ~ '$41,000 to $60,000',
                                                              child_income == 4~ '$61,000 to $80,000',
                                                              child_income == 5 ~ '$81,000 to $100,000',
                                                              child_income == 6 ~ 'Above $100,000',
                                                              child_income == 7 ~ 'Above $100,000',
                                                              child_income == 8 ~ 'Above $100,000'))

Mica_data <- Mica_data %>% mutate(child_sex = case_when(child_sex == 2 ~ 'Female'))

Mica_data <- Mica_data %>% mutate(child_gender = case_when(child_gender == 2 ~ 'Cisfemale'))

###MICA done

###clean RuMe
DR <- DR %>% drop_na(c('demo_age'))

DR_Clean <- DR %>% select(demo_id, demo_age, demo_sex, demo_gender,
                          demo_grade, demo_race, demo_ethnicity, demo_familyincome, demo_familyincome_above)
DR_Clean <- DR_Clean %>% drop_na(demo_familyincome)
DR_Clean <- DR_Clean %>% drop_na(demo_race)

DR_Clean$Race <- NA

DR_Clean <- DR_Clean %>% mutate(Race = case_when(demo_race == 1 & demo_ethnicity == 1 ~ 'Hispanic/Latinx',
                                                 demo_race == 5 ~ 'Indigenous Peoples',
                                                 demo_race == 3 ~ 'Asian',
                                                 demo_race == 1 ~ 'White',
                                                 demo_race == 4 ~ 'Native Hawaiian or Other Pacific Islander',
                                                 demo_race == 6 ~ 'Other',
                                                 demo_race == 1 & demo_ethnicity == 1 ~ 'Hispanic/Latinx'))

DR_Clean$Income <- NA

DR_Clean <- DR_Clean %>% mutate(Income = case_when(
  # demo_familyincome_above == 1 ~ '$101,000 to $150,000',
  #                                                  demo_familyincome_above == 2 ~ '$151,000 to $250,000',
  #                                                  demo_familyincome_above == 3 ~ 'Above $250,000',
                                                    demo_familyincome == 1 ~ '<$21,000',
                                                    demo_familyincome == 2 ~ '$21,000 to $40,000',
                                                                demo_familyincome == 3 ~ '$41,000 to $60,000',
                                                               demo_familyincome == 4~ '$61,000 to $80,000',
                                                               demo_familyincome == 5 ~ '$81,000 to $100,000',
                                                   demo_familyincome == 6 ~ 'Above $100,000'
                                                               ))

DR_Clean <- DR_Clean %>% mutate(demo_sex = case_when(demo_sex == 2 ~ 'Female', demo_sex == 1 ~'Male'))

DR_Clean <- DR_Clean %>% mutate(demo_gender = case_when(demo_gender == 1 ~ 'Cismale',
                                                        demo_gender == 2 ~ 'Cisfemale',
                                                        demo_gender == 3 ~ 'Transmale',
                                                        demo_gender == 4 ~ 'Transfemale',
                                                        demo_gender == 5 ~ 'Non-Binary',
                                                        demo_gender == 6 ~ 'Agender',
                                                        demo_gender == 7 ~ 'Other'))

Rume_data <- DR_Clean

#gender pie adjust
#Rume_data <- Rume_data %>% drop_na(demo_gender)

#RUME Done

#Combine

Mica_comb <- Mica_data %>% select(record_id, child_age, child_sex, child_gender, child_grade, Race, child_income)
colnames(Mica_comb) <- c('ID','Age','Sex','Gender','Grade','Ethnicity', 'Income')

Rume_comb <- Rume_data  %>% select(demo_id, demo_age, demo_sex, demo_gender,
                                   demo_grade, Race, Income)
colnames(Rume_comb) <- c('ID','Age','Sex','Gender','Grade','Ethnicity', 'Income')


Combined_data <- rbind(Mica_comb,Rume_comb)


Combined_data$Income <- factor(Combined_data$Income, levels = c('<$21,000', '$21,000 to $40,000', '$41,000 to $60,000',
                                                                '$61,000 to $80,000', '$81,000 to $100,000', 'Above $100,000',
                                                                '$101,000 to $150,000', '$151,000 to $250,000', 'Above $250,000'))

Combined_data <- arrange(Combined_data, Ethnicity)
# Combined_data$Ethnicity <- factor(Combined_data$Ethnicity, levels = c('Asian','Hispanic/Latinx',
#                                                                       'Indigenous Peoples', 'Native Hawaiian or Other Pacific Islanders',
#                                                                       'Other', 'White'))
# 

##visualize
demo_bar <- ggplot(Combined_data, aes(x= Income)) + geom_bar(aes(fill=Ethnicity), color = 'black') +  
  scale_fill_manual(values = c('#F8EC22','#F4D03F','#F5B041','#E67E22','#CB4335','#943126')) + 
  xlab(label='Annual Household Income') + ylab(label='Participants') + 
  theme(plot.background = element_rect(fill = "#F4F6F6")) + labs(title="")
demo_bar <- annotate_figure(demo_bar, top = text_grob("Participant Household Income", color= 'black',
                                                      face = 'bold', size = 14))  

slices <- c(2,6,1,15,1,138)
lbls <- c('Other','Asian','Indigenous Peoples','Hispanic/Latinx', 
            'Native Hawaiian or Other Pacific Islanders', 'White')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%", sep = "")
pie(slices, labels = lbls, main = "Participant Ethnicities", col = c('#CB4335','#E67E22','#F5B041','#F4D03f','#5DADE2','#1F618D'),
    density = 200, angle = 135)



# demo_pie <- ggplot(Combined_data, aes(y= Income, x=thnicity, fill = Ethnicity), color = Ethnicity) +
#             geom_col(stat=) +
#             coord_polar("x", start = 0) + theme_void() +
#             scale_fill_brewer(palette = 'RdYlBu')

#print(demo_bar)



