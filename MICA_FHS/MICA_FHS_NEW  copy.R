#MICA FHS
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
##vis
library(ggplot2)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/')

D_raw <- read.csv('MICA_FHS.csv', header=T)

##select those with data
D1 <- D_raw %>% drop_na(fhs_mother_v2)

## change all 'IDK' answers to null values ##
D1[D1 == 9] <- NA

D2 <- D1 %>% select(c('record_id' | contains('fhs2_') | contains('fhs3_') | contains('fhs4_') | contains('fhs5') |
                      contains('fhs6') | contains('b_')) | contains( '31a') | contains('32') | contains ('33'))

#father df
D_father <- D2 %>% select(contains('fa'))
D_father$Subject <- D2$record_id
D_father <- D_father %>% relocate(Subject, .before = fhs2_fa_v2)
D_father$Sum <- rowSums(D_father[ , c(2:32)], na.rm=TRUE)
D_father$Sum_Depression <- rowSums(D_father[ , c(7:10)], na.rm=TRUE)
D_father$Sum_Depression_2 <- rowSums(D_father[ , c(8:9)], na.rm=TRUE)

#mother df
D_mother <- D2 %>% select(contains('mo'))
D_mother$Subject <- D2$record_id
D_mother <- D_mother %>% relocate(Subject, .before = fhs2_mo_v2)
D_mother$Sum <- rowSums(D_mother[ , c(2:32)], na.rm=TRUE)
D_mother$Sum_Depression <- rowSums(D_mother[ , c(7:10)], na.rm=TRUE)
D_mother$Sum_Depression_2 <- rowSums(D_mother[ , c(8:9)], na.rm=TRUE)

#biological sibling only df
D_siblings<- D2 %>% select(contains('_sib'))
D_siblings$Subject <- D2$record_id
D_siblings <- D_siblings %>% relocate(Subject, .before = fhs2_sib1_v2)
D_siblings$Number_of_siblings <- rowSums(D1[, c('fhs_sib1_v2','fhs_sib2_v2','fhs_sib3_v2','fhs_sib4_v2',
                                                'fhs_sib5_v2','fhs_sib6_v2','fhs_sib7_v2','fhs_sib8_v2')], 
                                         na.rm = TRUE)
D_siblings$Sum <- rowSums(D_siblings[ , c(2:249)], na.rm=TRUE)
D_siblings$Sum_Depression <- rowSums(D_siblings[ , c(42:73)], na.rm=TRUE)
D_siblings$Sum_Depression_2 <- rowSums(D_siblings[ , c(49:64)], na.rm=TRUE)

##basic results df
D_results <- D_siblings %>% select(Subject, Number_of_siblings)
D_results$Father_Risk <- D_father$Sum
D_results$Mother_Risk <- D_mother$Sum
D_results$Sibling_Risk <- D_siblings$Sum
D_results$Total_Family_Risk <- rowSums(D_results[ , c(2:4)], na.rm=TRUE)

##add groups
D_results$Group <- c(1, 1, 2, 2, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0,
                     1, 2, 1, 1, 0, 1, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0)
## convert to strings
D_results <- D_results %>% mutate(Group = replace(Group, Group == 1, 'High Risk')) %>% 
  mutate(Group = replace(Group, Group == 0, 'Low Risk')) %>%
mutate(Group = replace(Group, Group == 2, 'Squishy'))
##move group
D_results <- D_results %>% relocate(Group, .after = Subject)

#####do the same process but for depression only!-- 4 quesiton
D_results_dep <- D_results
D_results_dep$Father_Dep <- D_father$Sum_Depression
D_results_dep$Mother_Dep <- D_mother$Sum_Depression
D_results_dep$Sibling_Dep <- D_siblings$Sum_Depression
D_results_dep$Dep_parent_ratio <- rowSums(D_results_dep[ , c(8:9)], na.rm=TRUE)/2
D_results_dep$Dep_sibling_ratio <- D_results_dep$Sibling_Dep / D_results_dep$Number_of_siblings
D_results_dep$Dep_total_family_ratio <- rowSums(D_results_dep[ , c(8:10)], na.rm = TRUE) / (D_results_dep$Number_of_siblings + 2)

###2 question for depression
D_results_dep_2 <- D_results
D_results_dep_2$Father_Dep <- D_father$Sum_Depression_2
D_results_dep_2$Mother_Dep <- D_mother$Sum_Depression_2
D_results_dep_2$Sibling_Dep <- D_siblings$Sum_Depression_2
D_results_dep_2$Dep_parent_ratio <- rowSums(D_results_dep_2[ , c(8:9)], na.rm=TRUE)/2
D_results_dep_2$Dep_sibling_ratio <- D_results_dep_2$Sibling_Dep / D_results_dep_2$Number_of_siblings
D_results_dep_2$Dep_total_family_ratio <- rowSums(D_results_dep_2[ , c(8:10)], na.rm = TRUE) / (D_results_dep_2$Number_of_siblings + 2)

###Depression for combining
D_family_dep_ratio_new< - D_results_dep_2 %>% select(Subject, Group, Dep_total_family_ratio)

###Visualize w/ group and distribution

D_vis <- ggplot(data = D_results_dep_2, aes(x=Dep_total_family_ratio, fill = Group)) +
  geom_histogram(bins = 30)

print(D_vis)

