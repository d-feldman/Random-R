#MICA FHS OLD
##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
##vis
library(ggplot2)



###SETUP###
##Set working Directory and load CSV##
setwd('/Users/danielfeldman/Desktop/MICA_FHS/')

D_raw_old <- read.csv('MICA_FHS_OLD.csv', header=T)

##select those with data
D1_old <- D_raw_old %>% drop_na(fhs_mother)

D1_old[D1_old == 9] <- NA

D2_old <- D1_old %>% select(c('record_id' | contains('fhs2_') | contains('fhs3_') | contains('fhs4_') | contains('fhs5') |
                        contains('fhs6') | contains('b_')) | contains( '31a') | contains ('hurt') | contains('32') | contains ('33'))

#father df
D_father_o <- D2_old %>% select(contains('fa'))
D_father_o$Subject <- D2_old$record_id
D_father_o <- D_father_o %>% relocate(Subject, .before = fhs2_fa)
D_father_o$Sum <- rowSums(D_father_o[ , c(2:31)], na.rm=TRUE)
D_father_o$Sum_Depression <- rowSums(D_father_o[ , c(7:10)], na.rm=TRUE)
D_father_o$Sum_Depression_2 <- rowSums(D_father_o[ , c(8:9)], na.rm=TRUE)


#father df
D_mother_o <- D2_old %>% select(contains('fa'))
D_mother_o$Subject <- D2_old$record_id
D_mother_o <- D_mother_o %>% relocate(Subject, .before = fhs2_fa)
D_mother_o$Sum <- rowSums(D_father_o[ , c(2:31)], na.rm=TRUE)
D_mother_o$Sum_Depression <- rowSums(D_mother_o[ , c(7:10)], na.rm=TRUE)
D_mother_o$Sum_Depression_2 <- rowSums(D_mother_o[ , c(8:9)], na.rm=TRUE)

#biological sibling only df
D_siblings_o<- D2_old %>% select(contains('_sib'))
D_siblings_o$Subject <- D2_old$record_id
D_siblings_o <- D_siblings_o %>% relocate(Subject, .before = fhs2_sib1)
D_siblings_o$Number_of_siblings <- rowSums(D1_old[, c('fhs_sib1','fhs_sib2','fhs_sib3','fhs_sib4', 'fhs_sib5')], na.rm = TRUE)
D_siblings_o$Sum <- rowSums(D_siblings_o[ , c(2:152)], na.rm=TRUE)
D_siblings_o$Sum_Depression <- rowSums(D_siblings_o[ , c(27:46)], na.rm=TRUE)
D_siblings_o$Sum_Depression_2 <- rowSums(D_siblings_o[ , c(32:41)], na.rm=TRUE)

##basic results df
D_results_old <- D_siblings_o %>% select(Subject, Number_of_siblings)
D_results_old$Father_Risk <- D_father_o$Sum
D_results_old$Mother_Risk <- D_mother_o$Sum
D_results_old$Sibling_Risk <- D_siblings_o$Sum
D_results_old$Total_Family_Risk <- rowSums(D_results_old[ , c(3:5)], na.rm=TRUE)

##add groups
D_results_old$Group <- c(1, 1, 1, 0, 1, 0)
## convert to strings
D_results_old <- D_results_old %>% mutate(Group = replace(Group, Group == 1, 'High Risk')) %>% 
  mutate(Group = replace(Group, Group == 0, 'Low Risk')) %>%
  mutate(Group = replace(Group, Group == 2, 'Squishy'))
##move group
D_results_old <- D_results_old %>% relocate(Group, .after = Subject)

###depression 2 question df
D_results_dep_2_old <- D_results_old
D_results_dep_2_old$Father_Dep <- D_father_o$Sum_Depression_2
D_results_dep_2_old$Mother_Dep <- D_mother_o$Sum_Depression_2
D_results_dep_2_old$Sibling_Dep <- D_siblings_o$Sum_Depression_2
D_results_dep_2_old$Dep_parent_ratio <- rowSums(D_results_dep_2_old[ , c(8:9)], na.rm=TRUE)/2
D_results_dep_2_old$Dep_sibling_ratio <- D_results_dep_2_old$Sibling_Dep / D_results_dep_2_old$Number_of_siblings
D_results_dep_2_old$Dep_total_family_ratio <- rowSums(D_results_dep_2_old[ , c(8:10)], na.rm = TRUE) / (D_results_dep_2_old$Number_of_siblings + 2)


###Depression for combining
D_family_dep_ratio_old <- D_results_dep_2_old %>% select(Subject, Group, Dep_total_family_ratio)
