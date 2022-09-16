###Visualizing things
library(ggplot2)
#manipulating data
library(dplyr)
library(tidyr)

setwd('/Users/danielfeldman/Desktop/SafeUT MHub/')

D_Raw <- read.csv('MHub<18.csv', header=T)

D_pre_dep <- D_Raw %>% select(Participant.ID, currentSafeUT_worrysad_preintense, SexualOrientation)
D_pre_dep$time <- 'Pre Chat'
D_pre_dep$Condition <- 'Depressed Pre'

colnames(D_pre_dep) <- c('ID','Scale', 'Sexual Orientation', 'Time', 'Condition')


D_post_dep <- D_Raw %>% select(Participant.ID, currentSafeUT_worrysad_postintense, SexualOrientation)
D_post_dep$time <- 'Post Chat'
D_post_dep$Condition <- 'Depressed Post'

colnames(D_post_dep) <- c('ID','Scale','Sexual Orientation','Time', 'Condition')

D_pre_SI <- D_Raw %>% select(Participant.ID, currentSafeUT_suicidethoughts_preintense, SexualOrientation)
D_pre_SI$time <- 'Pre Chat'
D_pre_SI$Condition <- 'SI Pre'
colnames(D_pre_SI) <- c('ID','Scale', 'Sexual Orientation', 'Time', 'Condition')


D_post_SI <- D_Raw %>% select(Participant.ID, currentSafeUT_suicidethoughts_postintense, SexualOrientation)
D_post_SI$time <- 'Post Chat'
D_post_SI$Condition <- 'SI Post'
colnames(D_post_SI) <- c('ID','Scale','Sexual Orientation','Time', 'Condition')

D_long <- rbind(D_pre_dep,D_post_dep,D_pre_SI,D_post_SI)

D_long <- drop_na(D_long)

D_long$`Sexual Orientation`<- as.character(D_long$`Sexual Orientation`)

D_long <- D_long %>% mutate(`Sexual Orientation`= replace(`Sexual Orientation`, `Sexual Orientation` == 'Straight (Heterosexual)', 'Straight')) %>% 
  mutate(`Sexual Orientation`= replace(`Sexual Orientation`, `Sexual Orientation` != 'Straight', 'Queer'))

D_long$`Sexual Orientation`<- as.factor(D_long$`Sexual Orientation`)



####
Visualization <-  ggplot(data = D_long, aes(x=factor(Time, levels = c('Pre Chat', 'Post Chat')),
                                            y= Scale, fill = Condition)) + geom_boxplot(position = 'dodge2', notch = T) + 
  xlab('Time') + ylab('Symptom Intensity')  +
  labs(title= '                                                  Pre-Post SafeUT Chat Analysis of Depressive & Suicidality Symptom Intensity', caption = '
       Figure 1) Both intensity of depressive symptoms and intensity of suicidal ideation significantly decreased over the course of the SafeUT chat: t(136)=19.24 (p<.001); t(38)=7.81(p<.001).') + 
  theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
        plot.title = element_text(face='bold'), axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'), axis.text.x = element_text(face = 'bold', size =10),
        axis.text.y = element_text(face = 'bold', size = 10), plot.caption = element_text(size =11)) + 
  scale_fill_manual(values = c('#5498af', '#5498af', '#f8c048', '#f8c048')) + 
  annotate('rect', xmin = .8, xmax =1.815, ymin = 5.4, ymax = 5.41, alpha = 1) +
  annotate('rect', xmin = .8, xmax =.805, ymin = 5.2, ymax = 5.41, alpha = 1) +
  annotate('rect', xmin = 1.810, xmax =1.815, ymin = 5.2, ymax = 5.41, alpha = 1) +
  annotate("text", x =1.3, y= 5.55, label = "Depression Change, p<.001 *" , size = 5, color= 'black') +
  annotate('rect', xmin = 1.18, xmax =2.195, ymin = 6.0, ymax = 6.01, alpha = 1) +
  annotate('rect', xmin = 1.18, xmax =1.185, ymin = 5.8, ymax = 6.01, alpha = 1) +
  annotate('rect', xmin = 2.19, xmax =2.195, ymin = 5.8, ymax = 6.01, alpha = 1) +
  annotate("text", x =1.7, y= 6.15, label = "Suicidal Ideation Change, p<.001 *" , size =5, color= 'black')

print(Visualization)
