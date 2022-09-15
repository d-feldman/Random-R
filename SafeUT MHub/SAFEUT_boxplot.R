###Visualizing things
library(ggplot2)
#manipulating data
library(dplyr)
library(tidyr)

setwd('/Users/danielfeldman/Desktop/SafeUT MHub/')

D_Raw <- read.csv('MHub<18.csv', header=T)

D_pre <- D_Raw %>% select(Participant.ID, currentSafeUT_worrysad_preintense, SexualOrientation)
D_pre$time <- 'Pre Chat'

colnames(D_pre) <- c('ID','Worry', 'Sexual Orientation', 'Time')


D_post <- D_Raw %>% select(Participant.ID, currentSafeUT_worrysad_postintense, SexualOrientation)
D_post$time <- 'Post Chat'
colnames(D_post) <- c('ID','Worry','Sexual Orientation','Time')

D_long <- rbind(D_pre,D_post)

D_long <- drop_na(D_long)

D_long$`Sexual Orientation`<- as.character(D_long$`Sexual Orientation`)

D_long <- D_long %>% mutate(`Sexual Orientation`= replace(`Sexual Orientation`, `Sexual Orientation` == 'Straight (Heterosexual)', 'Straight')) %>% 
  mutate(`Sexual Orientation`= replace(`Sexual Orientation`, `Sexual Orientation` != 'Straight', 'Queer'))

D_long$`Sexual Orientation`<- as.factor(D_long$`Sexual Orientation`)

Visualization <-  ggplot(data = D_long, aes(x=factor(Time, levels = c('Pre Chat', 'Post Chat')),
                                            y= Worry, fill = Time)) + geom_boxplot(position = 'dodge') +
  xlab('Time') + ylab('Depressive Symptom Intensity')  +
  labs(title= '                           Pre-Post SafeUT Chat Analysis of Depressive Symptom Intensity', caption = '
       Figure 1) Intensity of depressive symptoms significantly decreased over the course of the SafeUT chat; t(136)=19.24 (p<.01).    ')  +
  theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'none', 
        plot.title = element_text(face='bold'), axis.title.x = element_text(face = 'bold'),
        axis.title.y = element_text(face = 'bold'), axis.text.x = element_text(face = 'bold', size =10),
        axis.text.y = element_text(face = 'bold', size = 10), plot.caption = element_text(size =11)) + 
  scale_fill_manual(values = c('#f8c048', '#68a2b9')) + 
  annotate('rect', xmin = 1, xmax =2, ymin = 5.9, ymax = 5.91, alpha = 1) +
  annotate('rect', xmin = 1, xmax =1.005, ymin = 5.7, ymax = 5.91, alpha = 1) +
  annotate('rect', xmin = 1.995, xmax =2, ymin = 5.7, ymax = 5.91, alpha = 1) +
  annotate("text", x =1.5, y= 6.1, label = "p<.01 *" , size = 4, color= 'black') 
 
  

print(Visualization)
