###Visualization for Parent groups
library(ggplot2)

##drop null
D_parent <- drop_na(D_binary)

D_parent <- D_parent %>% select(participant_id, age, barriers_PG_not_communicate,
                                Toughts_of_NSSI, Thoughts_of_Suicide, Communicating_NSSI, Communicating_Suicidality, NSSI)

colnames(D_parent) <- c('Participant','Age','Not_want_talk','Thoughts_of_NSSI',
                        'Thoughts of Suicide', 'Communicated_NSSI', 'Communicated_Suicidality', 'NSSI')

##create NA then 1 for positive##
D_parent[D_parent == 1] <- NA
D_parent <- D_parent %>% replace(is.na(.), 'Yes')

##now make NA then 0 for negative
D_parent[D_parent == 0] <- NA
D_parent <- D_parent %>% replace(is.na(.), 'No')

D_parent$Not_want_talk<- as.factor(D_parent$Not_want_talk)
D_parent$Thoughts_of_NSSI <- as.factor(D_parent$Thoughts_of_NSSI)
D_parent$NSSI <- as.factor(D_parent$NSSI)


##Create NSSI thought
Parent_NSSI <- ggplot(data = D_parent, aes(group = Not_want_talk,
  x= NSSI, fill = Not_want_talk)) + 
  geom_bar(position = 'dodge', alpha = .75) + 
  xlab('Have you hurt yourself on purpose without wanting to die?') +
  ylab('Number of Responses') + labs(title = 'Working') + 
  guides(color=guide_legend(title = 'Is not wanting to talke to your parent/guardian about mental health a barrier to seeking help?')) + 
  scale_fill_manual(values = c('#68a2b9','#f8c048')) +
  theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'right',
  legend.background = element_rect(fill = '#EAEDED' ), plot.title = element_text(face='bold'))

# 
# ##Create Suicide thought
# Parent_Suicide_thought <- ggplot(data = D_parent, aes(group = Thoughts_of_Suicide,
#                             x= barriers_PG_not_communicate, fill = Thoughts_of_Suicide)) + 
#   geom_bar(position = 'dodge', alpha = .75) 
# # 
# # ##create NSSI action
# # Parent_NSSI_action <- ggplot(data = D_parent, aes(group = barriers_PG_not_communicate,
# #                         x= Communicating_NSSI, fill = barriers_PG_not_communicate)) + 
# #   geom_bar(position = 'dodge', alpha = .75) 
# # 
# # 
# # ##communicating suicide
# # Parent_Suicide <- ggplot(data = D_parent, aes(group = barriers_PG_not_communicate,
# #               x= Communicating_Suicidality, fill = barriers_PG_not_communicate)) + 
# #   geom_bar(position = 'dodge', alpha = .75) 
# 
# 
# 
# 
# 
# # 
# # +
# #   xlab('Participant') + ylab('Difference Scores')  + labs(title= 'A1) Sadness             p<0.001 ***')+
# #   theme(plot.background = element_rect(fill = "#F4F6F6"), legend.position = 'bottom',
# #         legend.background = element_rect(fill = '#EAEDED' ), plot.title = element_text(face='bold')) +
# #   guides(color=guide_legend(title = 'Dose')) + scale_fill_manual(values = c('#C0392B','#2471A3')) + 
# #   annotate('rect', xmin = .4, xmax =3, ymin = -.25, ymax =.25, alpha = 1) 