###Safe UT Corr plot###
library(corrplot)

##correlation Dataframe for barriers only
D_barriers_cor <- D_binary %>% select(starts_with('barriers'))

D_barriers_cor <- D_barriers_cor %>% select(barriers_PG_not_communicate,
                                            barriers_Cost, barriers_Insurance,
                                            barriers_overwhelming, barriers_stigma  ,barriers_no_issue)
colnames(D_barriers_cor) <- c('    Parent \n    Communication', 'Cost', 'Insurance', 'Overwhelmed', 'Stigma', 'No Issue')

###create cor data for vis
cor.data <- cor(D_barriers_cor)



SFUT_cor_plot <- corrplot(cor.data, method = 'square', order = 'FPC',
                        type = 'lower', diag = F,
                         addCoef.col = 'black', tl.srt = 40, tl.col = 'black',
                        cl.ratio = 0.2)

