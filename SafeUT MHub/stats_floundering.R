###SafeUT Stats###
library(rstatix)
library(sjstats)

D_stats <- D_parent

D_stats$NSSI<- as.numeric(D_stats$NSSI)
D_stats$Not_want_talk <- as.numeric(D_stats$Not_want_talk)
try_wil <- wilcox.test(NSSI ~ Not_want_talk,
                                    exact = F, data = D_stats)

print(try_wil)