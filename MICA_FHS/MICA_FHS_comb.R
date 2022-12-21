###combine and visualize!

##Libraries##
#manipulating data
library(dplyr)
library(tidyr)
##vis
library(ggplot2)


D_total_fam_dep_conb <- rbind (D_family_dep_ratio_new, D_family_dep_ratio_old)



D_vis <- ggplot(data = D_total_fam_dep_conb, aes(x=Dep_total_family_ratio, fill = Group)) +
  geom_histogram(bins = 30)

print(D_vis)