library(tidyverse)
library(effectsize)
load("./data/df.rdata")

#### ANOVA ----

contrasts(df$group)<- c(1,0)
contrasts(df$group)
contrasts(df$treatment)

#linear regression
model = lm(memory_score ~ group + treatment + group:treatment, data = df)
model = lm(memory_score ~ group*treatment, data = df)

print(model)
summary(model)

#> Coefficients:
#>(Intercept)               groupControl               treatmentTMS  groupControl:treatmentTMS  
#>41.004                     -2.481                     10.127                     18.995 

