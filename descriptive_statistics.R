#this is a comment to make this clearer

library(tidyverse)
library(ggdist)
library(ggpubr)
load("./data/df.rdata")

#### DESCRIPTIVE STATISTICS ----

summary(df)



#plot using manual ggplot 
df_summary <- df |>
    group_by(group, treatment) |>
    summarise(
    mean = mean(memory_score),
    se   = sd(memory_score) / sqrt(n()),
  )





ggplot(df, aes(x = group, y = memory_score, color = treatment)) +
  
  geom_jitter(position = position_jitter(width = 0.2, height = 0), size = 1, alpha = 0.3) +
  
  geom_point(data = df_summary, aes(x = group, y = mean, group = treatment, color = treatment), 
    position = position_dodge(1), size = 4) +
  
  geom_errorbar(data = df_summary,aes(x = group, y = mean, group = treatment, ymin = mean - se, ymax = mean + se, color = treatment ), 
    position = position_dodge(1), width = 0.2
  ) +
  theme_minimal() 


#use ggpubr
ggerrorplot(df,  x = "group",   y = "memory_score",   color = "treatment",  desc_stat = "mean_se",
            add = "jitter",  add.params = list(alpha = 0.2), position = position_dodge(1)) +
  theme_minimal()



