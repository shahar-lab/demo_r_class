
#### CREATE RAW DATA ----
library(dplyr)

# combine files to one df
files_names = dir("./data/collected_data/")

df = data.frame()

for (file in files_names) {
  df <- rbind(df, read.csv(paste0("./data/collected_data/",file)))
}

# take only the columns we need
df <- df |>
  mutate(
    task       = ifelse(grepl("ink_naming", condition), "ink_naming", "word_reading"),
    congruency = ifelse(grepl("incong", condition), "congruent", "incongruent"),
    acc        = ifelse(participant_response == correct_response, 1, 0)
  ) |>
  
  mutate(
    subject    = as.factor(subject),
    task       = as.factor(task),
    congruency = as.factor(congruency),
    block      = as.numeric(block),
    trial      = as.numeric(trial),
    acc        = as.numeric(acc),
    rt         = as.numeric(rt)
  ) |>
  
  select(subject, block, trial, task, congruency, participant_response, correct_response, acc, rt)

summary(df)

#check factors
contrasts(df$task)<-c(1,0)
contrasts(df$task)
contrasts(df$congruency)

save(df, file = "./data/raw_data.rdata")


#### CREATE FILTERD DATA ----
load("./data/raw_data.rdata")

cat("Number of unique subjects:", length(unique(df$subject)))

# Remove NA 
df <- df |> filter(!is.na(rt))

# Remove RT outliers (below 300 or above 3000)
df <- df |> filter(rt >= 300 & rt <= 3000)

#check the percentage of remaining trials
df |>  group_by(subject) |> summarise(percentage = 1 - (n() / 400)) |>  print(n = Inf)

save(df, file = "./data/filtered_data.rdata")

