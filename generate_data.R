library(dplyr)

set.seed(123) # For reproducibility

# Function to generate a single CSV file
generate_csv <- function(subject_id) {
  subject <- sprintf("subj%03d", subject_id)
  
  blocks <- rep(1:8, each = 50)
  trials <- rep(1:50, times = 8)
  
  condition <- sample(c("ink_naming_cong", "ink_naming_incong", 
                        "word_reading_cong", "word_reading_incong"), 
                      size = 400, replace = TRUE)
  
  correct_response <- sample(c("red", "blue", "green", "yellow"), 
                             size = 400, replace = TRUE)
  
  participant_response <- ifelse(runif(400) < 0.95, 
                                 correct_response, 
                                 sample(c("red", "blue", "green", "yellow", NA), 
                                        size = 400, replace = TRUE, 
                                        prob = c(0.23, 0.23, 0.23, 0.23, 0.08)))
  
  rt <- ifelse(is.na(participant_response), NA, 
               round(rnorm(400, 
                           mean = ifelse(grepl("ink_naming", condition), 1000, 700) + 
                             ifelse(grepl("incong", condition), 
                                    ifelse(grepl("ink_naming", condition), 150, 20), 0), 
                           sd = 100)))
  
  # Introduce random NA values in participant_response
  na_indices <- sample(1:400, size = sample(0:25, 1), replace = FALSE)
  participant_response[na_indices] <- NA
  rt[na_indices] <- NA
  
  # Add RT outliers
  low_outlier_indices <- sample(which(!is.na(rt)), size = sample(5:10, 1), replace = FALSE)
  high_outlier_indices <- sample(which(!is.na(rt) & !(1:400 %in% low_outlier_indices)), size = sample(5:10, 1), replace = FALSE)
  
  rt[low_outlier_indices] <- sample(0:400, length(low_outlier_indices), replace = TRUE)
  rt[high_outlier_indices] <- sample(1500:6000, length(high_outlier_indices), replace = TRUE)
  
  # Adding irrelevant columns
  irrelevant_columns <- data.frame(
    ip_address = paste0(sample(192:223, 400, replace = TRUE), ".",
                        sample(0:255, 400, replace = TRUE), ".",
                        sample(0:255, 400, replace = TRUE), ".",
                        sample(0:255, 400, replace = TRUE)),
    latitude = runif(400, -90, 90),
    longitude = runif(400, -180, 180),
    browser_version = sample(c("Chrome"), 400, replace = TRUE),
    operating_system = sample(c("Windows"), 400, replace = TRUE),
    screen_resolution = paste0(sample(c(1024), 400, replace = TRUE), "x", 
                               sample(c(768), 400, replace = TRUE)),
    device_type = sample(c("Desktop"), 400, replace = TRUE),
    user_agent = paste0("Agent", sample(1000:9999, 400, replace = TRUE)),
    session_id = sample(1:10000, 400, replace = TRUE),
    click_rate = runif(400, 0, 0),
    error_count = sample(0:0, 400, replace = TRUE)
  )
  
  data <- data.frame(
    subject = subject,
    block = blocks,
    trial = trials,
    condition = condition,
    correct_response = correct_response,
    participant_response = participant_response,
    rt = rt
  ) %>%
    bind_cols(irrelevant_columns)
  
  write.csv(data, file = paste0("./data/collected_data/", subject, ".csv"), row.names = FALSE)
}

# Generate 30 CSV files
for (i in 1:30) {
  generate_csv(i)
}
