
library(tidyverse)

# Data Management ####

## Load data ====

data <- read_rds("Data.rds")

## Reshape data ====

data <- data %>%
# define a row by person-exam-subject
pivot_longer(
  # identify the columns to pivot
  col = hsee_trk:cee_com,
  # name the column that will store the values from the column names
  names_to = c("exam", "subject"),
  # define the character that partitions the column names into two parts
  names_sep = "_",
  # name the column that will store the column values
  values_to = "value"
) %>%
# define a row by person-exam
pivot_wider(
  # identify the column whose values will supply the column names
  names_from = subject,
  # identify the column whose values will supply the column values
  values_from = value,
  # to error if the column names are duplicated
  names_repair = "check_unique"
)

## Process variables ====

# Tidy cohort columns
data <- data %>%
  mutate(
    cohort = substr(cohortid, 2, 5),
    cohortid = NULL
  )

# Tidy and factor "trk"
data <- data %>%
  mutate(
    trk = case_when(
      str_detect(trk, "L|l|普理|艺理|体育|理") ~ "1",
      str_detect(trk, "W|w|普文|艺文|文") ~ "2",
      TRUE ~ NA
    )
  )

data$trk <- factor(data$trk,
                      levels = c(1, 2),
                      labels = c("Science Track", "Liberal Arts Track"))

# Tidy "cls" and "cid"
data <- data %>%
  mutate(
    cls = case_when(
      cls == "0" ~ NA,
      str_detect(cls, "^0") ~ substr(cls, 2, 2),
      str_detect(cls, "\\D") ~ NA,
      TRUE ~ cls
    ),
    cid = case_when(
      cid == "0" ~ NA,
      str_detect(cid, "^0") ~ substr(cid, 2, 2),
      TRUE ~ cid
    )
  )

# Process the score variables
# First create a vector containing the variable names
score_col <- c("tot", "chn", "mat", "eng", "phy", "che", "bio", "geo",
               "his", "pol", "sci", "lib", "gen", "com")

# Convert the variables to numeric
data[score_col] <- map_df(data[score_col], as.numeric)

# Replace negative values and 0s with NA
data[score_col][data[score_col] <= 0] <- NA
