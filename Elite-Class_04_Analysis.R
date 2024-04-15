
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

## Zeros in score variables ====

# First create a vector containing the variable names
score_col <- c("tot", "chn", "mat", "eng", "phy", "che", "bio", "geo",
               "his", "pol", "sci", "lib", "gen", "com")

# Convert the variables to numeric
data[score_col] <- map_df(data[score_col], as.numeric)

# Replace negative values and 0s with NA
data[score_col][data[score_col] <= 0] <- NA

## Track variable ====

# Identify students in science track using their exam scores in Grade 3
dat_sci <- data %>% 
  filter(exam %in% c("g2f2", "g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(sci) | (!is.na(phy) & !is.na(che) & !is.na(bio))
  ) %>% 
  select(cssid)

id_sci <- unique(dat_sci$cssid)

# Identify students in liberal arts track using their exam scores in Grade 3
dat_lib <- data %>% 
  filter(exam %in% c("g2f2", "g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(lib) | (!is.na(pol) & !is.na(his) & !is.na(geo))
  ) %>% 
  select(cssid)

id_lib <- unique(dat_lib$cssid)

# Assign value to "track" variable
data <- data %>%
  mutate(
    track = case_when(
      cssid %in% id_sci ~ "Science Track",
      cssid %in% id_lib ~ "Liberal Arts Track",
      TRUE ~ NA
    )
  )

# Continue with "trk" variable for students who remain unidentified
# in the above step

# First tidy "trk"
data <- data %>%
  mutate(
    trk = case_when(
      str_detect(trk, "L|l|普理|艺理|体育|理") ~ "Science Track",
      str_detect(trk, "W|w|普文|艺文|文") ~ "Liberal Arts Track",
      TRUE ~ NA
    )
  )

# For students who have at least one non-NA value in "trk", we use
# the last non-NA value in "trk"
data <- data %>% 
  group_by(cssid) %>%
  mutate(
    track = case_when(
      !is.na(track) ~ track,
      TRUE ~ last(na.omit(trk))
    )
  ) %>%
  ungroup()

# Finally, we use "btrack" variable
data <- data %>%
  mutate(
    track = case_when(
      is.na(track) ~ btrack,
      TRUE ~ track
    )
  )

## Class number variable (wip) ====

# First tidy "cls" and also "cid"
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

## Comprehensive test scores (wip) ====
# >>>>>> deal with "com" 
data <- data %>%
  mutate(
    sci = case_when(
      # when "sci" is NA, replace with the sum of scores of
      # the three composing subjects
      is.na(sci) ~ rowSums(select(., phy, che, bio), na.rm = TRUE),
      TRUE ~ sci
    ),
    lib = case_when(
      # when "lib" is NA, replace with the sum of scores of
      # the three composing subjects
      is.na(lib) ~ rowSums(select(., geo, his, pol), na.rm = TRUE),
      TRUE ~ lib
    )
  ) 

# Replace 0s produced in the above step with NA
data[score_col][data[score_col] == 0] <- NA

## Standardize exam scores within cohort/exam (wip) ====
# >>>>>> standardize "tot" around the cutoffs
data <- data %>% 
  group_by(cohort, exam) %>% 
  mutate(
    across(
      .cols = tot:com,
      .fns = ~ scale(.)[, 1],
      .names = "z{.col}"
    )
  ) %>% 
  ungroup()