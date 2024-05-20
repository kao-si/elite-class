
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

## Class number set ====

# Create "cls_set" variable for each cohort
data <- data %>%
  mutate(
    cls_set = case_when(
      # cohorts 2003, 2004, 2007, and 2010 - 2014
      cohort %in% c("2003", "2004", "2007", "2010", "2011", "2012", "2013", "2014") &
        str_detect(exam, "hsee|^g1") ~ "1",
      cohort %in% c("2003", "2004", "2007", "2010", "2011", "2012", "2013", "2014") &
        !str_detect(exam, "hsee|^g1") ~ "2",
      # cohort 2005
      cohort == "2005" & exam %in% c("hsee", "g1m1", "g1f1") ~ "1",
      cohort == "2005" & !exam %in% c("hsee", "g1m1", "g1f1") ~ "2",
      # cohort 2006
      cohort == "2006" & exam %in% c("hsee", "g1m1", "g1f1") ~ "1",
      cohort == "2006" & exam %in% c("g1m2", "g1f2") ~ "2",
      cohort == "2006" & !str_detect(exam, "hsee|^g1") ~ "3",
      # cohort 2008
      cohort == "2008" & str_detect(exam, "hsee|^g1") ~ "1",
      cohort == "2008" & str_detect(exam, "^g2") ~ "2",
      cohort == "2008" & str_detect(exam, "^g3|cee") ~ "3",
      # cohort 2009
      cohort == "2009" & str_detect(exam, "hsee|^g1") ~ "1",
      cohort == "2009" & exam %in% c("g2m1", "g2f1") ~ "2",
      cohort == "2009" & str_detect(exam, "g2m2|g2f2|^g3|cee") ~ "3"
    )
  )

# Tidy "trk", "cls", and "cid"
data <- data %>%
  mutate(
    trk = case_when(
      str_detect(trk, "L|l|普理|艺理|体育|理") ~ "Science Track",
      str_detect(trk, "W|w|普文|艺文|文") ~ "Liberal Arts Track",
      TRUE ~ NA
    ),
    cls = case_when(
      cls == "0" ~ NA,
      str_detect(cls, "^0") ~ substr(cls, 2, 2),
      str_detect(cls, "\\D") ~ NA,
      TRUE ~ cls
    ),
    cid = case_when(
      cid == "0" ~ NA,
      cid == "99" ~ NA,
      str_detect(cid, "^0") ~ substr(cid, 2, 2),
      TRUE ~ cid
    )
  )

# Fill missing values of "trk", "cls", and "cid"
data <- data %>%
  group_by(cssid, cls_set) %>%
  fill(trk, cls, cid, .direction = "updown") %>%
  ungroup()

## Zeros in exam scores ====

# First create a vector containing the variable names
score_col <- c("tot", "chn", "mat", "eng", "phy", "che", "bio", "geo",
               "his", "pol", "sci", "lib", "gen", "com")

# Convert the variables to numeric
data[score_col] <- map_df(data[score_col], as.numeric)

# Replace negative values and 0s with NA
data[score_col][data[score_col] <= 0] <- NA

## Anomalous exam scores ====

# Replicate of comprehensive test scores

# 2004_g3k1, phy == sci
# Replace phy values with NA
data$phy[data$cohort == "2004" & data$exam == "g3k1" & data$phy == data$sci] <- NA

# 2006_g3k1, che == sci
# Replace che values with NA
data$che[data$cohort == "2006" & data$exam == "g3k1" & data$che == data$sci] <- NA

# 2005_g3m1, bio == lib
# Replace bio values with NA
data$bio[data$cohort == "2005" & data$exam == "g3m1" & data$bio == data$lib] <- NA

# 2005_g3m1, geo == sci
# Replace geo values with NA
data$geo[data$cohort == "2005" & data$exam == "g3m1" & data$geo == data$sci] <- NA

# 2004_g3k1 & 2006_g3k1, pol == lib
# Replace pol values with NA
data$pol[data$cohort %in% c("2004", "2006") & data$exam == "g3k1" & data$pol == data$lib] <- NA

# gen scores

# 2004_g3m1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2004" & data$exam == "g3m1" & 
data$trk == "Science Track"] <- data$his[data$cohort == "2004" &
data$exam == "g3m1" & data$trk == "Science Track"]

data$gen[data$cohort == "2004" & data$exam == "g3m1" & 
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2004" &
data$exam == "g3m1" & data$trk == "Liberal Arts Track"]

data$his[data$cohort == "2004" & data$exam == "g3m1" & 
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2004" & data$exam == "g3m1" & 
data$trk == "Liberal Arts Track"] <- NA

# 2004_g3f1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2004" & data$exam == "g3f1" & 
data$trk == "Science Track"] <- data$his[data$cohort == "2004" &
data$exam == "g3f1" & data$trk == "Science Track"]

data$gen[data$cohort == "2004" & data$exam == "g3f1" & 
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2004" &
data$exam == "g3f1" & data$trk == "Liberal Arts Track"]

data$his[data$cohort == "2004" & data$exam == "g3f1" & 
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2004" & data$exam == "g3f1" & 
data$trk == "Liberal Arts Track"] <- NA

# 2004_g3k1, gen score recorded under pol for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2004" & data$exam == "g3k1" & 
data$trk == "Science Track"] <- data$pol[data$cohort == "2004" &
data$exam == "g3k1" & data$trk == "Science Track"]

data$gen[data$cohort == "2004" & data$exam == "g3k1" & 
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2004" &
data$exam == "g3k1" & data$trk == "Liberal Arts Track"]

data$pol[data$cohort == "2004" & data$exam == "g3k1" & 
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2004" & data$exam == "g3k1" & 
data$trk == "Liberal Arts Track"] <- NA

# 2005_g2f2, gen score recorded under pol for Science Track and under phy for Liberal Arts Track
# gen scores have been created for this exam in the Import Raw Data step
data$pol[data$cohort == "2005" & data$exam == "g2f2" & 
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2005" & data$exam == "g2f2" & 
data$trk == "Liberal Arts Track"] <- NA

# 2005_g3m1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2005" & data$exam == "g3m1" & !is.na(data$trk) &
data$trk == "Science Track"] <- data$his[data$cohort == "2005" &
data$exam == "g3m1" & !is.na(data$trk) & data$trk == "Science Track"]

data$gen[data$cohort == "2005" & data$exam == "g3m1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2005" &
data$exam == "g3m1" & !is.na(data$trk) & data$trk == "Liberal Arts Track"]

data$his[data$cohort == "2005" & data$exam == "g3m1" & !is.na(data$trk) &
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2005" & data$exam == "g3m1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- NA

# 2006_g3f1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2006" & data$exam == "g3f1" & !is.na(data$trk) &
data$trk == "Science Track"] <- data$his[data$cohort == "2006" &
data$exam == "g3f1" & !is.na(data$trk) & data$trk == "Science Track"]

data$gen[data$cohort == "2006" & data$exam == "g3f1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2006" &
data$exam == "g3f1" & !is.na(data$trk) & data$trk == "Liberal Arts Track"]

data$his[data$cohort == "2006" & data$exam == "g3f1" & !is.na(data$trk) &
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2006" & data$exam == "g3f1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- NA

# 2006_g3k1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
data$gen[data$cohort == "2006" & data$exam == "g3k1" & !is.na(data$trk) &
data$trk == "Science Track"] <- data$his[data$cohort == "2006" &
data$exam == "g3k1" & !is.na(data$trk) & data$trk == "Science Track"]

data$gen[data$cohort == "2006" & data$exam == "g3k1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- data$phy[data$cohort == "2006" &
data$exam == "g3k1" & !is.na(data$trk) & data$trk == "Liberal Arts Track"]

data$his[data$cohort == "2006" & data$exam == "g3k1" & !is.na(data$trk) &
data$trk == "Science Track"] <- NA

data$phy[data$cohort == "2006" & data$exam == "g3k1" & !is.na(data$trk) &
data$trk == "Liberal Arts Track"] <- NA

# Plausibly pure errors

# A "com" score of 128 was erroneously typed as 728
data$com[data$com == 728] <- 128

# Other Notes

# 1) Extra scores in his in Science Track in 2008_g2m2; they are NOT part of tot

# 2) Scores in pol in Science Track in 2009_g2f1/g2m2, but they are part of tot

# 3) Extra scores in pol in Science Track and extra scores in phy in Liberal
# Arts Track in 2011_g2f1; they are NOT part of tot

# 4) Different combinations of subjects are present for 2008_g2m1/g2f1,
# 2009_g1m1/g1f1, and 2011_g2m1; they all sum up to tot

## Study track ====

# Identify students in science track using their exam scores in Grade 3
dat_sci <- data %>% 
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(sci) | (!is.na(phy) & !is.na(che) & !is.na(bio))
  ) %>% 
  select(cssid)

id_sci <- unique(dat_sci$cssid)

# Identify students in liberal arts track using their exam scores in Grade 3
dat_lib <- data %>% 
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
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
# in the above step; 
# For students who have non-NA values in "trk", we use the last
# non-NA value in "trk"
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

## Comprehensive test scores ====

# For later analyses, use comprehensive test scores(sci/lib), do not use scores
# of composing subjects (phy, che, bio/pol, his, geo) or "com" score

data <- data %>%
  # when "sci" or "lib" is NA, first replace with "com"
  mutate(
    sci = case_when(
      is.na(sci) & track == "Science Track" ~ com,
      TRUE ~ sci
    ),
    lib = case_when(
      is.na(lib) & track == "Liberal Arts Track" ~ com,
      TRUE ~ lib
    )
  ) %>% 
  # continue to replace NAs of "sci" and "lib" with the sum of scores of
  # the three composing subjects (when all three subjects have scores)
  mutate(
    sci = case_when(
      is.na(sci) ~ rowSums(select(., phy, che, bio), na.rm = FALSE),
      TRUE ~ sci
    ),
    lib = case_when(
      is.na(lib) ~ rowSums(select(., geo, his, pol), na.rm = FALSE),
      TRUE ~ lib
    )
  )

# Continue to process special cases

# 2004_g1f1_sci (no bio)
data <- data %>% 
  mutate(
    sci = case_when(
      cohort == "2004" & exam == "g1f1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

# 2005_g1m1_sci (no bio)
data <- data %>% 
  mutate(
    sci = case_when(
      cohort == "2005" & exam == "g1m1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

# 2005_g1f1_sci (no bio)
data <- data %>% 
  mutate(
    sci = case_when(
      cohort == "2005" & exam == "g1f1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

## Elite class (wip) ====

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
