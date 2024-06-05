
library(tidyverse)

# Data Wrangling ####

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
id_sci <- data %>% 
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(sci) | (!is.na(phy) & !is.na(che) & !is.na(bio))
  ) %>% 
  pull(cssid) %>%
  unique()

# Identify students in liberal arts track using their exam scores in Grade 3
id_lib <- data %>% 
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(lib) | (!is.na(pol) & !is.na(his) & !is.na(geo))
  ) %>% 
  pull(cssid) %>%
  unique()

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

## Elite class membership ====

# Identify students who stayed in elite classes throughout all three grades

# Cohort 2003
id_elite03_1 <- data %>%
  filter(cohort == "2003", cls_set == "1", cls %in% c("11", "12")) %>%
  pull(cssid) %>%
  unique()

id_elite03_2 <- data %>%
  filter(cohort == "2003", cls_set == "2", cls %in% c("9", "10")) %>%
  pull(cssid) %>%
  unique()

id_elite03 <- intersect(id_elite03_1, id_elite03_2)

# Cohort 2004
id_elite04_1 <- data %>%
  filter(cohort == "2004", cls_set == "1", cls %in% c("1", "2")) %>%
  pull(cssid) %>%
  unique()

id_elite04_2 <- data %>%
  filter(cohort == "2004", cls_set == "2", cls == "23") %>%
  pull(cssid) %>%
  unique()

id_elite04 <- union(id_elite04_1, id_elite04_2)

# Cohort 2005
id_elite05_1 <- data %>%
  filter(cohort == "2005", cls_set == "1", cls %in% c("9", "10", "11")) %>%
  pull(cssid) %>%
  unique()

id_elite05_2 <- data %>%
  filter(cohort == "2005", cls_set == "2", cls %in% c("8", "11", "12")) %>%
  pull(cssid) %>%
  unique()

id_elite05 <- intersect(id_elite05_1, id_elite05_2)

# Cohort 2006
id_elite06_1 <- data %>%
  filter(cohort == "2006", cls_set == "1", cls %in% c("23", "24", "25")) %>%
  pull(cssid) %>%
  unique()

id_elite06_2 <- data %>%
  filter(cohort == "2006", cls_set == "3", cls %in% c("23", "24", "25")) %>%
  pull(cssid) %>%
  unique()

id_elite06 <- intersect(id_elite06_1, id_elite06_2)

# Cohort 2007
id_elite07_1 <- data %>%
  filter(cohort == "2007", cls_set == "1", cls %in% c("1", "2", "23", "24")) %>%
  pull(cssid) %>%
  unique()

id_elite07_2 <- data %>%
  filter(cohort == "2007", cls_set == "2", cls %in% c("1", "2", "23", "24")) %>%
  pull(cssid) %>%
  unique()

id_elite07 <- intersect(id_elite07_1, id_elite07_2)

# Create dummy variable indicating elite class membership
data <- data %>%
  mutate(
    cls_elite = case_when(
      cssid %in%
      c(id_elite03, id_elite04, id_elite05, id_elite06, id_elite07) ~ "Elite Class",
      TRUE ~ "Regular Class"
    )
  )

## Elites ====

# Identify students who were "true elites" in Cohorts 2003 - 2007
# (i.e., those who were in the elite classes and whose hsee scores were
# higher than the highest score in the regular classes [not including
# a few top scorers who chose to stay in regular classes])

# Cohort 2003
id_elitetrue03_sci <- data %>% 
  filter(cohort == "2003", track == "Science Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 579) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2004
# (Cutoff point value for cohort 2004 liberal arts track elite class is 599 
# using the average of total scores of g1m1, g1f1, g1m2, and g1f2)
id_elitetrue04_sci <- data %>% 
  filter(cohort == "2004", track == "Science Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 596) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2005
id_elitetrue05_sci <- data %>% 
  filter(cohort == "2005", track == "Science Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 594.5) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue05_lib <- data %>% 
  filter(cohort == "2005", track == "Liberal Arts Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 573) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2006
id_elitetrue06_sci <- data %>% 
  filter(cohort == "2006", track == "Science Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 595) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue06_lib <- data %>% 
  filter(cohort == "2006", track == "Liberal Arts Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 586.5) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2007
id_elitetrue07_sci <- data %>% 
  filter(cohort == "2007", track == "Science Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 606) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue07_lib <- data %>% 
  filter(cohort == "2007", track == "Liberal Arts Track", 
         cls_elite == "Elite Class", exam == "hsee", tot >= 556) %>%
  pull(cssid) %>% 
  unique()

# Create dummy variable indicating true elites
data <- data %>%
  mutate(
    elite = case_when(
      cssid %in% c(
        id_elitetrue03_sci,
        id_elitetrue04_sci,
        id_elitetrue05_sci,
        id_elitetrue06_sci,
        id_elitetrue07_sci,
        id_elitetrue05_lib,
        id_elitetrue06_lib,
        id_elitetrue07_lib
      ) ~ "Yes",
      TRUE ~ "No"
    )
  )

# Define students who were "true elites" in Cohorts 2008 - 2014
# 501 "true elites" in Science Track in Cohorts 2003 - 2007 (average 100/cohort)
# 212 "true elites" in Liberal Arts Track in Cohorts 2005 - 2007 (average 71/cohort)

# Identify students whose hsee scores were higher or equal to the 100th/71st
# highest score in the Science Track/Liberal Arts Track

# Cohort 2008
id_elitetrue08_sci <- data %>% 
  filter(cohort == "2008", track == "Science Track", 
         exam == "hsee", tot >= 647) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue08_lib <- data %>% 
  filter(cohort == "2008", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 625.5) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2009
id_elitetrue09_sci <- data %>% 
  filter(cohort == "2009", track == "Science Track", 
         exam == "hsee", tot >= 692.5) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue09_lib <- data %>% 
  filter(cohort == "2009", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 667.5) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2010
id_elitetrue10_sci <- data %>% 
  filter(cohort == "2010", track == "Science Track", 
         exam == "hsee", tot >= 686) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue10_lib <- data %>% 
  filter(cohort == "2010", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 652) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2011
id_elitetrue11_sci <- data %>% 
  filter(cohort == "2011", track == "Science Track", 
         exam == "hsee", tot >= 741) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue11_lib <- data %>% 
  filter(cohort == "2011", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 721.5) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2012
id_elitetrue12_sci <- data %>% 
  filter(cohort == "2012", track == "Science Track", 
         exam == "hsee", tot >= 726) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue12_lib <- data %>% 
  filter(cohort == "2012", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 703) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2013
id_elitetrue13_sci <- data %>% 
  filter(cohort == "2013", track == "Science Track", 
         exam == "hsee", tot >= 761) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue13_lib <- data %>% 
  filter(cohort == "2013", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 739) %>%
  pull(cssid) %>% 
  unique()

# Cohort 2014
id_elitetrue14_sci <- data %>% 
  filter(cohort == "2014", track == "Science Track", 
         exam == "hsee", tot >= 749) %>%
  pull(cssid) %>% 
  unique()

id_elitetrue14_lib <- data %>% 
  filter(cohort == "2014", track == "Liberal Arts Track", 
         exam == "hsee", tot >= 729) %>%
  pull(cssid) %>% 
  unique()

# Assign "Yes" to true elites in cohorts 2008 - 2014
data <- data %>%
  mutate(
    elite = case_when(
      cssid %in% c(
        id_elitetrue08_sci,
        id_elitetrue09_sci,
        id_elitetrue10_sci,
        id_elitetrue11_sci,
        id_elitetrue12_sci,
        id_elitetrue13_sci,
        id_elitetrue14_sci,
        id_elitetrue08_lib,
        id_elitetrue09_lib,
        id_elitetrue10_lib,
        id_elitetrue11_lib,
        id_elitetrue12_lib,
        id_elitetrue13_lib,
        id_elitetrue14_lib
      ) ~ "Yes",
      TRUE ~ elite
    )
  )

## Standardize exam scores within cohort-track-exam ====
data <- data %>% 
  group_by(cohort, track, exam) %>% 
  mutate(
    across(
      .cols = tot:com,
      .fns = ~ scale(.)[, 1],
      .names = "z{.col}"
    )
  ) %>%
  # standardize hsee_tot around the cutoffs for "true elites"
  mutate(
    hsee_ctot = case_when(
      exam == "hsee" & cohort == "2003" & track == "Science Track"
      ~ scale(tot, center = 579)[, 1],
      exam == "hsee" & cohort == "2004" & track == "Science Track"
      ~ scale(tot, center = 596)[, 1],
      exam == "hsee" & cohort == "2005" & track == "Science Track"
      ~ scale(tot, center = 594.5)[, 1],
      exam == "hsee" & cohort == "2006" & track == "Science Track"
      ~ scale(tot, center = 595)[, 1],
      exam == "hsee" & cohort == "2007" & track == "Science Track"
      ~ scale(tot, center = 606)[, 1],
      exam == "hsee" & cohort == "2008" & track == "Science Track"
      ~ scale(tot, center = 647)[, 1],
      exam == "hsee" & cohort == "2009" & track == "Science Track"
      ~ scale(tot, center = 692.5)[, 1],
      exam == "hsee" & cohort == "2010" & track == "Science Track"
      ~ scale(tot, center = 686)[, 1],
      exam == "hsee" & cohort == "2011" & track == "Science Track"
      ~ scale(tot, center = 741)[, 1],
      exam == "hsee" & cohort == "2012" & track == "Science Track"
      ~ scale(tot, center = 726)[, 1],
      exam == "hsee" & cohort == "2013" & track == "Science Track"
      ~ scale(tot, center = 761)[, 1],
      exam == "hsee" & cohort == "2014" & track == "Science Track"
      ~ scale(tot, center = 749)[, 1],
      exam == "hsee" & cohort == "2005" & track == "Liberal Arts Track"
      ~ scale(tot, center = 573)[, 1],
      exam == "hsee" & cohort == "2006" & track == "Liberal Arts Track"
      ~ scale(tot, center = 586.5)[, 1],
      exam == "hsee" & cohort == "2007" & track == "Liberal Arts Track"
      ~ scale(tot, center = 556)[, 1],
      exam == "hsee" & cohort == "2008" & track == "Liberal Arts Track"
      ~ scale(tot, center = 625.5)[, 1],
      exam == "hsee" & cohort == "2009" & track == "Liberal Arts Track"
      ~ scale(tot, center = 667.5)[, 1],
      exam == "hsee" & cohort == "2010" & track == "Liberal Arts Track"
      ~ scale(tot, center = 652)[, 1],
      exam == "hsee" & cohort == "2011" & track == "Liberal Arts Track"
      ~ scale(tot, center = 721.5)[, 1],
      exam == "hsee" & cohort == "2012" & track == "Liberal Arts Track"
      ~ scale(tot, center = 703)[, 1],
      exam == "hsee" & cohort == "2013" & track == "Liberal Arts Track"
      ~ scale(tot, center = 739)[, 1],
      exam == "hsee" & cohort == "2014" & track == "Liberal Arts Track"
      ~ scale(tot, center = 729)[, 1],
      TRUE ~ NA
    )
  ) %>%
  ungroup()

# Save Data ####
write_rds(data, "Data-Tidy.rds")