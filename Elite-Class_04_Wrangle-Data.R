
# Step 4: Data Wrangling for Analysis

# Run this script before analysis

library(tidyverse)

# Load data ####

dat <- read_rds("Data.rds")

# Reshape data ####

dat <- dat %>%
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

# Create helper functions for data inspection ####

# Extract all observations of n random students from each cohort

sample_dat1 <- function(extra_cols = NULL, n = 2) {
  sample_cssid <- dat %>%
    distinct(cohort, cssid) %>%
    group_by(cohort) %>%
    slice_sample(n = n) %>%
    ungroup()

  base_cols <- c("cohort", "cssid", "name", "exam", "cls", "tot", "chn", "mat",
  "eng", "phy", "che", "bio", "geo", "his", "pol", "sci", "lib", "gen", "com")

  select_cols <- if (!is.null(extra_cols)) {
    c(base_cols, setdiff(extra_cols, base_cols))
  } else {
    base_cols
  }

  sampled_dat <- dat %>%
    semi_join(sample_cssid, by = "cssid") %>%
    select(all_of(select_cols)) %>%
    arrange(cohort, cssid)

  print(sampled_dat, n = Inf)
}

# Extract all observations of n random students from each cohort-track

sample_dat2 <- function(extra_cols = NULL, n = 1) {
  sample_cssid <- dat %>%
    filter(!is.na(track)) %>%
    distinct(cohort, track, cssid) %>%
    group_by(cohort, track) %>%
    slice_sample(n = n) %>%
    ungroup()

  base_cols <- c("cohort", "track", "cssid", "name", "exam", "cls", "tot",
  "chn", "mat", "eng", "phy", "che", "bio", "geo", "his", "pol", "sci", "lib",
  "gen", "com")

  select_cols <- if (!is.null(extra_cols)) {
    c(base_cols, setdiff(extra_cols, base_cols))
  } else {
    base_cols
  }

  sampled_dat <- dat %>%
    semi_join(sample_cssid, by = "cssid") %>%
    select(all_of(select_cols)) %>%
    arrange(cohort, track, cssid)

  print(sampled_dat, n = Inf)
}

# Identify class number set ####

# Create "cls_set" variable for each cohort
dat <- dat %>%
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
dat <- dat %>%
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
dat <- dat %>%
  group_by(cssid, cls_set) %>%
  fill(trk, cls, cid, .direction = "updown") %>%
  ungroup()

# Tidy exam scores ####

## Convert score variables to numeric ====

# First create a vector containing the names of the score variables
score_col <- c("tot", "chn", "mat", "eng", "phy", "che", "bio", "geo",
               "his", "pol", "sci", "lib", "gen", "com")

dat <- dat %>%
  mutate(across(all_of(score_col), as.numeric))

## Replace negative values and 0s with NA ====
dat[score_col][dat[score_col] <= 0] <- NA

## Comprehensive test score replicates ====

# 2004_g3k1, phy == sci
# Replace phy values with NA
dat$phy[dat$cohort == "2004" & dat$exam == "g3k1" & dat$phy == dat$sci] <- NA

# 2006_g3k1, che == sci
# Replace che values with NA
dat$che[dat$cohort == "2006" & dat$exam == "g3k1" & dat$che == dat$sci] <- NA

# 2005_g3m1, bio == lib
# Replace bio values with NA
dat$bio[dat$cohort == "2005" & dat$exam == "g3m1" & dat$bio == dat$lib] <- NA

# 2005_g3m1, geo == sci
# Replace geo values with NA
dat$geo[dat$cohort == "2005" & dat$exam == "g3m1" & dat$geo == dat$sci] <- NA

# 2004_g3k1 & 2006_g3k1, pol == lib
# Replace pol values with NA
dat$pol[dat$cohort %in% c("2004", "2006") & dat$exam == "g3k1" & dat$pol == dat$lib] <- NA

## Ability (gen) scores ====

# 2004_g3m1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2004" & dat$exam == "g3m1" &
dat$trk == "Science Track"] <- dat$his[dat$cohort == "2004" &
dat$exam == "g3m1" & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2004" & dat$exam == "g3m1" &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2004" &
dat$exam == "g3m1" & dat$trk == "Liberal Arts Track"]

dat$his[dat$cohort == "2004" & dat$exam == "g3m1" &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2004" & dat$exam == "g3m1" &
dat$trk == "Liberal Arts Track"] <- NA

# 2004_g3f1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2004" & dat$exam == "g3f1" &
dat$trk == "Science Track"] <- dat$his[dat$cohort == "2004" &
dat$exam == "g3f1" & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2004" & dat$exam == "g3f1" &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2004" &
dat$exam == "g3f1" & dat$trk == "Liberal Arts Track"]

dat$his[dat$cohort == "2004" & dat$exam == "g3f1" &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2004" & dat$exam == "g3f1" &
dat$trk == "Liberal Arts Track"] <- NA

# 2004_g3k1, gen score recorded under pol for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2004" & dat$exam == "g3k1" &
dat$trk == "Science Track"] <- dat$pol[dat$cohort == "2004" &
dat$exam == "g3k1" & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2004" & dat$exam == "g3k1" &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2004" &
dat$exam == "g3k1" & dat$trk == "Liberal Arts Track"]

dat$pol[dat$cohort == "2004" & dat$exam == "g3k1" &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2004" & dat$exam == "g3k1" &
dat$trk == "Liberal Arts Track"] <- NA

# 2005_g2f2, gen score recorded under pol for Science Track and under phy for Liberal Arts Track
# gen scores have been created for this exam in the Import Raw Data step
dat$pol[dat$cohort == "2005" & dat$exam == "g2f2" &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2005" & dat$exam == "g2f2" &
dat$trk == "Liberal Arts Track"] <- NA

# 2005_g3m1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2005" & dat$exam == "g3m1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- dat$his[dat$cohort == "2005" &
dat$exam == "g3m1" & !is.na(dat$trk) & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2005" & dat$exam == "g3m1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2005" &
dat$exam == "g3m1" & !is.na(dat$trk) & dat$trk == "Liberal Arts Track"]

dat$his[dat$cohort == "2005" & dat$exam == "g3m1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2005" & dat$exam == "g3m1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- NA

# 2006_g3f1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2006" & dat$exam == "g3f1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- dat$his[dat$cohort == "2006" &
dat$exam == "g3f1" & !is.na(dat$trk) & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2006" & dat$exam == "g3f1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2006" &
dat$exam == "g3f1" & !is.na(dat$trk) & dat$trk == "Liberal Arts Track"]

dat$his[dat$cohort == "2006" & dat$exam == "g3f1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2006" & dat$exam == "g3f1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- NA

# 2006_g3k1, gen score recorded under his for Science Track and under phy for Liberal Arts Track
dat$gen[dat$cohort == "2006" & dat$exam == "g3k1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- dat$his[dat$cohort == "2006" &
dat$exam == "g3k1" & !is.na(dat$trk) & dat$trk == "Science Track"]

dat$gen[dat$cohort == "2006" & dat$exam == "g3k1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- dat$phy[dat$cohort == "2006" &
dat$exam == "g3k1" & !is.na(dat$trk) & dat$trk == "Liberal Arts Track"]

dat$his[dat$cohort == "2006" & dat$exam == "g3k1" & !is.na(dat$trk) &
dat$trk == "Science Track"] <- NA

dat$phy[dat$cohort == "2006" & dat$exam == "g3k1" & !is.na(dat$trk) &
dat$trk == "Liberal Arts Track"] <- NA

## Plausibly pure errors ====

# A com score of 128 was erroneously typed as 728
dat$com[dat$com == 728] <- 128

## Other Notes ====

# 1) Extra scores in his in Science Track in 2008_g2m2; they are NOT part of tot

# 2) Scores in pol in Science Track in 2009_g2f1/g2m2, but they are part of tot

# 3) Extra scores in pol in Science Track and extra scores in phy in Liberal
# Arts Track in 2011_g2f1; they are NOT part of tot

# 4) Different combinations of subjects are present for 2008_g2m1/g2f1,
# 2009_g1m1/g1f1, and 2011_g2m1; they all sum up to tot

# Identify study track ####

# Identify students in science track using their exam scores in Grade 3
id_sci <- dat %>%
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(sci) | (!is.na(phy) & !is.na(che) & !is.na(bio))
  ) %>%
  pull(cssid) %>%
  unique()

# Identify students in liberal arts track using their exam scores in Grade 3
id_lib <- dat %>%
  filter(exam %in% c("g3m1", "g3f1", "g3k1", "g3k2", "cee")) %>%
  filter(
    !is.na(lib) | (!is.na(pol) & !is.na(his) & !is.na(geo))
  ) %>%
  pull(cssid) %>%
  unique()

# Assign value to "track" variable
dat <- dat %>%
  mutate(
    track = case_when(
      cssid %in% id_sci ~ "Science Track",
      cssid %in% id_lib ~ "Liberal Arts Track",
      TRUE ~ NA
    )
  )

# Continue with students who remain unidentified in the above step

# For students who have non-NA values in "trk", use the last
# non-NA value in "trk"
dat <- dat %>%
  group_by(cssid) %>%
  mutate(
    track = case_when(
      !is.na(track) ~ track,
      TRUE ~ last(na.omit(trk))
    )
  ) %>%
  ungroup()

# Finally, use "btrack" variable
dat <- dat %>%
  mutate(
    track = case_when(
      is.na(track) ~ btrack,
      TRUE ~ track
    )
  )

# Tidy comprehensive test scores ####

# For later analyses, use comprehensive test scores(sci/lib), do not use scores
# of composing subjects (phy, che, bio/pol, his, geo) or com score

dat <- dat %>%
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
dat <- dat %>%
  mutate(
    sci = case_when(
      cohort == "2004" & exam == "g1f1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

# 2005_g1m1_sci (no bio)
dat <- dat %>%
  mutate(
    sci = case_when(
      cohort == "2005" & exam == "g1m1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

# 2005_g1f1_sci (no bio)
dat <- dat %>%
  mutate(
    sci = case_when(
      cohort == "2005" & exam == "g1f1" ~ rowSums(select(., phy, che), na.rm = FALSE),
      TRUE ~ sci
    )
  )

# Identify elite class membership ####

# Cohort 2003
id_elite03_1 <- dat %>%
  filter(cohort == "2003", cls_set == "1", cls %in% c("11", "12")) %>%
  pull(cssid) %>%
  unique()

id_elite03_2 <- dat %>%
  filter(cohort == "2003", cls_set == "2", cls %in% c("9", "10")) %>%
  pull(cssid) %>%
  unique()

id_elite03 <- intersect(id_elite03_1, id_elite03_2)

# Cohort 2004
id_elite04_s1 <- dat %>%
  filter(cohort == "2004", cls_set == "1", cls %in% c("1", "2")) %>%
  pull(cssid) %>%
  unique()

id_elite04_s2 <- dat %>%
  filter(cohort == "2004", cls_set == "2", cls %in% c("1", "2")) %>%
  pull(cssid) %>%
  unique()

id_elite04_s <- intersect(id_elite04_s1, id_elite04_s2)

id_elite04_l <- dat %>%
  filter(cohort == "2004", cls_set == "2", cls == "23") %>%
  pull(cssid) %>%
  unique()

id_elite04 <- union(id_elite04_s, id_elite04_l)

# Cohort 2005
id_elite05_1 <- dat %>%
  filter(cohort == "2005", cls_set == "1", cls %in% c("9", "10", "11")) %>%
  pull(cssid) %>%
  unique()

id_elite05_2 <- dat %>%
  filter(cohort == "2005", cls_set == "2", cls %in% c("8", "11", "12")) %>%
  pull(cssid) %>%
  unique()

id_elite05 <- intersect(id_elite05_1, id_elite05_2)

# Cohort 2006
id_elite06_1 <- dat %>%
  filter(cohort == "2006", cls_set == "1", cls %in% c("23", "24", "25")) %>%
  pull(cssid) %>%
  unique()

id_elite06_2 <- dat %>%
  filter(cohort == "2006", cls_set == "2", cls %in% c("23", "24", "25")) %>%
  pull(cssid) %>%
  unique()

id_elite06_3 <- dat %>%
  filter(cohort == "2006", cls_set == "3", cls %in% c("23", "24", "25")) %>%
  pull(cssid) %>%
  unique()

id_elite06 <- Reduce(intersect, list(id_elite06_1, id_elite06_2, id_elite06_3))

# Cohort 2007
id_elite07_1 <- dat %>%
  filter(cohort == "2007", cls_set == "1", cls %in% c("1", "2", "23", "24")) %>%
  pull(cssid) %>%
  unique()

id_elite07_2 <- dat %>%
  filter(cohort == "2007", cls_set == "2", cls %in% c("1", "2", "23", "24")) %>%
  pull(cssid) %>%
  unique()

id_elite07 <- intersect(id_elite07_1, id_elite07_2)

# Create dummy variable indicating elite class membership
dat <- dat %>%
  mutate(
    cls_elite = case_when(
      cssid %in%
      c(id_elite03, id_elite04, id_elite05, id_elite06, id_elite07) ~ "Elite Class",
      TRUE ~ "Regular Class"
    )
  )

# Identify true elites ####

## Cohorts 2003 - 2007 ====

# Identify students who were "true elites" in Cohorts 2003 - 2007
# (i.e., those who were in the elite classes and whose hsee scores were
# higher than the highest score in the regular classes [not including
# a few top scorers who chose to stay in regular classes])

# Cohort 2003

id_elitetrue03_sci <- dat %>%
  filter(cohort == "2003", track == "Science Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 579) %>%
  pull(cssid) %>%
  unique()

# Cohort 2004
# (Cutoff point value for cohort 2004 liberal arts track elite class is 599
# using the average of total scores of g1m1, g1f1, g1m2, and g1f2)

id_elitetrue04_sci <- dat %>%
  filter(cohort == "2004", track == "Science Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 596) %>%
  pull(cssid) %>%
  unique()

# Cohort 2005

id_elitetrue05_sci <- dat %>%
  filter(cohort == "2005", track == "Science Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 594.5) %>%
  pull(cssid) %>%
  unique()

id_elitetrue05_lib <- dat %>%
  filter(cohort == "2005", track == "Liberal Arts Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 573) %>%
  pull(cssid) %>%
  unique()

# Cohort 2006

id_elitetrue06_sci <- dat %>%
  filter(cohort == "2006", track == "Science Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 595) %>%
  pull(cssid) %>%
  unique()

id_elitetrue06_lib <- dat %>%
  filter(cohort == "2006", track == "Liberal Arts Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 586.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2007

id_elitetrue07_sci <- dat %>%
  filter(cohort == "2007", track == "Science Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 606) %>%
  pull(cssid) %>%
  unique()

id_elitetrue07_lib <- dat %>%
  filter(cohort == "2007", track == "Liberal Arts Track",
         cls_elite == "Elite Class", exam == "hsee", tot >= 556) %>%
  pull(cssid) %>%
  unique()

# Create dummy variable indicating "true elites"
dat <- dat %>%
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

## Cohorts 2008 - 2014 ====

# Define students who were "true elites" in Cohorts 2008 - 2014
# 501 "true elites" in Science Track in Cohorts 2003 - 2007 (average 100/cohort)
# 212 "true elites" in Liberal Arts Track in Cohorts 2005 - 2007 (average 71/cohort)

# Identify students whose hsee scores were higher or equal to the 100th/71st
# highest score in the Science Track/Liberal Arts Track

# Cohort 2008

id_elitetrue08_sci <- dat %>%
  filter(cohort == "2008", track == "Science Track",
         exam == "hsee", tot >= 647) %>%
  pull(cssid) %>%
  unique()

id_elitetrue08_lib <- dat %>%
  filter(cohort == "2008", track == "Liberal Arts Track",
         exam == "hsee", tot >= 625.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2009

id_elitetrue09_sci <- dat %>%
  filter(cohort == "2009", track == "Science Track",
         exam == "hsee", tot >= 692.5) %>%
  pull(cssid) %>%
  unique()

id_elitetrue09_lib <- dat %>%
  filter(cohort == "2009", track == "Liberal Arts Track",
         exam == "hsee", tot >= 667.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2010

id_elitetrue10_sci <- dat %>%
  filter(cohort == "2010", track == "Science Track",
         exam == "hsee", tot >= 686) %>%
  pull(cssid) %>%
  unique()

id_elitetrue10_lib <- dat %>%
  filter(cohort == "2010", track == "Liberal Arts Track",
         exam == "hsee", tot >= 652) %>%
  pull(cssid) %>%
  unique()

# Cohort 2011

id_elitetrue11_sci <- dat %>%
  filter(cohort == "2011", track == "Science Track",
         exam == "hsee", tot >= 741) %>%
  pull(cssid) %>%
  unique()

id_elitetrue11_lib <- dat %>%
  filter(cohort == "2011", track == "Liberal Arts Track",
         exam == "hsee", tot >= 721.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2012

id_elitetrue12_sci <- dat %>%
  filter(cohort == "2012", track == "Science Track",
         exam == "hsee", tot >= 726) %>%
  pull(cssid) %>%
  unique()

id_elitetrue12_lib <- dat %>%
  filter(cohort == "2012", track == "Liberal Arts Track",
         exam == "hsee", tot >= 703) %>%
  pull(cssid) %>%
  unique()

# Cohort 2013

id_elitetrue13_sci <- dat %>%
  filter(cohort == "2013", track == "Science Track",
         exam == "hsee", tot >= 761) %>%
  pull(cssid) %>%
  unique()

id_elitetrue13_lib <- dat %>%
  filter(cohort == "2013", track == "Liberal Arts Track",
         exam == "hsee", tot >= 739) %>%
  pull(cssid) %>%
  unique()

# Cohort 2014

id_elitetrue14_sci <- dat %>%
  filter(cohort == "2014", track == "Science Track",
         exam == "hsee", tot >= 749) %>%
  pull(cssid) %>%
  unique()

id_elitetrue14_lib <- dat %>%
  filter(cohort == "2014", track == "Liberal Arts Track",
         exam == "hsee", tot >= 729) %>%
  pull(cssid) %>%
  unique()

# Assign "Yes" to "true elites" in cohorts 2008 - 2014
dat <- dat %>%
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

# Standardize exam scores within cohort-track-exam ####

dat <- dat %>%
  group_by(cohort, track, exam) %>%
  mutate(
    across(
      .cols = tot:com,
      .fns = ~ scale(.)[, 1],
      .names = "z{.col}"
    )
  ) %>%
  ungroup()

# Center total score of hsee around the cutoffs for true elites ####

# Note that the centered hsee scores only apply to
# (1) Science Track of Cohorts 2003 - 2007
# (2) Liberal Arts Track of Cohorts 2005 - 2007

ctot <- dat %>%
  select(cohort, cssid, track, exam, tot) %>%
  filter(exam == "hsee") %>%
  group_by(cohort, track) %>%
  # method 1: center tot at cutoffs then divided by standard deviation
  # (equivalent to centering ztot at cutoffs)
  mutate(
    hsee_ctot1 = case_when(
      cohort == "2003" & track == "Science Track"
      ~ (tot - 579) / sd(tot, na.rm = TRUE),
      cohort == "2004" & track == "Science Track"
      ~ (tot - 596) / sd(tot, na.rm = TRUE),
      cohort == "2005" & track == "Science Track"
      ~ (tot - 594.5) / sd(tot, na.rm = TRUE),
      cohort == "2006" & track == "Science Track"
      ~ (tot - 595) / sd(tot, na.rm = TRUE),
      cohort == "2007" & track == "Science Track"
      ~ (tot - 606) / sd(tot, na.rm = TRUE),
      cohort == "2005" & track == "Liberal Arts Track"
      ~ (tot - 573) / sd(tot, na.rm = TRUE),
      cohort == "2006" & track == "Liberal Arts Track"
      ~ (tot - 586.5) / sd(tot, na.rm = TRUE),
      cohort == "2007" & track == "Liberal Arts Track"
      ~ (tot - 556) / sd(tot, na.rm = TRUE),
      TRUE ~ NA
    )
  ) %>%
  # method 2: center tot at cutoffs then divided by root mean square
  mutate(
    hsee_ctot2 = case_when(
      cohort == "2003" & track == "Science Track"
      ~ scale(tot, center = 579)[, 1],
      cohort == "2004" & track == "Science Track"
      ~ scale(tot, center = 596)[, 1],
      cohort == "2005" & track == "Science Track"
      ~ scale(tot, center = 594.5)[, 1],
      cohort == "2006" & track == "Science Track"
      ~ scale(tot, center = 595)[, 1],
      cohort == "2007" & track == "Science Track"
      ~ scale(tot, center = 606)[, 1],
      cohort == "2005" & track == "Liberal Arts Track"
      ~ scale(tot, center = 573)[, 1],
      cohort == "2006" & track == "Liberal Arts Track"
      ~ scale(tot, center = 586.5)[, 1],
      cohort == "2007" & track == "Liberal Arts Track"
      ~ scale(tot, center = 556)[, 1],
      TRUE ~ NA
    )
  ) %>%
  ungroup()

# Join the centered variables
dat <- dat %>%
  left_join(
    select(ctot, cssid, hsee_ctot1, hsee_ctot2),
    by = "cssid",
    na_matches = "never",
    relationship = "many-to-one"
  )

# Filter Data ####

# Filter out students whose track is NA and who did not have hsee score
id_natrack <- dat %>%
  filter(is.na(track)) %>%
  pull(cssid) %>%
  unique()

id_nahsee <- dat %>%
  filter(exam == "hsee", is.na(tot)) %>%
  pull(cssid) %>%
  unique()

id_excl <- union(id_natrack, id_nahsee)

dat <- dat %>%
  filter(!cssid %in% id_excl)
