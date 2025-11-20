
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Elite Class Project
# Step 4: Data Wrangling for Analysis
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Load packages
library(tidyverse)

# Load data
dat <- read_rds("Data.rds")

# Helper Functions for Data Inspection ####

# First create vectors containing the names of the score variables

score_col <- c("tot", "chn", "mat", "eng", "phy", "che", "bio", "geo",
               "his", "pol", "sci", "lib", "gen", "com")

zscore_col <- paste0("z", score_col)

# Extract all observations of n random students from each cohort
sample_dat0 <- function(extra_cols = NULL, n = 2) {
  sample_cssid <- dat %>%
    distinct(cohort, cssid) %>%
    group_by(cohort) %>%
    slice_sample(n = n) %>%
    ungroup()

  base_cols <- c("cohort", "cssid", "name", "exam", "cls", score_col,
                 "head_tname", "head_tmale")

  select_cols <- if (!is.null(extra_cols)) {
    c(base_cols, setdiff(extra_cols, base_cols))
  } else {
    base_cols
  }

  sampled_dat <- dat %>%
    semi_join(sample_cssid, by = "cssid") %>%
    select(all_of(select_cols)) %>%
    arrange(cohort, cssid)

  return(sampled_dat)
}

# Extract all observations of n random students from each cohort-track
sample_dat <- function(extra_cols = NULL, n = 1) {
  sample_cssid <- dat %>%
    filter(!is.na(track)) %>%
    distinct(cohort, track, cssid) %>%
    group_by(cohort, track) %>%
    slice_sample(n = n) %>%
    ungroup()

  base_cols <- c("cohort", "track", "cssid", "name", "exam", "cls", zscore_col,
                 "head_tname", "head_tmale")

  select_cols <- if (!is.null(extra_cols)) {
    c(base_cols, setdiff(extra_cols, base_cols))
  } else {
    base_cols
  }

  sampled_dat <- dat %>%
    semi_join(sample_cssid, by = "cssid") %>%
    select(all_of(select_cols)) %>%
    arrange(cohort, track, cssid)

  return(sampled_dat)
}

# Study Track ####

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

# For students who have non-NA values in "trk6d" or "trk", use the last
# non-NA value in "trk6d" or "trk"
dat <- dat %>%
  group_by(cssid) %>%
  mutate(
    track = case_when(
      !is.na(track) ~ track,
      TRUE ~ last(na.omit(trk6d))
    ),
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

# Comprehensive Test (sci/lib) Scores ####

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

# Elite Class Membership ####

# Create a dummy variable indicating elite class policy treatment
dat <- dat %>%
  mutate(
    policy = case_when(
      cohort %in% c("2003", "2004", "2005", "2006", "2007") ~ "Treated",
      TRUE ~ "Untreated"
    )
  )

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

# Create a dummy variable indicating elite class membership
dat <- dat %>%
  mutate(
    cls_elite = case_when(
      cssid %in% c(id_elite03, id_elite04, id_elite05, id_elite06, id_elite07)
      ~ "Elite Class",
      policy == "Treated" &
      !cssid %in% c(id_elite03, id_elite04, id_elite05, id_elite06, id_elite07)
      ~ "Regular Class",
      TRUE ~ NA
    )
  )

# Top Scorers in hsee ####

## Cohorts 2003 - 2007 ====

# For cohorts 2003 - 2007, top scorers are those whose total scores in HSEE are
# higher than the highest score in the regular classes

# Importantly, when identifying the highest score in the regular classes, we
# do not count a few top scorers who chose to stay in regular classes but whose
# total scores in HSEE are discontinuously higher than the rest of the regular
# class

# Cohort 2003

id_topscore03_sci <- dat %>%
  filter(cohort == "2003", track == "Science Track",
         exam == "hsee", tot >= 579) %>%
  pull(cssid) %>%
  unique()

# Cohort 2004
# (Cutoff point value for cohort 2004 liberal arts track elite class is 599
# using the average of total scores of g1m1, g1f1, g1m2, and g1f2)

id_topscore04_sci <- dat %>%
  filter(cohort == "2004", track == "Science Track",
         exam == "hsee", tot >= 596) %>%
  pull(cssid) %>%
  unique()

# Cohort 2005

id_topscore05_sci <- dat %>%
  filter(cohort == "2005", track == "Science Track",
         exam == "hsee", tot >= 594.5) %>%
  pull(cssid) %>%
  unique()

id_topscore05_lib <- dat %>%
  filter(cohort == "2005", track == "Liberal Arts Track",
         exam == "hsee", tot >= 573) %>%
  pull(cssid) %>%
  unique()

# Cohort 2006

id_topscore06_sci <- dat %>%
  filter(cohort == "2006", track == "Science Track",
         exam == "hsee", tot >= 595) %>%
  pull(cssid) %>%
  unique()

id_topscore06_lib <- dat %>%
  filter(cohort == "2006", track == "Liberal Arts Track",
         exam == "hsee", tot >= 586.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2007

id_topscore07_sci <- dat %>%
  filter(cohort == "2007", track == "Science Track",
         exam == "hsee", tot >= 606) %>%
  pull(cssid) %>%
  unique()

id_topscore07_lib <- dat %>%
  filter(cohort == "2007", track == "Liberal Arts Track",
         exam == "hsee", tot >= 556) %>%
  pull(cssid) %>%
  unique()

# Create a dummy variable indicating "top scorers" in HSEE
dat <- dat %>%
  mutate(
    top_scorer = case_when(
      cssid %in% c(
        id_topscore03_sci,
        id_topscore04_sci,
        id_topscore05_sci,
        id_topscore06_sci,
        id_topscore07_sci,
        id_topscore05_lib,
        id_topscore06_lib,
        id_topscore07_lib
      ) ~ "Yes",
      TRUE ~ "No"
    )
  )

## Cohorts 2008 - 2014 ====

# Define students who were "top scorers" in Cohorts 2008 - 2014

# (1) 501 "top scorers" in Science Track elite classes in Cohorts 2003 - 2007
# (average 100/cohort)

# (2) 212 "top scorers" in Liberal Arts Track elite classes in Cohorts
# 2005 - 2007 (average 71/cohort)

# Therefore, students whose HSEE total scores were higher or equal to the
# 100th/71st highest score in the Science Track/Liberal Arts Track were
# identified as "top scorers" in Cohorts 2008 - 2014

# Cohort 2008

id_topscore08_sci <- dat %>%
  filter(cohort == "2008", track == "Science Track",
         exam == "hsee", tot >= 647) %>%
  pull(cssid) %>%
  unique()

id_topscore08_lib <- dat %>%
  filter(cohort == "2008", track == "Liberal Arts Track",
         exam == "hsee", tot >= 625.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2009

id_topscore09_sci <- dat %>%
  filter(cohort == "2009", track == "Science Track",
         exam == "hsee", tot >= 692.5) %>%
  pull(cssid) %>%
  unique()

id_topscore09_lib <- dat %>%
  filter(cohort == "2009", track == "Liberal Arts Track",
         exam == "hsee", tot >= 667.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2010

id_topscore10_sci <- dat %>%
  filter(cohort == "2010", track == "Science Track",
         exam == "hsee", tot >= 686) %>%
  pull(cssid) %>%
  unique()

id_topscore10_lib <- dat %>%
  filter(cohort == "2010", track == "Liberal Arts Track",
         exam == "hsee", tot >= 652) %>%
  pull(cssid) %>%
  unique()

# Cohort 2011

id_topscore11_sci <- dat %>%
  filter(cohort == "2011", track == "Science Track",
         exam == "hsee", tot >= 741) %>%
  pull(cssid) %>%
  unique()

id_topscore11_lib <- dat %>%
  filter(cohort == "2011", track == "Liberal Arts Track",
         exam == "hsee", tot >= 721.5) %>%
  pull(cssid) %>%
  unique()

# Cohort 2012

id_topscore12_sci <- dat %>%
  filter(cohort == "2012", track == "Science Track",
         exam == "hsee", tot >= 726) %>%
  pull(cssid) %>%
  unique()

id_topscore12_lib <- dat %>%
  filter(cohort == "2012", track == "Liberal Arts Track",
         exam == "hsee", tot >= 703) %>%
  pull(cssid) %>%
  unique()

# Cohort 2013

id_topscore13_sci <- dat %>%
  filter(cohort == "2013", track == "Science Track",
         exam == "hsee", tot >= 761) %>%
  pull(cssid) %>%
  unique()

id_topscore13_lib <- dat %>%
  filter(cohort == "2013", track == "Liberal Arts Track",
         exam == "hsee", tot >= 739) %>%
  pull(cssid) %>%
  unique()

# Cohort 2014

id_topscore14_sci <- dat %>%
  filter(cohort == "2014", track == "Science Track",
         exam == "hsee", tot >= 749) %>%
  pull(cssid) %>%
  unique()

id_topscore14_lib <- dat %>%
  filter(cohort == "2014", track == "Liberal Arts Track",
         exam == "hsee", tot >= 729) %>%
  pull(cssid) %>%
  unique()

# Assign "Yes" to "top_scorer" in cohorts 2008 - 2014
dat <- dat %>%
  mutate(
    top_scorer = case_when(
      cssid %in% c(
        id_topscore08_sci,
        id_topscore09_sci,
        id_topscore10_sci,
        id_topscore11_sci,
        id_topscore12_sci,
        id_topscore13_sci,
        id_topscore14_sci,
        id_topscore08_lib,
        id_topscore09_lib,
        id_topscore10_lib,
        id_topscore11_lib,
        id_topscore12_lib,
        id_topscore13_lib,
        id_topscore14_lib
      ) ~ "Yes",
      TRUE ~ top_scorer
    )
  )

## Variable "elite" ====

# Create variable "elite" to indicate four types of students
dat <- dat %>%
  mutate(
    elite = case_when(
      policy == "Treated" & cls_elite == "Elite Class" & top_scorer == "Yes"
      ~ "Elite Students",
      policy == "Untreated" & top_scorer == "Yes"
      ~ "Elite Students",
      policy == "Treated" & cls_elite == "Regular Class" & top_scorer == "Yes"
      ~ "Regular Class Top Scorers",
      policy == "Treated" & cls_elite == "Elite Class" & top_scorer == "No"
      ~ "Elite Class Non-Top Scorers",
      TRUE ~ "Regular Students"
    )
  )

# Exam Score Trimming, Standardization, and Centering ####

## Score Trimming ====

# Trim the lowest 5% of scores in each subject within cohort-track-exam
dat <- dat %>%
  group_by(cohort, track, exam) %>%
  mutate(
    across(
      .cols = tot:com,
      .fns = ~ ifelse(. < quantile(., 0.05, na.rm = TRUE), NA, .),
      .names = "{.col}_trim"
    )
  ) %>%
  ungroup()

## Score Standardization ====

# Standardize exam scores within cohort-track-exam

# Untrimmed scores
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

# Trimmed scores
dat <- dat %>%
  group_by(cohort, track, exam) %>%
  mutate(
    across(
      .cols = tot_trim:com_trim,
      .fns = ~ scale(.)[, 1],
      .names = "z{.col}"
    )
  ) %>%
  ungroup()

## Centering Total Score of HSEE ====

# Center total score of hsee (untrimmed) around the cutoffs for top scorers

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
    hsee_ctot = case_when(
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
    select(ctot, cssid, hsee_ctot, hsee_ctot2),
    by = "cssid",
    na_matches = "never",
    relationship = "many-to-one"
  )

# Parental Employment Status ####

# Divide parental employment status into two levels
  # Low Status Jobs: Peasant, Unstable Occupation, Self-Employed
  # High Status Jobs: Enterprise Employee, Public Institution Employee, Civil Servant/Military Man

# Create an index for combined status of parents, with three levels
  # Both Parents in High Status Jobs
  # One Parent in High Status Job
  # Neither Parent in High Status Jobs

dat <- dat %>%
  mutate(
    f_job_l2 = factor(
      case_when(
        is.na(f_job) ~ NA_character_,
        f_job %in% c("Peasant", "Unstable Occupation", "Self-Employed")
        ~ "Low Status Jobs",
        TRUE ~ "High Status Jobs"
      ),
      levels = c("Low Status Jobs", "High Status Jobs")
    ),
    m_job_l2 = factor(
      case_when(
        is.na(m_job) ~ NA_character_,
        m_job %in% c("Peasant", "Unstable Occupation", "Self-Employed")
        ~ "Low Status Jobs",
        TRUE ~ "High Status Jobs"
      ),
      levels = c("Low Status Jobs", "High Status Jobs")
    ),
    par_job = factor(
      case_when(
        is.na(f_job_l2) & is.na(m_job_l2) ~ NA_character_,
        f_job_l2 == "High Status Jobs" & m_job_l2 == "High Status Jobs"
        ~ "Both Parents in High Status Jobs",
        f_job_l2 == "High Status Jobs" | m_job_l2 == "High Status Jobs"
        ~ "One Parent in High Status Job",
        f_job_l2 == "Low Status Jobs"  & m_job_l2 == "Low Status Jobs"
        ~ "Neither Parent in High Status Jobs",
        # one Low Status and one NA will be counted as NA
        TRUE ~ NA_character_
      ),
      levels = c(
        "Neither Parent in High Status Jobs",
        "One Parent in High Status Job",
        "Both Parents in High Status Jobs"
      )
    )
  )

# Factor Variables ####

dat$track <- factor(dat$track,
                    levels = c("Science Track", "Liberal Arts Track"))

dat$policy <- factor(dat$policy,
                     levels = c("Untreated", "Treated"))

dat$cls_elite <- factor(dat$cls_elite,
                        levels = c("Regular Class", "Elite Class"))

dat$top_scorer <- factor(dat$top_scorer,
                         levels = c("No", "Yes"))

dat$elite <- factor(dat$elite,
                    levels = c("Regular Students", "Elite Students",
                               "Elite Class Non-Top Scorers",
                               "Regular Class Top Scorers"))

# Teacher gender variables
dat <- dat %>%
  mutate(
    across(
      .cols = ends_with("_tmale"),
      .fns = ~ factor(., levels = c("No", "Yes"))
    )
  )

# Factor and relevel "exam" variable
dat <- dat %>%
  mutate(
    exam = factor(exam,
                  levels = c("hsee",
                             setdiff(unique(exam), c("hsee", "cee")),
                             "cee"))
  )

# Filter Data ####

# Filter out students whose track is NA and who did not have hsee total score

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

# Pivot Wider ####

wdat <- dat %>%
  pivot_wider(
    names_from = exam,
    values_from = c(trk:bio_tmale, tot_trim:zcom_trim),
    names_glue = "{exam}_{.value}",
    names_repair = "check_unique"
  )

# Ad hoc Wrangling ####

# Subset data for RDD analyses
wdat_rd <- wdat %>%
  filter(policy == "Treated") %>%
  # exclude "Regular Class Top Scorers" and "Elite Class Non-Top Scorers"
  # for majority of analyses
  filter(elite %in% c("Elite Students", "Regular Students"))

# Factor variable "elite" in wdat_rd (again)
wdat_rd$elite <- factor(wdat_rd$elite,
                        levels = c("Regular Students", "Elite Students"))

# Create averages of exam performance in high school, using trimmed scores
wdat_rd <- wdat_rd %>%
  mutate(
    avg_hs = rowMeans(select(., g1m1_ztot_trim:g3k2_ztot_trim), na.rm = TRUE),
    avg_g1 = rowMeans(select(., g1m1_ztot_trim:g1f2_ztot_trim), na.rm = TRUE),
    avg_g2 = rowMeans(select(., g2m1_ztot_trim:g2f2_ztot_trim), na.rm = TRUE),
    avg_g3 = rowMeans(select(., g3m1_ztot_trim:g3k2_ztot_trim), na.rm = TRUE)
  )

# Use bandwidth of 0.3
wdat_rd_bw03 <- wdat_rd %>%
  filter(policy == "Treated", hsee_ctot >= -0.3, hsee_ctot <= 0.3)

# Define broom tidiers for rdrobust objects
tidy.rdrobust <- function(x, ...) {
  data.frame(
    term      = rownames(x$coef),
    estimate  = x$coef[, 1],
    std.error = x$se[,   1],
    statistic = x$z[,    1],
    p.value   = x$pv[,   1],
    conf.low  = x$ci[,   1],
    conf.high = x$ci[,   2],
    row.names = NULL
  )
}

glance.rdrobust <- function(x, ...) {
  data.frame(
    nobs.left            = x$N[1],
    nobs.right           = x$N[2],
    nobs.effective.left  = x$N_h[1],
    nobs.effective.right = x$N_h[2],
    cutoff               = x$c,
    order.regression     = x$p,
    kernel               = x$kernel,
    bwselect             = x$bwselect,
    row.names = NULL
  )
}
