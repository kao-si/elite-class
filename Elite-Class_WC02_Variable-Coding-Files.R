
# Each part should be run separately and independently!

library(readxl)
library(tidyverse)

# PART I ####

# Date: 2023-10-25
# Note: The code was written before completing first part of "03_Bind-Data"
#       and is **OBSOLETE**!

# Create the working file for variable coding work performed in November 2023
# Output: "Variable-Coding-Files/住址工作分类文件_2023Nov.sav"

# The working file will be used for coding of three variables:
# (1) rural
# (2) f_job
# (3) m_job

# Load the working file from previous work
wkfile <- read_excel("Variable-Coding-Files/住址工作分类文件_2022Mar.xlsx", col_types = "text", trim_ws = TRUE)

# Relabel `hukou` in c03_demo
c03_demo$hukou <- str_replace_all(c03_demo$hukou,
                            c("A" = "2-非农业", "a" = "2-非农业",
                              "B" = "1-农业", "b" = "1-农业"))

# Retrieve all `demo` files
demos <- mget(ls(pattern = "c\\d{2}_demo"))

# Check and fix duplicate values in XJH
map_df(demos, ~ count(., xjh) %>% filter(n > 1))

count(wkfile, 学籍号) %>% filter(n > 1)

wkfile$学籍号[wkfile$学籍号 == "复"] <- NA
wkfile$学籍号[wkfile$学籍号 == "0"] <- NA

# Modify XJH values in wkfile for cohort 2004 to 2007
wkfile$学籍号[wkfile$年级 %in% 2004:2007] <- paste0("0", wkfile$学籍号[wkfile$年级 %in% 2004:2007])

# Bind the `demo` files
df_bind <- bind_rows(demos, .id = "file") %>%
  select(
    file, xjh, name, jhsch, home_add, hukou_loc, hukou,
    f_name, f_job_text, f_pos_text, m_name, m_job_text, m_pos_text
  )

# Create and code variables in the combined `demo` file
df_bind <- df_bind %>%
  mutate(
    home_add_comb = paste(home_add, hukou_loc, sep = " // "),
    f_job_comb = paste(f_job_text, f_pos_text, sep = " // "),
    m_job_comb = paste(m_job_text, m_pos_text, sep = " // "),
    city = case_when(
      str_detect(hukou, "2-非农业") ~ "1",
      str_detect(hukou, "1-农业") ~ "0",
      TRUE ~ ""
    )
  ) %>%
  mutate(
    f_job = case_when(
      str_detect(f_job_comb, "本村") ~ "1",
      str_detect(f_job_comb, "农民") ~ "1",
      str_detect(f_job_comb, "在家") ~ "2",
      str_detect(f_job_comb, "务农") ~ "1",
      str_detect(f_job_comb, "无业") ~ "2",
      str_detect(f_job_comb, "个体") ~ "3",
      str_detect(f_job_comb, "个人") ~ "3",
      str_detect(f_job_comb, "临时") ~ "2",
      str_detect(f_job_comb, "不固定") ~ "2",
      str_detect(f_job_comb, "下岗") ~ "2",
      str_detect(f_job_comb, "待业") ~ "2",
      str_detect(f_job_comb, "部队") ~ "6",
      str_detect(f_job_comb, "自由") ~ "2",
      str_detect(f_job_comb, "打工") ~ "2",
      TRUE ~ ""
    ),
    m_job = case_when(
      str_detect(m_job_comb, "本村") ~ "1",
      str_detect(m_job_comb, "农民") ~ "1",
      str_detect(m_job_comb, "在家") ~ "2",
      str_detect(m_job_comb, "务农") ~ "1",
      str_detect(m_job_comb, "无业") ~ "2",
      str_detect(m_job_comb, "个体") ~ "3",
      str_detect(m_job_comb, "个人") ~ "3",
      str_detect(m_job_comb, "临时") ~ "2",
      str_detect(m_job_comb, "不固定") ~ "2",
      str_detect(m_job_comb, "下岗") ~ "2",
      str_detect(m_job_comb, "待业") ~ "2",
      str_detect(m_job_comb, "部队") ~ "6",
      str_detect(m_job_comb, "自由") ~ "2",
      str_detect(m_job_comb, "家庭") ~ "2",
      str_detect(m_job_comb, "主妇") ~ "2",
      str_detect(m_job_comb, "打工") ~ "2",
      TRUE ~ ""
    )
  )

# Join variable coding from wkfile
df_bind <- df_bind %>%
  # Duplicated XJH are due to 复读生
  filter(!duplicated(xjh)) %>%
  left_join(
  select(wkfile, 学籍号, 城市, 父亲工作单位分类, 母亲工作单位分类),
  by = c("xjh" = "学籍号"),
  na_matches = "never",
  relationship = "one-to-one"
)

# Fill variable coding from wkfile
df_bind <- df_bind %>% mutate(
  city = case_when(
    city == "" ~ 城市,
    TRUE ~ city
  ),
  f_job = case_when(
    f_job == "" ~ 父亲工作单位分类,
    TRUE ~ f_job
  ),
  m_job = case_when(
    m_job == "" ~ 母亲工作单位分类,
    TRUE ~ m_job
  )
)

# Select columns to export
df_bind_exp <- df_bind %>% select(
  file, xjh, name, jhsch, home_add_comb, city, f_name, f_job_comb, f_job,
  m_name, m_job_comb, m_job
)

# Export the working file into .sav file
haven::write_sav(df_bind_exp, "Variable-Coding-Files/住址工作分类文件_2023Nov.sav")

# PART II ####

# Date: 2023-12-02
# Note: Run "03_Bind-Data" until "Tidy and factor variables" before
#       running code in this part

# Load the completed working file (coding work performed in November 2023)
# and rename variables
varcode <- read_excel("Variable-Coding-Files/住址工作分类文件_完成_2023Dec.xlsx", col_types = "text", trim_ws = TRUE)

colnames(varcode) <- c("cohort", "ssid", "name", "jhsch", "address", "city",
                       "f_name", "f_job_text", "f_job",
                       "m_name", "m_job_text", "m_job")

# (1) Create working file for possible further coding work
#     for "rural", "f_job", and "m_job"
# Output: "Variable-Coding-Files/住址工作分类文件_补充_2023Dec.sav"

# anti_join "demo" (after tidying and factoring variables in "03_Bind-Data")
# with "varcode"
demo_nomatch <- demo %>% anti_join(varcode, by = "ssid", na_matches = "never")

# Filter rows for possible further coding of `rural`, `f_job`, and `m_job`
demo_nomatch <- demo_nomatch %>%
  filter(
    !is.na(hukou) | !is.na(hukou_loc) | !is.na(home_add) | !is.na(jhsch) |
      !is.na(f_job_text) | !is.na(f_pos_text) | !is.na(f_job_add) |
      !is.na(m_job_text) | !is.na(m_pos_text) | !is.na(m_job_add), !is.na(nid)
  )

# Select columns to export
demo_nomatch <- demo_nomatch %>%
  select(
    cohort, ssid, name, nid,
    户口所在地 = hukou_loc,
    家庭住址 = home_add,
    父亲姓名 = f_name,
    父亲工作 = f_job_text,
    母亲姓名 = m_name,
    母亲工作 = m_job_text
  )

# Export the working file into .sav file
haven::write_sav(demo_nomatch, "Variable-Coding-Files/住址工作分类文件_补充_2023Dec.sav")

# (2) Create working file for coding of "jhsch_name"
# Output: "Variable-Coding-Files/初中学校统一名称文件_2023Dec.sav"


# Get unique values of "jhsch" from "demo"
# (after tidying and factoring variables in "03_Bind-Data")
unique_jhsch <- unique(demo$jhsch)

# Create the data frame for output
df_jhsch <- data.frame(
  初中学校 = unique_jhsch,
  标准名称 = "",
  城市 = ""
) %>%
  filter(!is.na(初中学校)) %>%
  arrange(初中学校)

# Export the working file into .sav file
haven::write_sav(df_jhsch, "Variable-Coding-Files/初中学校统一名称文件_2023Dec.sav")

# PART III ####

# Date: 2023-12-09

# Prepare final data frame of variable coding for use in the data set

# Four variables to be added to the data set:
# (1) rural
# (2) f_job
# (3) m_job
# (4) jhsch_name
# (5) jhsch_rural

# Variables (1) - (3)

# Load the completed working files and rename variables

# Unique identifier in "varcode1": "ssid"
varcode1 <- read_excel("Variable-Coding-Files/住址工作分类文件_完成_2023Dec.xlsx", col_types = "text", trim_ws = TRUE)

colnames(varcode1) <- c("cohort", "ssid", "name", "jhsch", "address", "city",
                       "f_name", "f_job_text", "f_job",
                       "m_name", "m_job_text", "m_job")

# Unique identifier in "varcode2": "nid"
varcode2 <- read_excel("Variable-Coding-Files/住址工作分类文件_补充_完成_2023Dec.xlsx", col_types = "text", trim_ws = TRUE)

colnames(varcode2) <- c("cohort", "ssid", "name", "nid",
                        "hukou_loc", "address", "city",
                        "f_name", "f_job_text", "f_job",
                        "m_name", "m_job_text", "m_job")

# Bind the two data frames
varcode <- bind_rows(varcode1, varcode2, .id = "file") %>%
  select(
    file, cohort, ssid, nid, name, address, city,
    f_name, f_job_text, f_job,
    m_name, m_job_text, m_job
  )

# Further process variable coding issues

# Create and process variable `rural`
varcode <- varcode %>%
  # recode `city` to create `rural`
  mutate(
    rural = case_when(
      city == "1" ~ "0",
      city == "0" ~ "1",
      TRUE ~ NA
    )
  ) %>%
  # correct coding of certain residential addresses as identified by the coder
  mutate(
    rural = case_when(
      str_detect(address, "李家窑|夏家庄|后峪|窝瞳|五龙|良庄|安上|珑山|祥和园|竹林小区") ~ "0",
      TRUE ~ rural
    )
  )

# Process variables `f_job` and `m_job`
varcode <- varcode %>%
  mutate(
    f_job = case_when(
      str_detect(f_job_text, "自来水公司") ~ "5",
      str_detect(f_job_text, "邮政局") ~ "5",
      str_detect(f_job_text, "供电所") ~ "5",
      str_detect(f_job_text, "鲁山林场") ~ "5",
      str_detect(f_job_text, "电信局") ~ "5",
      str_detect(f_job_text, "粮食局") ~ "6",
      str_detect(f_job_text, "街道办事处") ~ "6",
      TRUE ~ f_job
    ),
    m_job = case_when(
      str_detect(m_job_text, "自来水公司") ~ "5",
      str_detect(m_job_text, "邮政局") ~ "5",
      str_detect(m_job_text, "供电所") ~ "5",
      str_detect(m_job_text, "鲁山林场") ~ "5",
      str_detect(m_job_text, "电信局") ~ "5",
      str_detect(m_job_text, "粮食局") ~ "6",
      str_detect(m_job_text, "街道办事处") ~ "6",
      TRUE ~ m_job
    )
  )

# Variables (4) - (5)

# Load the completed working file and rename variables

jhschcode <- read_excel("Variable-Coding-Files/初中学校统一名称文件_完成_2023Dec.xlsx", col_types = "text", trim_ws = TRUE)

colnames(jhschcode) <- c("jhsch", "jhsch_name", "jhsch_city")

# Recode `jhsch_city` to create `jhsch_rural`
jhschcode <- jhschcode %>%
mutate(
  jhsch_rural = case_when(
    jhsch_city == "1" ~ "0",
    jhsch_city == "0" ~ "1",
    TRUE ~ NA
  )
)