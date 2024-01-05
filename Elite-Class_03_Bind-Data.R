
library(readxl)
library(tidyverse)
library(lubridate)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

c03_base <- read_excel("Raw-Data/2003/ZBYZ2003_Demographics&Grades.XLS", col_types = "text", trim_ws = TRUE)
c04_base <- read_excel("Raw-Data/2004/ZBYZ2004_Demographics&Grades.XLS", col_types = "text", trim_ws = TRUE)
c05_base <- read_excel("Raw-Data/2005/ZBYZ2005_Demographics&Grades.XLS", col_types = "text", trim_ws = TRUE)
c06_base <- read_excel("Raw-Data/2006/ZBYZ2006_Demographics&Grades.XLS", col_types = "text", trim_ws = TRUE)
c07_base <- read_excel("Raw-Data/2007/ZBYZ2007_Demographics&Grades.XLS", col_types = "text", trim_ws = TRUE)
c08_base <- read_excel("Raw-Data/2008/ZBYZ2008_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c09_base <- read_excel("Raw-Data/2009/ZBYZ2009_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c10_base <- read_excel("Raw-Data/2010/ZBYZ2010_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c11_base <- read_excel("Raw-Data/2011/ZBYZ2011_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c12_base <- read_excel("Raw-Data/2012/ZBYZ2012_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c13_base <- read_excel("Raw-Data/2013/ZBYZ2013_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c14_base <- read_excel("Raw-Data/2014/ZBYZ2014_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)
c04_gk <- read_excel("Raw-Data/2004/G2004/高考成绩.xlsx", col_types = "text", trim_ws = TRUE)
c06_base3 <- read_excel("Raw-Data/2006/ZBYZ2006_Demographics&Grades_3.xlsx", col_types = "text", trim_ws = TRUE)
# See 01 File for correction of XJH values in c10_gk
c10_gk <- read_excel("Raw-Data/2010/2010级高考成绩.xls", sheet = "Sheet5", col_types = "text", trim_ws = TRUE)

# Three pairs of students share identical `zcxjh` in c10_gk
c10_gk %>%
  filter(
    duplicated(zcxjh) | duplicated(zcxjh, fromLast = TRUE), !is.na(zcxjh)
  ) %>%
  # Extract XJH, name, and father's name
  select(zcxjh, 姓名, jtcy1_xm)

# zcxjh               姓名   jtcy1_xm
# <chr>               <chr>  <chr>
# 1 2010370301000130966 孙康   孙凤林
# 2 2010370301000130966 常嘉琪 常建交
# 3 2010370301000130112 王晞   王忠
# 4 2010370301000130112 苏天宇 苏同伟
# 5 2010370301000130853 王文烨 王维刚
# 6 2010370301000130853 刘阳   刘绪枝

# Extract these students' XJH from c10_base
c10_base %>%
  filter(
    str_detect(xm, "孙康|常嘉琪|王晞|苏天宇|王文烨|刘阳"),
    str_detect(父姓名mzk, "孙凤林|常建交|王忠|苏同伟|王维刚|刘绪枝")
  ) %>%
  select(zcxh, xm, 父姓名mzk)

# zcxh                xm        父姓名mzk
# <chr>               <chr>     <chr>
# 1 2010370301001030112 苏天宇027 苏同伟200
# 2 2010370301000130112 王晞65    王忠52
# 3 2010370301000130583 刘阳56    刘绪枝200
# 4 2010370301000130853 王文烨527 王维刚200
# 5 2010370301000130966 孙康38    孙凤林200
# 6 2010370301000130971 常嘉琪 07 常建交200

# Replace correct XJH value in c10_gk,
# notice that the `zcxjh` column is `character`
c10_gk$zcxjh[c10_gk$姓名 == "苏天宇" & c10_gk$jtcy1_xm == "苏同伟"] <- "2010370301001030112"
c10_gk$zcxjh[c10_gk$姓名 == "刘阳" & c10_gk$jtcy1_xm == "刘绪枝"] <- "2010370301000130583"
c10_gk$zcxjh[c10_gk$姓名 == "常嘉琪" & c10_gk$jtcy1_xm == "常建交"] <- "2010370301000130971"


# Function that replaces duplicate or incorrect values of XJH
# in source files with NAs

# Number of digits of XJH in each cohort (n):
# 2003: 10
# 2004-2007: 12
# 2008-2014: 19

# xjh is the name of XJH variable in df
# n is the number of digits of XJH in a particular cohort
tidyxjh <- function(df, xjh, n) {
  
  # capture the name of df
  df_name <- deparse(substitute(df))
  
  # get the number of XJH values that are being replaced
  m <- df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]],
  fromLast = TRUE) | nchar(df[[xjh]]) != n] %>% length()
  
  df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]],
  fromLast = TRUE) | nchar(df[[xjh]]) != n] <- NA
  
  cat("Number of XJH values being replaced in", df_name, "is", m, "\n")
  
  return(df)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create and Tidy Demographic Files for Each Cohort ####

## Cohort 2003 ====

c03_demo <- c03_base %>%
  select(
    xjh, 姓名, 性别, zx, 民族, 政治面貌, zy, zymc, school, 联系电话, 籍贯,
    父姓名, 父单位, 母姓名, 母单位, 家庭住址, 户口性质, birth, 科类
  ) %>%
  rename(
    ssid = xjh,
    name = 姓名,
    male = 性别,
    board = zx,
    han = 民族,
    polsta = 政治面貌,
    spec = zy,
    spec_rank = zymc,
    jhsch = school,
    tel = 联系电话,
    orig = 籍贯,
    f_name = 父姓名,
    f_job_text = 父单位,
    m_name = 母姓名,
    m_job_text = 母单位,
    home_add = 家庭住址,
    hukou = 户口性质,
    dob2 = birth,
    btrack = 科类
  )

### Tidy variables ----

c03_demo <- c03_demo %>%
  mutate(
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      is.na(male) ~ NA
    ),
    board = case_when(
      str_detect(board, "A") ~ "1",
      is.na(board) ~ "0"
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      str_detect(han, "它") ~ "0",
      is.na(han) ~ NA
    ),
    polsta = case_when(
      str_detect(polsta, "团") ~ "1",
      str_detect(polsta, "它") ~ "0",
      is.na(polsta) ~ NA
    ),
    spec_rank = case_when(
      spec_rank == "0" ~ NA,
      TRUE ~ spec_rank
    ),
    hukou = case_when(
      str_detect(hukou, "A|A`|a|Α|非农|非农业") ~ "1",
      str_detect(hukou, "B|b|农|农业") ~ "0",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    dob = case_when(
      # Convert `dob2` with exact birth date to `dob`
      nchar(dob2) == 6 & str_detect(dob2, "^[0-9]+$") ~ paste0("19", dob2),
      TRUE ~ NA
    ),
    cohort = "2003"
  )

# Further ensure that `ssid` is tidy
c03_demo <- tidyxjh(c03_demo, "ssid", 10)

## Cohort 2004 ====

c04_demo <- c04_base %>%
  select(
    xjh, 姓名, 性别, tc, zymc, 毕业学校1, 父姓名, 父单位, 母姓名, 母单位,
    联系电话, 出生日期, 家庭住址
  ) %>%
  rename(
    ssid = xjh,
    name = 姓名,
    male = 性别,
    spec = tc,
    spec_rank = zymc,
    jhsch = 毕业学校1,
    f_name = 父姓名,
    f_job_text = 父单位,
    m_name = 母姓名,
    m_job_text = 母单位,
    tel = 联系电话,
    dob2 = 出生日期,
    home_add = 家庭住址
  ) %>%
  # Extract `btrack`, `nid`, `univ`, and `univmajor` from c04_gk
  full_join(
    select(c04_gk, HKKH, KL, SFZH, YXMC, ZYMC),
    by = c("ssid" = "HKKH"),
    # Make sure NAs do not match
    na_matches = "never",
    # Make sure the relationship between the matching variables is one-to-one
    relationship = "one-to-one"
  ) %>%
  rename(
    btrack = KL,
    nid = SFZH,
    univ = YXMC,
    univmajor = ZYMC
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c04_demo <- c04_demo %>%
  mutate(
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      is.na(male) ~ NA
    ),
    spec_rank = case_when(
      spec_rank == "0" ~ NA,
      TRUE ~ spec_rank
    ),
    btrack = case_when(
      str_detect(btrack, "理") ~ "1",
      str_detect(btrack, "文") ~ "2",
      str_detect(btrack, "体育") ~ "1",
      TRUE ~ NA
    ),
    cohort = "2004"
  )

# Further ensure that `ssid` is tidy
c04_demo <- tidyxjh(c04_demo, "ssid", 12)

## Cohort 2005 ====

c05_demo <- c05_base %>%
  select(
    zcxh, xm, xb, sfzh, tc, zx, race, appe, jtzz, 父姓名, 父单位, 母姓名,
    母单位, byxx, csrq, lxdh, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    nid = sfzh,
    spec = tc,
    board = zx,
    han = race,
    polsta = appe,
    home_add = jtzz,
    f_name = 父姓名,
    f_job_text = 父单位,
    m_name = 母姓名,
    m_job_text = 母单位,
    jhsch = byxx,
    dob2 = csrq,
    tel = lxdh,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c05_demo <- c05_demo %>%
  mutate(
    ssid = case_when(
      # Keep `ssid` that contain only numeric strings
      str_detect(ssid, "^[0-9]+$") ~ ssid,
      TRUE ~ NA
    ),
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      is.na(male) ~ NA
    ),
    board = case_when(
      str_detect(board, "住") ~ "1",
      is.na(board) ~ "0"
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      str_detect(han, "它") ~ "0",
      is.na(han) ~ NA
    ),
    polsta = case_when(
      str_detect(polsta, "团") ~ "1",
      str_detect(polsta, "它") ~ "0",
      is.na(polsta) ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "理") ~ "1",
      str_detect(btrack, "文") ~ "2",
      TRUE ~ NA
    ),
    cohort = "2005"
  )

# Further ensure that `ssid` is tidy
c05_demo <- tidyxjh(c05_demo, "ssid", 12)

## Cohort 2006 ====

c06_demo <- c06_base %>%
  select(
    zcxh, xm, jtzz, lxdh, 父姓名, 父工作, 父职务, 父电话, 母姓名, 母工作,
    母职务, 母电话, sfzh, byxx, zx, xb, 民族, 政治面貌, wl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    home_add = jtzz,
    tel = lxdh,
    f_name = 父姓名,
    f_job_text = 父工作,
    f_pos_text = 父职务,
    f_tel = 父电话,
    m_name = 母姓名,
    m_job_text = 母工作,
    m_pos_text = 母职务,
    m_tel = 母电话,
    nid = sfzh,
    jhsch = byxx,
    board = zx,
    male = xb,
    han = 民族,
    polsta = 政治面貌,
    btrack = wl
  ) %>%
  # 12 students were found to share a same `nid` with others
  # Convert the `nid` value of these rows to NA
  mutate(
    nid = case_when(
      duplicated(nid) | duplicated(nid, fromLast = TRUE) ~ NA,
      TRUE ~ nid
    )
  ) %>%
  # Extract `spec` from c06_base3
  full_join(
    select(c06_base3, zcxh, tc),
    by = c("ssid" = "zcxh"),
    # Make sure NAs do not match
    na_matches = "never",
    # Make sure the relationship between the matching variables is one-to-one
    relationship = "one-to-one"
  ) %>%
  rename(
    spec = tc
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c06_demo <- c06_demo %>%
  mutate(
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      is.na(male) ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      str_detect(han, "回") ~ "0",
      is.na(han) ~ NA
    ),
    polsta = case_when(
      str_detect(polsta, "团") ~ "1",
      str_detect(polsta, "它") ~ "0",
      is.na(polsta) ~ NA
    ),
    spec = case_when(
      str_detect(spec, "田径") ~ "田径",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    cohort = "2006"
  )

# Further ensure that `ssid` is tidy
c06_demo <- tidyxjh(c06_demo, "ssid", 12)

## Cohort 2007 ====

c07_demo <- c07_base %>%
  select(
    zcxh, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, zx, mark, 籍贯, 父姓名,
    父工作单位, 父文化程度, 父政治面貌, 父电话, 母姓名, 母工作单位,
    母政治面貌, 母文化程度, 母电话
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    han = mz,
    nid = sfzh,
    polsta = zzmm,
    home_add = jtzz,
    tel = lxdh,
    jhsch = byxx,
    board = zx,
    spec = mark,
    orig = 籍贯,
    f_name = 父姓名,
    f_job_text = 父工作单位,
    f_edu = 父文化程度,
    f_polsta = 父政治面貌,
    f_tel = 父电话,
    m_name = 母姓名,
    m_job_text = 母工作单位,
    m_polsta = 母政治面貌,
    m_edu = 母文化程度,
    m_tel = 母电话
  ) %>%
  # 3 students were found to share a same `nid`
  # Convert the `nid` value of these rows to NA
  mutate(
    nid = case_when(
      duplicated(nid) | duplicated(nid, fromLast = TRUE) ~ NA,
      TRUE ~ nid
    )
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c07_demo <- c07_demo %>%
  mutate(
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      is.na(male) ~ NA
    ),
    board = case_when(
      str_detect(board, "住") ~ "1",
      str_detect(board, "否") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    spec = case_when(
      str_detect(spec, "健美") ~ "健美",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "无|群众") ~ "0",
      str_detect(f_polsta, "^党员$|中共党员|党 员") ~ "1",
      str_detect(f_polsta, "九三学社|民革党员") ~ "2",
      TRUE ~ NA
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "无|群众") ~ "0",
      str_detect(m_polsta, "党员") ~ "1",
      TRUE ~ NA
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2007"
  )

# Further ensure that `ssid` is tidy
c07_demo <- tidyxjh(c07_demo, "ssid", 12)

## Cohort 2008 ====

c08_demo <- c08_base %>%
  select(
    zcxh, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, tc, 籍贯31, 父姓名1zc,
    父工作单位choõ, 父文化程度choõ, 父政治面貌choõ, 父电话面貌c,
    母姓名面貌c, 母工作单位choõ, 母政治面貌choõ, 母文化程度choõ, 母电话程度c, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    han = mz,
    nid = sfzh,
    polsta = zzmm,
    home_add = jtzz,
    tel = lxdh,
    jhsch = byxx,
    spec = tc,
    orig = 籍贯31,
    f_name = 父姓名1zc,
    f_job_text = 父工作单位choõ,
    f_edu = 父文化程度choõ,
    f_polsta = 父政治面貌choõ,
    f_tel = 父电话面貌c,
    m_name = 母姓名面貌c,
    m_job_text = 母工作单位choõ,
    m_polsta = 母政治面貌choõ,
    m_edu = 母文化程度choõ,
    m_tel = 母电话程度c,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c08_demo <- c08_demo %>%
  mutate(
    # Remove numeric string and then extract the first 3 characters in `name`
    name = substr(gsub("[0-9]", "", name), 1, 3),
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      is.na(han) ~ NA,
    ),
    spec = case_when(
      str_detect(spec, "健美") ~ "健美",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string and then extract the first 3 characters in `f_name`
    f_name = case_when(
      str_detect(f_name, "^无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", f_name), 1, 3)
    ),
    # Remove numeric string and then extract the first 3 characters in `m_name`
    m_name = case_when(
      str_detect(m_name, "^无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", m_name), 1, 3)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "群众") ~ "0",
      str_detect(f_polsta, "党员") ~ "1",
      str_detect(f_polsta, "九三学社") ~ "2",
      TRUE ~ NA
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "群众|团员") ~ "0",
      str_detect(m_polsta, "党员") ~ "1",
      str_detect(m_polsta, "民进") ~ "2",
      TRUE ~ NA
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2008"
  )

# Further ensure that `ssid` is tidy
c08_demo <- tidyxjh(c08_demo, "ssid", 19)

## Cohort 2009 ====

c09_demo <- c09_base %>%
  select(
    zcxh, xm, xb, byxx, byxxdh, sfzh, csrq, dszn, 籍贯zn, mz, zzmm, jtzz, hkszd,
    lxdh, 父姓名mzk, "父工作单位x09\u09ba", 父地址单位x, 父电话单位x,
    母姓名yzb, "母工作单位xm9\u09ba", 母地址单位x, 母电话单位x, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    jhsch = byxx,
    jhsch_id = byxxdh,
    nid = sfzh,
    dob2 = csrq,
    onlychd = dszn,
    orig = 籍贯zn,
    han = mz,
    polsta = zzmm,
    home_add = jtzz,
    hukou_loc = hkszd,
    tel = lxdh,
    f_name = 父姓名mzk,
    f_job_text = "父工作单位x09\u09ba",
    f_job_add = 父地址单位x,
    f_tel = 父电话单位x,
    m_name = 母姓名yzb,
    m_job_text = "母工作单位xm9\u09ba",
    m_job_add = 母地址单位x,
    m_tel = 母电话单位x,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c09_demo <- c09_demo %>%
  mutate(
    # Remove numeric string, English alphabets, ".", and "-" in `name`
    name = gsub("[0-9]|[A-Za-z]|\\.|-", "", name),
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      TRUE ~ NA
    ),
    polsta = case_when(
      str_detect(polsta, "03") ~ "1",
      str_detect(polsta, "13") ~ "0",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string in `f_name`
    f_name = case_when(
      str_detect(f_name, "^无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ gsub("[0-9]", "", f_name)
    ),
    # Remove numeric string in `m_name`
    m_name = case_when(
      str_detect(m_name, "^无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ gsub("[0-9]", "", m_name)
    ),  
    cohort = "2009"
  )

# Further ensure that `ssid` is tidy
c09_demo <- tidyxjh(c09_demo, "ssid", 19)

## Cohort 2010 ====

c10_demo <- c10_base %>%
  select(
    zcxh, sfzh, xb, csrq, dszn, jg, mz, zzmm, hkszd, kl
  ) %>%
  rename(
    ssid = zcxh,
    nid = sfzh,
    male = xb,
    dob2 = csrq,
    onlychd = dszn,
    orig = jg,
    han = mz,
    polsta = zzmm,
    hukou_loc = hkszd,
    btrack = kl
  ) %>%
  # Extract other variables from c10_gk
  full_join(
    select(
      c10_gk, zcxjh, 姓名, 录取院校, lxdh, jtdz, grjl1_jl, jtcy1_xm, jtcy1_gzdw,
      jtcy1_zw, jtcy1_lxdh, jtcy2_xm, jtcy2_gzdw, jtcy2_zw, jtcy2_lxdh
    ),
    by = c("ssid" = "zcxjh"),
    # Make sure NAs do not match
    na_matches = "never",
    # Make sure the relationship between the matching variables is one-to-one
    relationship = "one-to-one"
  ) %>%
  rename(
    name = 姓名,
    univ = 录取院校,
    tel = lxdh,
    home_add = jtdz,
    jhsch = grjl1_jl,
    f_name = jtcy1_xm,
    f_job_text = jtcy1_gzdw,
    f_pos_text = jtcy1_zw,
    f_tel = jtcy1_lxdh,
    m_name = jtcy2_xm,
    m_job_text = jtcy2_gzdw,
    m_pos_text = jtcy2_zw,
    m_tel = jtcy2_lxdh
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c10_demo <- c10_demo %>%
  mutate(
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "01") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    polsta = case_when(
      str_detect(polsta, "03") ~ "1",
      str_detect(polsta, "13") ~ "0",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    cohort = "2010"
  )

# Further ensure that `ssid` is tidy
c10_demo <- tidyxjh(c10_demo, "ssid", 19)

## Cohort 2011 ====

c11_demo <- c11_base %>%
  select(
    zcxh, xm, xb, sfzh, mz, zzmm, jg, byxx, tc, lxdh, jtzz,
    父姓名z1言, "父工作单位言\u07b4翬ৄ",
    父电话单位言, 父面貌单位言, 父文化单位言, 母姓名单位言,
    "母工作单位言\u07b4翬ৄ", 母电话单位言, 母面貌单位言, 母文化单位言
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    nid = sfzh,
    han = mz,
    polsta = zzmm,
    orig = jg,
    jhsch = byxx,
    spec = tc,
    tel = lxdh,
    home_add = jtzz,
    f_name = 父姓名z1言,
    f_job_text = "父工作单位言\u07b4翬ৄ",
    f_tel = 父电话单位言,
    f_polsta = 父面貌单位言,
    f_edu = 父文化单位言,
    m_name = 母姓名单位言,
    m_job_text = "母工作单位言\u07b4翬ৄ",
    m_tel = 母电话单位言,
    m_polsta = 母面貌单位言,
    m_edu = 母文化单位言
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c11_demo <- c11_demo %>%
  mutate(
    # Remove numeric string in `name`
    name = gsub("[0-9]", "", name),
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    polsta = case_when(
      str_detect(polsta, "团员|党员") ~ "1",
      is.na(polsta) ~ NA,
      TRUE ~ "0"
    ),
    spec = case_when(
      str_detect(spec, "健美") ~ "健美",
      str_detect(spec, "羽毛球") ~ "羽毛球",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string and then extract the first 3 characters in `f_name`
    f_name = case_when(
      str_detect(f_name, "^无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", f_name), 1, 3)
    ),
    # Remove numeric string and then extract the first 3 characters in `m_name`
    m_name = case_when(
      str_detect(m_name, "^无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", m_name), 1, 3)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "群众|^无|团员") ~ "0",
      str_detect(f_polsta, "^党员|中共党员|中国党员|共产党") ~ "1",
      str_detect(f_polsta, "九三|民建|民进|民革") ~ "2",
      TRUE ~ NA
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "群众|^无|团员") ~ "0",
      str_detect(m_polsta, "党员|共产党") ~ "1",
      str_detect(m_polsta, "农工民主党") ~ "2",
      TRUE ~ NA
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中技|中转|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范|师专") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2011"
  )

# Further ensure that `ssid` is tidy
c11_demo <- tidyxjh(c11_demo, "ssid", 19)

## Cohort 2012 ====

c12_demo <- c12_base %>%
  select(
    zcxh, xm1, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd, jtcy1dh,
    jtcy1gz, zzmm, hkxz, dszn, jdfs, xxmc, tc, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm1,
    nid = sfzh,
    male = xb,
    han = mz,
    dob2 = csrq,
    tel = lxdh,
    orig = jg,
    hukou_loc = hjszd,
    home_add = zz,
    m_name = jtcy2xm,
    m_polsta = jtcy2zzmm,
    m_edu = jtcy2whcd,
    m_tel = jtcy2dh,
    m_job_text = jtcy2gz,
    f_name = jtcy1xm,
    f_polsta = jtcy1zzmm,
    f_edu = jtcy1whcd,
    f_tel = jtcy1dh,
    f_job_text = jtcy1gz,
    polsta = zzmm,
    hukou = hkxz,
    onlychd = dszn,
    board = jdfs,
    jhsch = xxmc,
    spec = tc,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c12_demo <- c12_demo %>%
  mutate(
    # Remove numeric string in `name`
    name = gsub("[0-9]", "", name),
    male = case_when(
      str_detect(male, "男") ~ "1",
      str_detect(male, "女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "汉") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    polsta = case_when(
      str_detect(polsta, "^03") ~ "1",
      str_detect(polsta, "^13") ~ "0",
      TRUE ~ NA
    ),
    spec = case_when(
      str_detect(spec, "羽毛球") ~ "羽毛球",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    hukou = case_when(
      str_detect(hukou, "^1") ~ "0",
      str_detect(hukou, "^2") ~ "1",
      TRUE ~ NA
    ),
    onlychd = case_when(
      str_detect(onlychd, "^0") ~ "1",
      str_detect(onlychd, "^1") ~ "0",
      TRUE ~ NA
    ),
    board = case_when(
      str_detect(board, "^1") ~ "0",
      str_detect(board, "^2") ~ "1",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string and then extract the first 3 characters in `f_name`
    f_name = case_when(
      str_detect(f_name, "^无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", f_name), 1, 3)
    ),
    # Remove numeric string and then extract the first 3 characters in `m_name`
    m_name = case_when(
      str_detect(m_name, "^无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", m_name), 1, 3)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "群众") ~ "0",
      str_detect(f_polsta, "共产党") ~ "1",
      is.na(f_polsta) ~ NA,
      TRUE ~ "2"
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "群众|^无") ~ "0",
      str_detect(m_polsta, "共产党") ~ "1",
      is.na(m_polsta) ~ NA,
      TRUE ~ "2"
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中技|中转|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范|师专") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中技|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2012"
  )

# Further ensure that `ssid` is tidy
c12_demo <- tidyxjh(c12_demo, "ssid", 19)

## Cohort 2013 ====

c13_demo <- c13_base %>%
  select(
    zcxh, xm, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy2sf, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, jtcy1sf, zzmm, hkxz, dszn, jdfs, byxx, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    nid = sfzh,
    male = xb,
    han = mz,
    dob2 = csrq,
    tel = lxdh,
    orig = jg,
    hukou_loc = hjszd,
    home_add = zz,
    m_name = jtcy2xm,
    m_polsta = jtcy2zzmm,
    m_edu = jtcy2whcd,
    m_tel = jtcy2dh,
    m_job_text = jtcy2gz,
    m_nid = jtcy2sf,
    f_name = jtcy1xm,
    f_polsta = jtcy1zzmm,
    f_edu = jtcy1whcd,
    f_tel = jtcy1dh,
    f_job_text = jtcy1gz,
    f_nid = jtcy1sf,
    polsta = zzmm,
    hukou = hkxz,
    onlychd = dszn,
    board = jdfs,
    jhsch = byxx,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c13_demo <- c13_demo %>%
  mutate(
    # Remove numeric string in `name`
    name = gsub("[0-9]", "", name),
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "^汉") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    polsta = case_when(
      str_detect(polsta, "^03") ~ "1",
      str_detect(polsta, "^13") ~ "0",
      TRUE ~ NA
    ),
    hukou = case_when(
      str_detect(hukou, "^1") ~ "0",
      str_detect(hukou, "^2") ~ "1",
      TRUE ~ NA
    ),
    onlychd = case_when(
      str_detect(onlychd, "^0") ~ "1",
      str_detect(onlychd, "^1") ~ "0",
      TRUE ~ NA
    ),
    board = case_when(
      str_detect(board, "^1") ~ "0",
      str_detect(board, "^2") ~ "1",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    # or English alphabet
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+|[A-Za-z]", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+|[A-Za-z]", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string and then extract the first 3 characters in `f_name`
    f_name = case_when(
      str_detect(f_name, "^暂无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", f_name), 1, 3)
    ),
    # Remove numeric string and then extract the first 3 characters in `m_name`
    m_name = case_when(
      str_detect(m_name, "^暂无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", m_name), 1, 3)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "群众") ~ "0",
      str_detect(f_polsta, "共产党|中共") ~ "1",
      is.na(f_polsta) ~ NA,
      TRUE ~ "2"
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "群众") ~ "0",
      str_detect(m_polsta, "共产党") ~ "1",
      is.na(m_polsta) ~ NA,
      TRUE ~ "2"
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中技|中转|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范|师专") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中技|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2013"
  )

# Further ensure that `ssid` is tidy
c13_demo <- tidyxjh(c13_demo, "ssid", 19)

## Cohort 2014 ====

c14_demo <- c14_base %>%
  select(
    zcxh, xm, xb, 特长c2, mz, sfzh, csrq, lxdh, jg, hjszd, zz, jtcy2xm,
    jtcy2zzmm, jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, zzmm, hkxz, dszn, jdfs, byxx, kl
  ) %>%
  rename(
    ssid = zcxh,
    name = xm,
    male = xb,
    spec = 特长c2,
    han = mz,
    nid = sfzh,
    dob2 = csrq,
    tel = lxdh,
    orig = jg,
    hukou_loc = hjszd,
    home_add = zz,
    m_name = jtcy2xm,
    m_polsta = jtcy2zzmm,
    m_edu = jtcy2whcd,
    m_tel = jtcy2dh,
    m_job_text = jtcy2gz,
    f_name = jtcy1xm,
    f_polsta = jtcy1zzmm,
    f_edu = jtcy1whcd,
    f_tel = jtcy1dh,
    f_job_text = jtcy1gz,
    polsta = zzmm,
    hukou = hkxz,
    onlychd = dszn,
    board = jdfs,
    jhsch = byxx,
    btrack = kl
  ) %>%
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

### Tidy variables ----

c14_demo <- c14_demo %>%
  mutate(
    # Remove numeric string and then extract the first 3 characters in `name`
    name = substr(gsub("[0-9]", "", name), 1, 3),
    male = case_when(
      str_detect(male, "^男") ~ "1",
      str_detect(male, "^女") ~ "0",
      TRUE ~ NA
    ),
    han = case_when(
      str_detect(han, "^汉") ~ "1",
      is.na(han) ~ NA,
      TRUE ~ "0"
    ),
    spec = case_when(
      str_detect(spec, "A") ~ "Unknown",
      str_detect(spec, "羽毛球") ~ "羽毛球",
      is.na(spec) ~ NA,
      TRUE ~ substr(spec, 1, 2)
    ),
    polsta = case_when(
      str_detect(polsta, "^03") ~ "1",
      str_detect(polsta, "^13") ~ "0",
      TRUE ~ NA
    ),
    hukou = case_when(
      str_detect(hukou, "^1") ~ "0",
      str_detect(hukou, "^2") ~ "1",
      TRUE ~ NA
    ),
    onlychd = case_when(
      str_detect(onlychd, "^0") ~ "1",
      str_detect(onlychd, "^1") ~ "0",
      TRUE ~ NA
    ),
    board = case_when(
      str_detect(board, "^1") ~ "0",
      str_detect(board, "^2") ~ "1",
      TRUE ~ NA
    ),
    btrack = case_when(
      str_detect(btrack, "L") ~ "1",
      str_detect(btrack, "W") ~ "2",
      TRUE ~ NA
    ),
    # Keep string in `jhsch` that appear before the first numeric character
    # or English alphabet
    jhsch = case_when(
      str_detect(jhsch, "^无") ~ NA,
      regexpr("[0-9]+|[A-Za-z]", jhsch) != -1
        ~ substr(jhsch, 1, regexpr("[0-9]+|[A-Za-z]", jhsch) - 1),
      TRUE ~ jhsch
    ),
    # Remove numeric string and then extract the first 3 characters in `f_name`
    f_name = case_when(
      str_detect(f_name, "^无") ~ NA,
      is.na(f_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", f_name), 1, 3)
    ),
    # Remove numeric string and then extract the first 3 characters in `m_name`
    m_name = case_when(
      str_detect(m_name, "^无") ~ NA,
      is.na(m_name) ~ NA,
      TRUE ~ substr(gsub("[0-9]", "", m_name), 1, 3)
    ),
    f_polsta = case_when(
      str_detect(f_polsta, "群众|无党派") ~ "0",
      str_detect(f_polsta, "共产党") ~ "1",
      is.na(f_polsta) ~ NA,
      TRUE ~ "2"
    ),
    m_polsta = case_when(
      str_detect(m_polsta, "群众|无党派") ~ "0",
      str_detect(m_polsta, "共产党") ~ "1",
      is.na(m_polsta) ~ NA,
      TRUE ~ "2"
    ),
    f_edu = case_when(
      str_detect(f_edu, "小学") ~ "1",
      str_detect(f_edu, "初中|中学") ~ "2",
      str_detect(f_edu, "高中|中专|技校|中技|中转|中等专业") ~ "3",
      str_detect(f_edu, "专科|大专|高专|中师|师范|师专") ~ "4",
      str_detect(f_edu, "本科|大学|大本") ~ "5",
      str_detect(f_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    m_edu = case_when(
      str_detect(m_edu, "小学") ~ "1",
      str_detect(m_edu, "初中|中学") ~ "2",
      str_detect(m_edu, "高中|中专|技校|中技|中等专业") ~ "3",
      str_detect(m_edu, "专科|大专|高专|中师|师范") ~ "4",
      str_detect(m_edu, "本科|大学|大本") ~ "5",
      str_detect(m_edu, "研究生|硕士|博士") ~ "6",
      TRUE ~ NA
    ),
    cohort = "2014"
  )

# Further ensure that `ssid` is tidy
c14_demo <- tidyxjh(c14_demo, "ssid", 19)

# Combine Demographic Files and Further Tidying & Processing ####

# Combine all demographic files into a list
demo_list <- mget(ls(pattern = "c\\d{2}_demo"))

# Combine all demographic files in the list into a data frame
demo <- bind_rows(demo_list) %>%
  select(
    cohort, ssid, btrack, name, male, nid, dob, han, orig, hukou, hukou_loc,
    onlychd, polsta, home_add, tel, board, jhsch,
    univ, univmajor, spec, spec_rank,
    f_name, f_job_text, f_pos_text, f_tel, f_edu, f_polsta, f_job_add, f_nid,
    m_name, m_job_text, m_pos_text, m_tel, m_edu, m_polsta, m_job_add, m_nid
  )

## Tidy and factor variables ====

demo$btrack <- factor(demo$btrack,
                 levels = c(1, 2),
                 labels = c("Science Track", "Liberal Arts Track"))

demo$male <- factor(demo$male,
                    levels = c(0, 1),
                    labels = c("Female", "Male"))

# Fix rows with incorrect `nid` and thus incorrect `dob`
# Convert `dob` to Date
# Also fix rows with incorrect `f_nid` or `m_nid`
demo <- demo %>%
  mutate(
    dob = case_when(
      nchar(nid) != 18 ~ NA,
      nchar(dob) != 8 ~ NA,
      TRUE ~ dob
    )
  ) %>%
  mutate(
    dob = ymd(dob)
  ) %>%
  mutate(
    nid = case_when(
      nchar(nid) != 18 ~ NA,
      TRUE ~ nid
    ),
    f_nid = case_when(
      nchar(f_nid) != 18 ~ NA,
      TRUE ~ f_nid
    ),
    m_nid = case_when(
      nchar(m_nid) != 18 ~ NA,
      TRUE ~ m_nid
    )
  )

demo$han <- factor(demo$han,
                    levels = c(0, 1),
                    labels = c("Other Ethnicity", "Han Ethnicity"))

demo$hukou <- factor(demo$hukou,
                   levels = c(0, 1),
                   labels = c("Rural", "Urban"))

demo$onlychd <- factor(demo$onlychd,
                     levels = c(0, 1),
                     labels = c("No", "Yes"))

demo$polsta <- factor(demo$polsta,
                       levels = c(0, 1),
                       labels = c("Not CYL Member", "CYL Member"))

demo$board <- factor(demo$board,
                      levels = c(0, 1),
                      labels = c("No", "Yes"))

demo <- demo %>%
  mutate(
    spec2 = case_when(
      is.na(spec) ~ "0",
      TRUE ~ "1"
    )
  )

demo$spec2 <- factor(demo$spec2,
                     levels = c(0, 1),
                     labels = c("No Specialty", "Specialty Student"))

demo$f_edu <- factor(demo$f_edu,
                  levels = 1:6,
                  labels = c("Primary School",
                            "Junior High School",
                            "Senior High School/Technical School",
                            "Associate Degree",
                            "Undergraduate Degree",
                            "Postgraduate Degree"))

demo$m_edu <- factor(demo$m_edu,
                  levels = 1:6,
                  labels = c("Primary School",
                            "Junior High School",
                            "Senior High School/Technical School",
                            "Associate Degree",
                            "Undergraduate Degree",
                            "Postgraduate Degree"))

demo$f_polsta <- factor(demo$f_polsta,
                     levels = c(0, 1, 2),
                     labels = c("No Party Affiliation",
                                "CCP Member",
                                "Member of Other Parties"))

demo$m_polsta <- factor(demo$m_polsta,
                        levels = c(0, 1, 2),
                        labels = c("No Party Affiliation",
                                   "CCP Member",
                                   "Member of Other Parties"))

## Prepare coding files (Part III in WC02) ====

# Five variables to be added to the data set:
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

## Extract demographic variables from coding files ====

# Extract "rural", "f_job", and "m_job"
demo <- demo %>%
# join coding using "ssid"
left_join(
  select(varcode, ssid, rural, f_job, m_job),
  by = "ssid",
  # Make sure NAs do not match
  na_matches = "never"
) %>%
# join coding using "nid"
left_join(
  select(varcode, nid, rural, f_job, m_job),
  by = "nid",
  # Make sure NAs do not match
  na_matches = "never"
)

# Tidy the columns
demo <- demo %>%
mutate(
  rural = case_when(
    is.na(rural.y) ~ rural.x,
    !is.na(rural.y) ~ rural.y
  ),
  f_job = case_when(
    is.na(f_job.y) ~ f_job.x,
    !is.na(f_job.y) ~ f_job.y
  ),
  m_job = case_when(
    is.na(m_job.y) ~ m_job.x,
    !is.na(m_job.y) ~ m_job.y
  ),
  rural.x = NULL,
  rural.y = NULL,
  f_job.x = NULL,
  f_job.y = NULL,
  m_job.x = NULL,
  m_job.y = NULL
)

# Extract "jhsch_name" and "jhsch_rural"
demo <- demo %>%
left_join(
  select(jhschcode, jhsch, jhsch_name, jhsch_rural),
  by = "jhsch",
  # Make sure NAs do not match
  na_matches = "never"
)

# Factor variables

demo$rural <- factor(demo$rural,
                          levels = c(0, 1),
                          labels = c("No", "Yes"))

demo$f_job <- factor(demo$f_job,
                    levels = 1:6,
                    labels = c("Peasant",
                              "Unstable Occupation",
                              "Self-Employed",
                              "Enterprise Employee",
                              "Public Institution Employee",
                              "Civil Servant/Military Man"))

demo$m_job <- factor(demo$m_job,
                    levels = 1:6,
                    labels = c("Peasant",
                              "Unstable Occupation",
                              "Self-Employed",
                              "Enterprise Employee",
                              "Public Institution Employee",
                              "Civil Servant/Military Man"))

demo$jhsch_rural <- factor(demo$jhsch_rural,
                          levels = c(0, 1),
                          labels = c("No", "Yes"))

## Select variables to finalize the demographic file ====

demo <- demo %>%
select(
  cohort, name, ssid, nid, dob,
  male, rural, onlychd, board, jhsch_name, jhsch_rural,
  han, polsta, spec, spec_rank, spec2, btrack, univ, univmajor,
  f_name, f_job, f_edu, f_polsta, f_nid,
  m_name, m_job, m_edu, m_polsta, m_nid
)