
# Step 3: Bind Exam Variables with Demographic Variables for All Cohorts
# and Combine Data of All Cohorts into One Data Frame

# Run script for Step 2 before running this script

library(readxl)
library(lubridate)

# Create and Tidy Demographic Variable Files for Each Cohort ####

## Cohort 2003 ====

c03_demo <- c03_base %>%
  select(
    ssid, 姓名, 性别, zx, 民族, 政治面貌, zy, zymc, school, 联系电话, 籍贯,
    父姓名, 父单位, 母姓名, 母单位, 家庭住址, 户口性质, birth, 科类
  ) %>%
  rename(
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

## Cohort 2004 ====

c04_demo <- c04_base %>%
  select(
    ssid, 姓名, 性别, tc, zymc, 毕业学校1, 父姓名, 父单位, 母姓名, 母单位,
    联系电话, 出生日期, 家庭住址
  ) %>%
  rename(
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
    select(c04_gk, ssid, KL, SFZH, YXMC, ZYMC),
    by = "ssid",
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

## Cohort 2005 ====

c05_demo <- c05_base %>%
  select(
    ssid, xm, xb, sfzh, tc, zx, race, appe, jtzz, 父姓名, 父单位, 母姓名,
    母单位, byxx, csrq, lxdh, kl
  ) %>%
  rename(
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

## Cohort 2006 ====

c06_demo <- c06_base %>%
  select(
    ssid, xm, jtzz, lxdh, 父姓名, 父工作, 父职务, 父电话, 母姓名, 母工作,
    母职务, 母电话, sfzh, byxx, zx, xb, 民族, 政治面貌, wl
  ) %>%
  rename(
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

## Cohort 2007 ====

c07_demo <- c07_base %>%
  select(
    ssid, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, zx, mark, 籍贯, 父姓名,
    父工作单位, 父文化程度, 父政治面貌, 父电话, 母姓名, 母工作单位,
    母政治面貌, 母文化程度, 母电话
  ) %>%
  rename(
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

## Cohort 2008 ====

c08_demo <- c08_base %>%
  select(
    ssid, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, tc, 籍贯31, 父姓名1zc,
    父工作单位choõ, 父文化程度choõ, 父政治面貌choõ, 父电话面貌c,
    母姓名面貌c, 母工作单位choõ, 母政治面貌choõ, 母文化程度choõ, 母电话程度c, kl
  ) %>%
  rename(
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

## Cohort 2009 ====

c09_demo <- c09_base %>%
  select(
    ssid, xm, xb, byxx, byxxdh, sfzh, csrq, dszn, 籍贯zn, mz, zzmm, jtzz, hkszd,
    lxdh, 父姓名mzk, "父工作单位x09\u09ba", 父地址单位x, 父电话单位x,
    母姓名yzb, "母工作单位xm9\u09ba", 母地址单位x, 母电话单位x, kl
  ) %>%
  rename(
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

## Cohort 2010 ====

c10_demo <- c10_base %>%
  select(
    ssid, sfzh, xb, csrq, dszn, jg, mz, zzmm, hkszd, kl
  ) %>%
  rename(
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
      c10_gk, ssid, 姓名, 录取院校, lxdh, jtdz, grjl1_jl, jtcy1_xm, jtcy1_gzdw,
      jtcy1_zw, jtcy1_lxdh, jtcy2_xm, jtcy2_gzdw, jtcy2_zw, jtcy2_lxdh
    ),
    by = "ssid",
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

## Cohort 2011 ====

c11_demo <- c11_base %>%
  select(
    ssid, xm, xb, sfzh, mz, zzmm, jg, byxx, tc, lxdh, jtzz,
    父姓名z1言, "父工作单位言\u07b4翬ৄ",
    父电话单位言, 父面貌单位言, 父文化单位言, 母姓名单位言,
    "母工作单位言\u07b4翬ৄ", 母电话单位言, 母面貌单位言, 母文化单位言
  ) %>%
  rename(
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

## Cohort 2012 ====

c12_demo <- c12_base %>%
  select(
    ssid, xm1, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd, jtcy1dh,
    jtcy1gz, zzmm, hkxz, dszn, jdfs, xxmc, tc, kl
  ) %>%
  rename(
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

## Cohort 2013 ====

c13_demo <- c13_base %>%
  select(
    ssid, xm, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy2sf, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, jtcy1sf, zzmm, hkxz, dszn, jdfs, byxx, kl
  ) %>%
  rename(
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

## Cohort 2014 ====

c14_demo <- c14_base %>%
  select(
    ssid, xm, xb, 特长c2, mz, sfzh, csrq, lxdh, jg, hjszd, zz, jtcy2xm,
    jtcy2zzmm, jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, zzmm, hkxz, dszn, jdfs, byxx, kl
  ) %>%
  rename(
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

# Further Process and Tidy Demographic Variable Files ####

# Combine all demographic variable files into a list
demo_list <- mget(ls(pattern = "c\\d{2}_demo"))

# Combine all demographic variable files in the list into a data frame
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

## Select variables to finalize the demographic variable files ====

demo <- demo %>%
select(
  cohort, name, ssid, nid, dob,
  male, rural, onlychd, board, jhsch_name, jhsch_rural,
  han, polsta, spec, spec_rank, spec2, btrack, univ, univmajor,
  f_name, f_job, f_edu, f_polsta, f_nid,
  m_name, m_job, m_edu, m_polsta, m_nid
)

# Split the finalized "demo" data frame by cohort and
# extract individual data frames

demo_list2 <- split(demo, demo$cohort)

for (df_name in names(demo_list2)) {
  dfname <- paste0("c", df_name, "_demo")
  assign(dfname, demo_list2[[df_name]])
}

# Join Exam Variables with Demographic Variables ####

# Function that selects and renames variables in an exam file

# df is the exam file
# prefix is the exam prefix; e.g., prefix = "hsee"
# for first three variables, name argument name if missing; e.g., trk = "trk"
# for other variables, no action needed if missing

srev <- function(df, prefix, trk, cls, cid,
tot = "总成绩", chn = "语文", mat = "数学", eng = "英语",
phy = "物理", che = "化学", bio = "生物",
geo = "地理", his = "历史", pol = "政治",
sci = "理综", lib = "文综", gen = "能力", com = "综合") {

  # full list of variables to be included
  full_vars <- c(trk, cls, cid, tot, chn, mat, eng, phy, che, bio,
  geo, his, pol, sci, lib, gen, com)

  # variables that exist in the current exam file
  select_vars <- intersect(full_vars, colnames(df))

  # variables that do not exist in the current exam file
  miss_vars <- setdiff(full_vars, colnames(df))

  # create the data frame for joining
  df1 <- df %>%
  dplyr::select(ssid, all_of(select_vars))

  # add missing columns with NA values
  for (col in miss_vars) {
    df1[[col]] <- NA
  }

  # reorder the columns
  df1 <- df1 %>%
  dplyr::select(ssid, all_of(full_vars))

  # rename the variables
  var_names <- paste0(prefix, "_", c("trk", "cls", "cid", "tot", "chn", "mat",
  "eng", "phy", "che", "bio", "geo", "his", "pol", "sci", "lib", "gen", "com"))

  colnames(df1) <- c("ssid", var_names)

  # convert all columns to character
  df1 <- purrr::map_df(df1, as.character)

  return(df1)
}

# Function that joins exam variables with demographic variable file in a cohort

jev <- function(cohort) {

  # get the files
  df_demo <- get(paste0("c", cohort, "_demo"))
  df_hsee <- get(paste0("c", cohort, "_hsee"))
  df_g1m1 <- get(paste0("c", cohort, "_g1m1"))
  df_g1f1 <- get(paste0("c", cohort, "_g1f1"))
  df_g1m2 <- get(paste0("c", cohort, "_g1m2"))
  df_g1f2 <- get(paste0("c", cohort, "_g1f2"))
  df_g2m1 <- get(paste0("c", cohort, "_g2m1"))
  df_g2f1 <- get(paste0("c", cohort, "_g2f1"))
  df_g2m2 <- get(paste0("c", cohort, "_g2m2"))
  df_g2f2 <- get(paste0("c", cohort, "_g2f2"))
  df_g3m1 <- get(paste0("c", cohort, "_g3m1"))
  df_g3f1 <- get(paste0("c", cohort, "_g3f1"))
  df_g3k1 <- get(paste0("c", cohort, "_g3k1"))
  df_g3k2 <- get(paste0("c", cohort, "_g3k2"))
  df_cee <- get(paste0("c", cohort, "_cee"))

  # mismatch checks
  n1 <- df_demo %>% dplyr::anti_join(df_hsee) %>% nrow()
  n2 <- df_demo %>% dplyr::anti_join(df_g1m1) %>% nrow()
  n3 <- df_demo %>% dplyr::anti_join(df_g1f1) %>% nrow()
  n4 <- df_demo %>% dplyr::anti_join(df_g1m2) %>% nrow()
  n5 <- df_demo %>% dplyr::anti_join(df_g1f2) %>% nrow()
  n6 <- df_demo %>% dplyr::anti_join(df_g2m1) %>% nrow()
  n7 <- df_demo %>% dplyr::anti_join(df_g2f1) %>% nrow()
  n8 <- df_demo %>% dplyr::anti_join(df_g2m2) %>% nrow()
  n9 <- df_demo %>% dplyr::anti_join(df_g2f2) %>% nrow()
  n10 <- df_demo %>% dplyr::anti_join(df_g3m1) %>% nrow()
  n11 <- df_demo %>% dplyr::anti_join(df_g3f1) %>% nrow()
  n12 <- df_demo %>% dplyr::anti_join(df_g3k1) %>% nrow()
  n13 <- df_demo %>% dplyr::anti_join(df_g3k2) %>% nrow()
  n14 <- df_demo %>% dplyr::anti_join(df_cee) %>% nrow()

  cat("Joining the demo file to each exam file, numbers of mismatches are:\n",
  "hsee:", n1, "\n",
  "g1m1:", n2, "\n",
  "g1f1:", n3, "\n",
  "g1m2:", n4, "\n",
  "g1f2:", n5, "\n",
  "g2m1:", n6, "\n",
  "g2f1:", n7, "\n",
  "g2m2:", n8, "\n",
  "g2f2:", n9, "\n",
  "g3m1:", n10, "\n",
  "g3f1:", n11, "\n",
  "g3k1:", n12, "\n",
  "g3k2:", n13, "\n",
  "cee:", n14, "\n")

  # perform the join
  df <- df_demo %>%
  dplyr::full_join(
    df_hsee, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g1m1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g1f1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g1m2, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g1f2, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g2m1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g2f1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g2m2, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g2f2, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g3m1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g3f1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g3k1, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_g3k2, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  dplyr::full_join(
    df_cee, by = "ssid", na_matches = "never", relationship = "one-to-one"
  ) %>%
  # keep rows with valid values of "ssid"
  dplyr::filter(!is.na(ssid))

  # check number of columns in df
  n_col <- ncol(df)

  # check number of rows in df
  n_row <- nrow(df)

  cat("Number of columns in", paste0("c", cohort, " is ", n_col), "\n",
      "Number of rows in", paste0("c", cohort, " is ", n_row), "\n")

  return(df)
}

## Cohort 2003 ====

c2003_hsee <- srev(df = c03_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2003_g1m1 <- srev(df = c03_base, prefix = "g1m1", trk = "trk", cls = "bh2m", cid = "xh2m",
                   tot = "cj11z")

c2003_g1f1 <- srev(df = c03_base, prefix = "g1f1", trk = "trk", cls = "bh2m", cid = "xh2m",
                   tot = "cj11m")

c2003_g1m2 <- srev(df = c03_base, prefix = "g1m2", trk = "trk", cls = "bh2m", cid = "xh2m",
                   tot = "cj12z")

c2003_g1f2 <- srev(df = c03_base, prefix = "g1f2", trk = "trk", cls = "bh2m", cid = "xh2m",
                   tot = "cj12mf")

c2003_g2m1 <- srev(df = c03_base, prefix = "g2m1", trk = "trk", cls = "bh", cid = "xh",
                   tot = "cj21z")

c2003_g2f1 <- srev(df = c03_base, prefix = "g2f1", trk = "trk", cls = "bh", cid = "xh",
                   tot = "cj21m")

c2003_g2m2 <- srev(df = c03_base, prefix = "g2m2", trk = "trk", cls = "bh", cid = "xh",
                   tot = "cj22z")

c2003_g2f2 <- srev(df = c03_base, prefix = "g2f2", trk = "trk", cls = "bh", cid = "xh",
                   tot = "cj22m")

c2003_g3m1 <- srev(df = c03_jc2, prefix = "g3m1", trk = "trk", cls = "bh", cid = "xh")

c2003_g3f1 <- srev(df = c03_jc4, prefix = "g3f1", trk = "trk", cls = "bh", cid = "xh")

c2003_g3k1 <- srev(df = c03_mn1, prefix = "g3k1", trk = "trk", cls = "bh", cid = "xh")

c2003_g3k2 <- srev(df = c03_mn2, prefix = "g3k2", trk = "trk", cls = "bh", cid = "xh")

c2003_cee <- srev(df = c03_gk, prefix = "cee", trk = "trk", cls = "班级", cid = "cid",
                   tot = "gkzf", chn = "yw", mat = "sx", eng = "yy", com = "zh")

# Join exam variables with demographic file
c2003 <- jev("2003")

## Cohort 2004 ====

c2004_hsee <- srev(df = c04_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2004_g1m1 <- srev(df = c04_base, prefix = "g1m1", trk = "trk", cls = "bh0506", cid = "xh0506",
                   tot = "cj11z")

c2004_g1f1 <- srev(df = c04_20050128qm, prefix = "g1f1", trk = "KL", cls = "BH", cid = "XH")

c2004_g1m2 <- srev(df = c04_base, prefix = "g1m2", trk = "trk", cls = "bh0506", cid = "xh0506",
                   tot = "cj12z")

c2004_g1f2 <- srev(df = c04_base, prefix = "g1f2", trk = "trk", cls = "bh0506", cid = "xh0506",
                   tot = "cj12m")

c2004_g2m1 <- srev(df = c04_20051104qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2004_g2f1 <- srev(df = c04_20060115qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2004_g2m2 <- srev(df = c04_20060426qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2004_g2f2 <- srev(df = c04_20060712qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2004_g3m1 <- srev(df = c04_jc2, prefix = "g3m1", trk = "trk", cls = "BH", cid = "XH")

c2004_g3f1 <- srev(df = c04_jc4, prefix = "g3f1", trk = "trk", cls = "BH", cid = "XH")

c2004_g3k1 <- srev(df = c04_mn1, prefix = "g3k1", trk = "trk", cls = "BH", cid = "XH")

c2004_g3k2 <- srev(df = c04_mn2, prefix = "g3k2", trk = "WL", cls = "cls", cid = "cid", 
                   tot = "AA", chn = "YY", mat = "SS", eng = "EE", phy = "WW",
                   che = "HH", bio = "BB", geo = "DD", his = "LL", pol = "ZZ",
                   sci = "LZ", lib = "WZ", gen = "XX")

c2004_cee <- srev(df = c04_gk, prefix = "cee", trk = "KL", cls = "BJ", cid = "cid", 
                  tot = "ZF", chn = "YW", mat = "SX", eng = "YY", gen = "NL",
                  com = "ZH")

# Join exam variables with demographic file
c2004 <- jev("2004")

## Cohort 2005 ====

c2005_hsee <- srev(df = c05_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2005_g1m1 <- srev(df = c05_20051106qz, prefix = "g1m1", trk = "KL", cls = "BH", cid = "XH")

c2005_g1f1 <- srev(df = c05_20060115qm, prefix = "g1f1", trk = "KL", cls = "BH", cid = "XH")

c2005_g1m2 <- srev(df = c05_base, prefix = "g1m2", trk = "kl", cls = "bh", cid = "xh", 
                   tot = "cj12z")

c2005_g1f2 <- srev(df = c05_base, prefix = "g1f2", trk = "kl", cls = "bh", cid = "xh", 
                   tot = "cj12m")

c2005_g2m1 <- srev(df = c05_20061106qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2005_g2f1 <- srev(df = c05_20070206qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2005_g2m2 <- srev(df = c05_20070429qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2005_g2f2 <- srev(df = c05_20070701qm_hnl, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2005_g3m1 <- srev(df = c05_20071110qz_wj, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2005_g3f1 <- srev(df = c05_20080201qm_wj, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2005_g3k1 <- srev(df = c05_20080229mn1_wj_zh, prefix = "g3k1", trk = "KL", cls = "BH", 
                   cid = "XH")

c2005_g3k2 <- srev(df = c05_20080430mn2_wj, prefix = "g3k2", trk = "KL", cls = "BH", cid = "cid")

c2005_cee <- srev(df = c05_gk, prefix = "cee", trk = "trk", cls = "班级", cid = "cid", 
                  tot = "总分", eng = "外语")

# Join exam variables with demographic file
c2005 <- jev("2005")

## Cohort 2006 ====

c2006_hsee <- srev(df = c06_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2006_g1m1 <- srev(df = c06_20061106qz_xb, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2006_g1f1 <- srev(df = c06_20070206qm_qb, prefix = "g1f1", trk = "KL", cls = "BH", cid = "XH")

c2006_g1m2 <- srev(df = c06_20070429qz_qb, prefix = "g1m2", trk = "KL", cls = "BH", cid = "XH")

c2006_g1f2 <- srev(df = c06_20070701qm_qb, prefix = "g1f2", trk = "KL", cls = "BH", cid = "XH")

c2006_g2m1 <- srev(df = c06_20071115qz_qb, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2006_g2f1 <- srev(df = c06_20080126qm_qb, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2006_g2m2 <- srev(df = c06_20080505qz_qbkm, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2006_g2f2 <- srev(df = c06_20080707qm_qb, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2006_g3m1 <- srev(df = c06_20081106jc_before, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2006_g3f1 <- srev(df = c06_20090115qm, prefix = "g3f1", trk = "trk", cls = "BH", cid = "XH")

c2006_g3k1 <- srev(df = c06_20090315mn1, prefix = "g3k1", trk = "trk", cls = "BH", cid = "cid")

c2006_g3k2 <- srev(df = c06_20090429mn2, prefix = "g3k2", trk = "trk", cls = "BH", cid = "XH")

c2006_cee <- srev(df = c06_gk, prefix = "cee", trk = "trk", cls = "班号", cid = "cid", 
                  tot = "gkzf", chn = "gkkm1", mat = "gkkm2", eng = "gkkm3",
                  gen = "jbnl", com = "zhkm")

# Join exam variables with demographic file
c2006 <- jev("2006")

## Cohort 2007 ====

c2007_hsee <- srev(df = c07_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid", 
                   tot = "rxcj")

c2007_g1m1 <- srev(df = c07_20071118qz, prefix = "g1m1", trk = "KL", cls = "BH", cid = "XH")

c2007_g1f1 <- srev(df = c07_20080125qm_qbkm, prefix = "g1f1", trk = "KL", cls = "BH", cid = "XH")

c2007_g1m2 <- srev(df = c07_20080506qz, prefix = "g1m2", trk = "KL", cls = "BH", cid = "XH")

c2007_g1f2 <- srev(df = c07_20080706qm, prefix = "g1f2", trk = "KL", cls = "BH", cid = "XH")

c2007_g2m1 <- srev(df = c07_20081103qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2007_g2f1 <- srev(df = c07_20090118qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2007_g2m2 <- srev(df = c07_20090415qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2007_g2f2 <- srev(df = c07_20090708qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2007_g3m1 <- srev(df = c07_20091111qz, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2007_g3f1 <- srev(df = c07_20100203qm_wj, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

# Create an empty df (with a column named "ssid") for c2007_g3k1
df_empty <- data.frame(ssid = NA)

c2007_g3k1 <- srev(df = df_empty, prefix = "g3k1", trk = "trk", cls = "cls", cid = "cid")

c2007_g3k2 <- srev(df = c07_20100430mn2_wj, prefix = "g3k2", trk = "KL", cls = "BH", cid = "cid")

c2007_cee <- srev(df = c07_gk, prefix = "cee", trk = "trk", cls = "bh", cid = "cid",
                  tot = "zf", chn = "yw", mat = "sx", eng = "yy", gen = "nl", 
                  com = "zonghe")

# Join exam variables with demographic file
c2007 <- jev("2007")

## Cohort 2008 ====

c2008_hsee <- srev(df = c08_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid", 
                   tot = "rxcj")

c2008_g1m1 <- srev(df = c08_20081106qz, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2008_g1f1 <- srev(df = c08_20090116qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2008_g1m2 <- srev(df = c08_20090416qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2008_g1f2 <- srev(df = c08_20090709qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2008_g2m1 <- srev(df = c08_20091111qz_zb_dy, prefix = "g2m1", trk = "KLKL", cls = "BH", cid = "XH")

c2008_g2f1 <- srev(df = c08_20100201qm_dy_2, prefix = "g2f1", trk = "KL1", cls = "BH", cid = "XH")

c2008_g2m2 <- srev(df = c08_20100429qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2008_g2f2 <- srev(df = c08_20100715qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2008_g3m1 <- srev(df = c08_20101112qz_yj, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2008_g3f1 <- srev(df = c08_20110122jc_wj, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2008_g3k1 <- srev(df = c08_20110318mn1_wj, prefix = "g3k1", trk = "KL", cls = "BH", cid = "cid")

c2008_g3k2 <- srev(df = c08_20110427mn2_wj, prefix = "g3k2", trk = "KL", cls = "BH", cid = "XH")

c2008_cee <- srev(df = c08_20110318mn1_wj, prefix = "cee", trk = "KL", cls = "BH", cid = "cid")

# Join exam variables with demographic file
c2008 <- jev("2008")

## Cohort 2009 ====

c2009_hsee <- srev(df = c09_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid", 
                   tot = "rxcj")

c2009_g1m1 <- srev(df = c09_20091111qz_2p, prefix = "g1m1", trk = "KL", cls = "BH", cid = "XH")

c2009_g1f1 <- srev(df = c09_20100201qm_2p, prefix = "g1f1", trk = "KL", cls = "BH", cid = "XH")

c2009_g1m2 <- srev(df = c09_20100430qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2009_g1f2 <- srev(df = c09_20100715qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2009_g2m1 <- srev(df = c09_20101112qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2009_g2f1 <- srev(df = c09_20110122qm_xzb, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2009_g2m2 <- srev(df = c09_20110428qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2009_g2f2 <- srev(df = c09_20110708qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2009_g3m1 <- srev(df = c09_20111106qz_wj, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2009_g3f1 <- srev(df = c09_20120113qm_wj_bzh, prefix = "g3f1", trk = "KL", cls = "BH", cid = "cid")

c2009_g3k1 <- srev(df = c09_20120303mn1_wj, prefix = "g3k1", trk = "KL", cls = "BH", cid = "cid")

c2009_g3k2 <- srev(df = c09_20120427mn2_wj, prefix = "g3k2", trk = "KL", cls = "BH", cid = "cid")

c2009_cee <- srev(df = c09_20120303mn1_wj, prefix = "cee", trk = "KL", cls = "BH", cid = "cid")

# Join exam variables with demographic file
c2009 <- jev("2009")

## Cohort 2010 ====

c2010_hsee <- srev(df = c10_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid", 
                   tot = "rxcj")

c2010_g1m1 <- srev(df = c10_20101110yk, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2010_g1f1 <- srev(df = c10_20110122qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2010_g1m2 <- srev(df = c10_20110428qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2010_g1f2 <- srev(df = c10_20110708qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2010_g2m1 <- srev(df = c10_20111104qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2010_g2f1 <- srev(df = c10_20120113qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2010_g2m2 <- srev(df = c10_20120419qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2010_g2f2 <- srev(df = c10_20120706qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2010_g3m1 <- srev(df = c10_20121108jc_wj, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2010_g3f1 <- srev(df = c10_20130125qm_wj, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2010_g3k1 <- srev(df = c10_20130307mn1_wj, prefix = "g3k1", trk = "KL", cls = "BH", cid = "cid")

c2010_g3k2 <- srev(df = c10_20130426mn2_wj, prefix = "g3k2", trk = "KL", cls = "BH", cid = "cid")

c2010_cee <- srev(df = c10_gk, prefix = "cee", trk = "trk", cls = "班", cid = "cid",
                  tot = "总分")

# Join exam variables with demographic file
c2010 <- jev("2010")

## Cohort 2011 ====

c2011_hsee <- srev(df = c11_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2011_g1m1 <- srev(df = c11_20111110qz, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2011_g1f1 <- srev(df = c11_20120113qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2011_g1m2 <- srev(df = c11_20120420qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2011_g1f2 <- srev(df = c11_20120701qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2011_g2m1 <- srev(df = c11_20121114qz_xzb, prefix = "g2m1", trk = "KLKL", cls = "BHJ", cid = "XHJ")

c2011_g2f1 <- srev(df = c11_20130130qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2011_g2m2 <- srev(df = c11_20130507qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2011_g2f2 <- srev(df = c11_20130707qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2011_g3m1 <- srev(df = c11_20131107qz, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2011_g3f1 <- srev(df = c11_20140120qm, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2011_g3k1 <- srev(df = c11_20140306mn1, prefix = "g3k1", trk = "KL", cls = "BH", cid = "cid")

c2011_g3k2 <- srev(df = c11_20140422mn2, prefix = "g3k2", trk = "KL", cls = "BH", cid = "XH")

c2011_cee <- srev(df = c11_gk, prefix = "cee", trk = "kldh", cls = "cls", cid = "cid",
                  tot = "总分")

# Join exam variables with demographic file
c2011 <- jev("2011")

## Cohort 2012 ====

c2012_hsee <- srev(df = c12_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                  tot = "rxcj")

c2012_g1m1 <- srev(df = c12_20121115qz, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2012_g1f1 <- srev(df = c12_20130130qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2012_g1m2 <- srev(df = c12_20130504qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2012_g1f2 <- srev(df = c12_20130708qm, prefix = "g1f2", trk = "KL", cls = "BH", cid = "XH")

c2012_g2m1 <- srev(df = c12_20131112qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2012_g2f1 <- srev(df = c12_20140115qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2012_g2m2 <- srev(df = c12_20140422qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2012_g2f2 <- srev(df = c12_20140708qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2012_g3m1 <- srev(df = c12_20141112qz, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2012_g3f1 <- srev(df = c12_20150202qm_b, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2012_g3k1 <- srev(df = c12_20150315mn1, prefix = "g3k1", trk = "KL", cls = "BH", cid = "cid")

c2012_g3k2 <- srev(df = c12_20150506mn2, prefix = "g3k2", trk = "KL", cls = "BH", cid = "XH")

c2012_cee <- srev(df = c12_gk, prefix = "cee", trk = "trk", cls = "bh", cid = "cid",
                  tot = "zf")

# Join exam variables with demographic file
c2012 <- jev("2012")

## Cohort 2013 ====

c2013_hsee <- srev(df = c13_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2013_g1m1 <- srev(df = c13_20131105qz, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2013_g1f1 <- srev(df = c13_20140120qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2013_g1m2 <- srev(df = c13_20140420qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2013_g1f2 <- srev(df = c13_20140703qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2013_g2m1 <- srev(df = c13_20141117qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2013_g2f1 <- srev(df = c13_20150201qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2013_g2m2 <- srev(df = c13_20150508qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2013_g2f2 <- srev(df = c13_20150707qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2013_g3m1 <- srev(df = c13_20151102qz, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2013_g3f1 <- srev(df = c13_20160128qm, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2013_g3k1 <- srev(df = c13_20160306mn1, prefix = "g3k1", trk = "KL", cls = "BH", cid = "XH")

c2013_g3k2 <- srev(df = c13_20160504mn2, prefix = "g3k2", trk = "KL", cls = "BH", cid = "XH")

c2013_cee <- srev(df = c13_gk, prefix = "cee", trk = "kl", cls = "bh", cid = "xh",
                  tot = "总分")

# Join exam variables with demographic file
c2013 <- jev("2013")

## Cohort 2014 ====

c2014_hsee <- srev(df = c14_base, prefix = "hsee", trk = "trk", cls = "cls", cid = "cid",
                   tot = "rxcj")

c2014_g1m1 <- srev(df = c14_20141121qz, prefix = "g1m1", trk = "trk", cls = "BH", cid = "XH")

c2014_g1f1 <- srev(df = c14_20150205qm, prefix = "g1f1", trk = "trk", cls = "BH", cid = "XH")

c2014_g1m2 <- srev(df = c14_20150427qz, prefix = "g1m2", trk = "trk", cls = "BH", cid = "XH")

c2014_g1f2 <- srev(df = c14_20150705qm, prefix = "g1f2", trk = "trk", cls = "BH", cid = "XH")

c2014_g2m1 <- srev(df = c14_20151112qz, prefix = "g2m1", trk = "KL", cls = "BH", cid = "XH")

c2014_g2f1 <- srev(df = c14_20160127qm, prefix = "g2f1", trk = "KL", cls = "BH", cid = "XH")

c2014_g2m2 <- srev(df = c14_20160510qz, prefix = "g2m2", trk = "KL", cls = "BH", cid = "XH")

c2014_g2f2 <- srev(df = c14_20160607qm, prefix = "g2f2", trk = "KL", cls = "BH", cid = "XH")

c2014_g3m1 <- srev(df = c14_20161116qz, prefix = "g3m1", trk = "KL", cls = "BH", cid = "XH")

c2014_g3f1 <- srev(df = c14_20170114qm, prefix = "g3f1", trk = "KL", cls = "BH", cid = "XH")

c2014_g3k1 <- srev(df = c14_20170305mn1, prefix = "g3k1", trk = "KL", cls = "BH", cid = "XH")

c2014_g3k2 <- srev(df = c14_20170505mn2, prefix = "g3k2", trk = "KL", cls = "BH", cid = "XH")

c2014_cee <- srev(df = c14_gk, prefix = "cee", trk = "文理科", cls = "班级", cid = "cid",
                  tot = "总分")

# Join exam variables with demographic file
c2014 <- jev("2014")

# Combine Data of All Cohorts into One Data Frame ####
dat <- mget(ls(pattern = "^c(20[0-9][0-9])$")) %>% bind_rows(.id = "cohortid")

# Tidy cohort columns
dat <- dat %>%
  mutate(
    cohort = substr(cohortid, 2, 5),
    cohortid = NULL
  )

# Combine "cohort" and "ssid" to form another id column
dat$cssid <- paste(dat$cohort, dat$ssid, sep = "_")

# Reorder columns
dat <- dat %>%
  select(cohort, ssid, cssid, everything())

# Save data to .rds ####

write_rds(dat, "Data.rds")
