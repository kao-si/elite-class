

library(readxl)
library(tidyverse)

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

# Replace correct XJH value in c10_gk, notice that the `zcxjh` column is `character`
c10_gk$zcxjh[c10_gk$姓名 == "苏天宇" & c10_gk$jtcy1_xm == "苏同伟"] <- "2010370301001030112"
c10_gk$zcxjh[c10_gk$姓名 == "刘阳" & c10_gk$jtcy1_xm == "刘绪枝"] <- "2010370301000130583"
c10_gk$zcxjh[c10_gk$姓名 == "常嘉琪" & c10_gk$jtcy1_xm == "常建交"] <- "2010370301000130971"


# Create and Tidy Demographic Files ####


## Cohort 2003 ====

c03_demo <- c03_base %>% 
  select(
    xjh, 姓名, 性别, zx, 民族, 政治面貌, zy, zymc, school, 
    联系电话, 籍贯, 父姓名, 父单位, 母姓名, 母单位, 家庭住址, 户口性质, birth
  ) %>% 
  rename(
    name = 姓名,
    gender = 性别,
    bd = zx,
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
    dob2 = birth
  )

## Cohort 2004 ====

c04_demo <- c04_base %>% 
  select(
    xjh, 姓名, 性别, tc, zymc, 毕业学校1, 父姓名, 父单位, 母姓名, 母单位, 
    联系电话, 出生日期, 家庭住址
  ) %>% 
  rename(
    name = 姓名,
    gender = 性别,
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
  # Extract `nid`, `univ`,`univmajor` from c04_gk
  full_join(
    select(c04_gk, HKKH, SFZH, YXMC, ZYMC),
    by = c("xjh" = "HKKH"),
    # Make sure NAs do not match
    na_matches = "never",
    # Make sure the relationship between the matching variables is one-to-one
    relationship = "one-to-one"
  ) %>% 
  rename(
    nid = SFZH,
    univ = YXMC,
    univmajor = ZYMC
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2005 ====

c05_demo <- c05_base %>% 
  select(
    zcxh, xm, xb, sfzh, tc, zx, race, appe, jtzz, 父姓名, 父单位, 母姓名,
    母单位, byxx, csrq, lxdh
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
    nid = sfzh,
    spec = tc,
    bd = zx,
    han = race,
    polsta = appe,
    home_add = jtzz,
    f_name = 父姓名,
    f_job_text = 父单位,
    m_name = 母姓名,
    m_job_text = 母单位,
    jhsch = byxx,
    dob2 = csrq,
    tel = lxdh
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2006 ====

c06_demo <- c06_base %>% 
  select(
    zcxh, xm, jtzz, lxdh, 父姓名, 父工作, 父职务, 父电话, 母姓名, 母工作, 
    母职务, 母电话, sfzh, byxx, zx, xb, 民族, 政治面貌
  ) %>% 
  rename(
    xjh = zcxh,
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
    bd = zx,
    gender = xb,
    han = 民族,
    polsta = 政治面貌
  ) %>% 
  # Extract `spec` from c06_base3
  full_join(
    select(c06_base3, zcxh, tc),
    by = c("xjh" = "zcxh"),
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

## Cohort 2007 ====

c07_demo <- c07_base %>% 
  select(
    zcxh, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, zx, mark, 籍贯, 父姓名,
    父工作单位, 父文化程度, 父政治面貌, 父电话, 母姓名, 母工作单位, 
    母政治面貌, 母文化程度, 母电话
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
    han = mz,
    nid = sfzh,
    polsta = zzmm,
    home_add = jtzz,
    tel = lxdh,
    jhsch = byxx,
    bd = zx,
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
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2008 ====

c08_demo <- c08_base %>% 
  select(
    zcxh, xm, xb, mz, sfzh, zzmm, jtzz, lxdh, byxx, tc, 籍贯31, 父姓名1zc,
    父工作单位choõ, 父文化程度choõ, 父政治面貌choõ, 父电话面貌c,
    母姓名面貌c, 母工作单位choõ, 母政治面貌choõ, 母文化程度choõ, 母电话程度c
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
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
    m_tel = 母电话程度c
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2009 ====

c09_demo <- c09_base %>% 
  select(
    zcxh, xm, xb, byxx, byxxdh, sfzh, csrq, dszn, 籍贯zn, mz, zzmm, jtzz, hkszd,
    lxdh, 父姓名mzk, "父工作单位x09\u09ba", 父地址单位x, 父电话单位x,            
    母姓名yzb, "母工作单位xm9\u09ba", 母地址单位x, 母电话单位x
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
    jhsch = byxx,
    jhsch_id = byxxdh,
    nid = sfzh,
    dob2 = csrq,
    sib = dszn, # 独生子女
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
    m_tel = 母电话单位x
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2010 ====

c10_demo <- c10_base %>% 
  select(
    zcxh, sfzh, xb, csrq, dszn, jg, mz, zzmm, hkszd 
  ) %>% 
  rename(
    xjh = zcxh,
    nid = sfzh,
    gender = xb,
    dob2 = csrq,
    sib = dszn,
    orig = jg,
    han = mz,
    polsta = zzmm,
    hukou_loc = hkszd
  ) %>% 
  # Extract other variables from c10_gk
  full_join(
    select(
      c10_gk, zcxjh, 姓名, 录取院校, lxdh, jtdz, grjl1_jl, jtcy1_xm, jtcy1_gzdw, 
      jtcy1_zw, jtcy1_lxdh, jtcy2_xm, jtcy2_gzdw, jtcy2_zw, jtcy2_lxdh
    ),
    by = c("xjh" = "zcxjh"),
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
  
## Cohort 2011 ====

c11_demo <- c11_base %>% 
  select(
    zcxh, xm, xb, sfzh, mz, zzmm, jg, byxx, tc, lxdh, jtzz,
    父姓名z1言, "父工作单位言\u07b4翬ৄ",
    父电话单位言, 父面貌单位言, 父文化单位言, 母姓名单位言,        
    "母工作单位言\u07b4翬ৄ", 母电话单位言, 母面貌单位言, 母文化单位言
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
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

## Cohort 2012 ====

c12_demo <- c12_base %>% 
  select(
    zcxh, xm1, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd, jtcy1dh, 
    jtcy1gz, zzmm, hkxz, dszn, jdfs, xxmc, tc
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm1,
    nid = sfzh,
    gender = xb,
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
    sib = dszn,
    bd = jdfs,
    jhsch = xxmc,
    spec = tc
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2013 ====

c13_demo <- c13_base %>% 
  select(
    zcxh, xm, sfzh, xb, mz, csrq, lxdh, jg, hjszd, zz, jtcy2xm, jtcy2zzmm,
    jtcy2whcd, jtcy2dh, jtcy2gz, jtcy2sf, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, jtcy1sf, zzmm, hkxz, dszn, jdfs, byxx 
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    nid = sfzh,
    gender = xb,
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
    sib = dszn,
    bd = jdfs,
    jhsch = byxx
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )

## Cohort 2014 ====

c14_demo <- c14_base %>% 
  select(
    zcxh, xm, xb, 特长c2, mz, sfzh, csrq, lxdh, jg, hjszd, zz, jtcy2xm,
    jtcy2zzmm, jtcy2whcd, jtcy2dh, jtcy2gz, jtcy1xm, jtcy1zzmm, jtcy1whcd,
    jtcy1dh, jtcy1gz, zzmm, hkxz, dszn, jdfs, byxx
  ) %>% 
  rename(
    xjh = zcxh,
    name = xm,
    gender = xb,
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
    sib = dszn,
    bd = jdfs,
    jhsch = byxx
  ) %>% 
  # Extract `dob` from `nid`
  mutate(
    dob = substr(nid, 7, 14)
  )























