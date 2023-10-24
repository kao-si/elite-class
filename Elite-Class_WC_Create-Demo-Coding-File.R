library(readxl)
library(tidyverse)

# Load the existing working file
wkfile <- read_excel("住址工作分类文件_2022Mar.xlsx", col_types = "text", trim_ws = TRUE)

# Relabel `hukou` in c03_demo
c03_demo$hukou <- str_replace_all(c03_demo$hukou, 
                            c("A" = "2-非农业", "a" = "2-非农业", 
                              "B" = "1-农业", "b" = "1-农业"))

# Retrieve all `demo` files
demos <- mget(ls(pattern = "c\\d{2}_demo"))

# Check and fix on duplicate values
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

# Create variables
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

# Join coding variables from wkfile
# Duplicated XJH are due to 复读生
df_bind <- df_bind %>% filter(!duplicated(xjh)) %>% left_join(
  select(wkfile, 学籍号, 城市, 父亲工作单位分类, 母亲工作单位分类),
  by = c("xjh" = "学籍号"),
  na_matches = "never",
  relationship = "one-to-one"
)

# Fill codings
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

haven::write_sav(df_bind_exp, "住址工作分类文件_2023Nov.sav")

