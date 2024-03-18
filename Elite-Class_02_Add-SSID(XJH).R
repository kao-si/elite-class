
library(tidyverse)

# Load Raw Data ####

# Load raw data created from "Elite-Class_01_Import-Raw-Data.R"
load("Raw-Data.RData")

# Functions ####

# Function that facilitates the creation of unique identifier

# The unique identifier is named "cls_name" and is made by combining a student's
# class number (bh) and name (xm)
cui <- function(df, bh, xm, uiv = "cls_name") {
  
  df[[uiv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  
  return(df)
}

# Function that facilitates the creation of unique identifier
# for base files with untidy names

cui2 <- function(df, bh, xm, uiv = "cls_name") {
  
  # remove numeric string and then extract the first 3 characters in name
  df[[xm]] <- substr(gsub("[0-9]", "", df[[xm]]), 1, 3)
  
  df[[uiv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  
  return(df)
}

# Function that replaces duplicate or incorrect values of XJH
# with NAs and rename XJH column as "ssid" in source files

# xjh is the name of XJH variable in df
tidyxjh <- function(df, xjh) {
  
  # capture the name of df
  df_name <- deparse(substitute(df))
  
  # number of digits of XJH in each cohort:
  # 2003: 10
  # 2004-2007: 12
  # 2008-2014: 19
  nchar_xjh <- c(10, 12, 19)
  
  # get the number of XJH values that are being replaced
  m <- df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]],
    fromLast = TRUE) | !nchar(df[[xjh]]) %in% nchar_xjh] %>% length()
  
  # replace XJH values with NAs
  df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]],
    fromLast = TRUE) | !nchar(df[[xjh]]) %in% nchar_xjh] <- NA
  
  # rename XJH column to "ssid"
  colnames(df)[colnames(df) == xjh] <- "ssid"
  
  cat("Number of XJH values being replaced in", df_name, "is", m, "\n")
  
  return(df)
}

# Function that performs checks on potential errors in XJH
# across the source files

mis <- function(df) {
  
  # create the output vector
  mismatch <- numeric(length = ncol(df) - 2)
  
  for (i in 3:ncol(df)) {
    
    # get the number of mismatches between XJH of the first source file
    # and XJH of the other source files
    mismatch[[i - 2]] <- which(df[[2]] != df[[i]]) %>% length()
  }
  
  return(mismatch)
}

# Function that corrects XJH values in source files with a
# relatively large number of mismatched XJH values

# df is the file with a relatively large number of mismatched XJH values
# xjh is the name of XJH variable in df
# check is the check file
# dfxjh is the name of XJH variable from df in check
# fxjh is the name of the first XJH variable in check
repmis <- function(df, xjh = "ssid", check, dfxjh, fxjh) {
  
  misdf <- dplyr::filter(check, !!rlang::sym(dfxjh) != !!rlang::sym(fxjh))
  
  df[[xjh]][match(misdf[[dfxjh]], df[[xjh]])] <- misdf[[fxjh]]
  
  return(df)
}

# Use repmis_print function to see all the cases with mismatched XJH values
repmis_print <- function(check, dfxjh, fxjh, uiv = "cls_name") {
  
  misdf <- dplyr::filter(check, !!rlang::sym(dfxjh) != !!rlang::sym(fxjh)) %>%
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(fxjh), !!rlang::sym(dfxjh))
  
  print(misdf)
}

# Function that replaces NAs in XJH column extracted from the first source file
# with values in XJH columns extracted from all the other source files

fillna <- function(df, uiv = "cls_name") {
  
  # n is the position of "cls_name" column
  # n + 1 is the position of XJH extracted from the first source file
  # n + 2 is the position of XJH extracted from the second source file
  n <- which(colnames(df) == uiv)
  
  # check and print the number of NAs in XJH extracted from the first
  # source file before replacement
  m_before <- sum(is.na(df[[n + 1]]))
  
  cat("Number of NAs in XJH from the 1st source file before replacement:",
    m_before, "\n")
  
  for (i in (n + 2):ncol(df)) {
    
    df[[n + 1]][which(is.na(df[[n + 1]]))] <- df[[i]][which(is.na(df[[n + 1]]))]
  }
  
  # check and print the number of NAs in XJH extracted from the first
  # source file after replacement
  m_after <- sum(is.na(df[[n + 1]]))
  
  cat("Number of NAs in XJH from the 1st source file after replacement:",
    m_after, "\n")
  
  # rename the focal XJH column to "ssid"
  colnames(df)[n + 1] <- "ssid"
  
  # remove the other extracted XJH columns
  df[(n + 2):ncol(df)] <- NULL
  
  return(df)
}

# Function that replaces incorrect XJH values caused by
# duplicate values of the unique identifier in the target file

# df1 is the target file
# df2 is a chosen source file
# xhv1 is the name of the XH variable in df1
# xhv2 is the name of the XH variable in df2
# xjhv1 is the name of the XJH variable in df1
# xjhv2 is the name of the XJH variable in df2

# Use dup_print function to see if the target file has duplicate values
# in "cls_name" and if the chosen source file is appropriate
dup_print <- function(df1, df2, xhv1, xhv2,
  xjhv1 = "ssid", xjhv2 = "ssid", uiv = "cls_name") {
  
  # Capture the name of df1
  df1_name <- deparse(substitute(df1))
  
  # Capture the name of df2
  df2_name <- deparse(substitute(df2))
  
  df1_dup <- df1 %>%
    dplyr::group_by(!!rlang::sym(uiv)) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1),
      !!rlang::sym(xjhv1)) %>%
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv1))
  
  df2_dup <- df2 %>%
    dplyr::group_by(!!rlang::sym(uiv)) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv2),
      !!rlang::sym(xjhv2)) %>%
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv2))
  
  cat("Students with duplicate values in cls_name in the target file",
    df1_name, "\n")
  
  print(df1_dup)
  
  cat("Students with duplicate values in cls_name in the source file",
    df2_name, "\n")
  
  print(df2_dup)
  
}

dup <- function(df1, df2, xhv1, xhv2,
  xjhv1 = "ssid", xjhv2 = "ssid", uiv = "cls_name") {
  
  # Capture the name of df1
  df1_name <- deparse(substitute(df1))
  
  df1_dup <- df1 %>%
    dplyr::group_by(!!rlang::sym(uiv)) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1), !!rlang::sym(xjhv1))
  
  for (i in 1:nrow(df1_dup)) {
    
    ui <- df1_dup[[i, 1]]
    
    xh <- df1_dup[[i, 2]]
    
    df1[[xjhv1]][df1[[uiv]] == ui & df1[[xhv1]] == xh] <-
      df2[[xjhv2]][df2[[uiv]] == ui & df2[[xhv2]] == xh]
    
  }
  
  df1_dup_after <- df1 %>%
    dplyr::group_by(!!rlang::sym(uiv)) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1),
      !!rlang::sym(xjhv1)) %>%
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv1))
  
  cat("Students with duplicate values in cls_name in the target file",
    df1_name, "after replacement\n")
  
  print(df1_dup_after)
  
  return(df1)
}

# Cohort 2003 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c03_zkg1g2 <- cui(c03_zkg1g2, bh = "bh2m", xm = "姓名")
c03_zk <- cui(c03_zk, bh = "bh", xm = "姓名") # Target file
c03_base <- cui(c03_base, bh = "bh2m", xm = "姓名")

# Tidy XJH values in source files
c03_zkg1g2 <- tidyxjh(c03_zkg1g2, xjh = "xjh")
c03_base <- tidyxjh(c03_base, xjh = "xjh")

### >>Add XJH to **c03_zk** ----

# Left_join XJH from all source files to the target file
c03_zk <- c03_zk %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_zk <- fillna(c03_zk)

# Further make sure "ssid" is tidy
c03_zk <- tidyxjh(c03_zk, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c03_zkg1g2 <- cui(c03_zkg1g2, bh = "bh", xm = "姓名")
c03_jc1 <- cui(c03_jc1, bh = "bh", xm = "姓名") # Target file
c03_jc2 <- cui(c03_jc2, bh = "bh", xm = "姓名") # Target file
c03_jc3 <- cui(c03_jc3, bh = "bh", xm = "姓名") # Target file
c03_jc4 <- cui(c03_jc4, bh = "bh", xm = "姓名") # Target file
c03_jc5 <- cui(c03_jc5, bh = "bh", xm = "姓名") # Target file
c03_mn1 <- cui(c03_mn1, bh = "bh", xm = "姓名") # Target file
c03_mn2 <- cui(c03_mn2, bh = "bh", xm = "姓名") # Target file
c03_gk <- cui(c03_gk, bh = "班级", xm = "姓名") # Target file
c03_base <- cui(c03_base, bh = "bh", xm = "姓名")

### >>Add XJH to **c03_jc1** ----

# Left_join XJH from all source files to the target file
c03_jc1 <- c03_jc1 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_jc1 <- fillna(c03_jc1)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_jc1 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_jc1, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_jc1$ssid[c03_jc1$cls_name == "1_王春燕"] <- NA
c03_jc1$ssid[c03_jc1$cls_name == "26_孙玫洁"] <- NA

# Further make sure "ssid" is tidy
c03_jc1 <- tidyxjh(c03_jc1, xjh = "ssid")

### >>Add XJH to **c03_jc2** ----

# Left_join XJH from all source files to the target file
c03_jc2 <- c03_jc2 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_jc2 <- fillna(c03_jc2)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_jc2 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_jc2, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_jc2$ssid[c03_jc2$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_jc2 <- tidyxjh(c03_jc2, xjh = "ssid")

### >>Add XJH to **c03_jc3** ----

# Left_join XJH from all source files to the target file
c03_jc3 <- c03_jc3 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_jc3 <- fillna(c03_jc3)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_jc3 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_jc3, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_jc3$ssid[c03_jc3$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_jc3 <- tidyxjh(c03_jc3, xjh = "ssid")

### >>Add XJH to **c03_jc4** ----

# Left_join XJH from all source files to the target file
c03_jc4 <- c03_jc4 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_jc4 <- fillna(c03_jc4)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_jc4 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_jc4, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_jc4$ssid[c03_jc4$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_jc4 <- tidyxjh(c03_jc4, xjh = "ssid")

### >>Add XJH to **c03_jc5** ----

# Left_join XJH from all source files to the target file
c03_jc5 <- c03_jc5 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_jc5 <- fillna(c03_jc5)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_jc5 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_jc5, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_jc5$ssid[c03_jc5$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_jc5 <- tidyxjh(c03_jc5, xjh = "ssid")

### >>Add XJH to **c03_mn1** ----

# Left_join XJH from all source files to the target file
c03_mn1 <- c03_mn1 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_mn1 <- fillna(c03_mn1)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_mn1 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_mn1, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_mn1$ssid[c03_mn1$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_mn1 <- tidyxjh(c03_mn1, xjh = "ssid")

### >>Add XJH to **c03_mn2** ----

# Left_join XJH from all source files to the target file
c03_mn2 <- c03_mn2 %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_mn2 <- fillna(c03_mn2)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_mn2 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c03_mn2, c03_base, xhv1 = "xh", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_mn2$ssid[c03_mn2$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_mn2 <- tidyxjh(c03_mn2, xjh = "ssid")

### >>Add XJH to **c03_gk** ----

# Left_join XJH from all source files to the target file
c03_gk <- c03_gk %>%
  left_join(select(c03_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c03_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") 

# Perform the function 'fillna'
c03_gk <- fillna(c03_gk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c03_gk %>% count(cls_name) %>% filter(n > 1)

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c03_gk$ssid[c03_gk$cls_name == "13_李鹏"] <- NA

# Further make sure "ssid" is tidy
c03_gk <- tidyxjh(c03_gk, xjh = "ssid")

# Cohort 2004 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c04_zkg1g2 <- cui(c04_zkg1g2, bh = "bh0506", xm = "市姓名")
c04_20050128qm <- cui(c04_20050128qm, bh = "BH", xm = "姓名") # Target file
c04_20050324yk <- cui(c04_20050324yk, bh = "D_BH", xm = "D_NAME") # Target file
c04_20050630qm <- cui(c04_20050630qm, bh = "BH", xm = "姓名") # Target file
c04_base <- cui(c04_base, bh = "bh0506", xm = "姓名")

# Tidy XJH values in source files
c04_zkg1g2 <- tidyxjh(c04_zkg1g2, xjh = "xjh")
c04_base <- tidyxjh(c04_base, xjh = "xjh")

# Tidy XJH value for c04_mn2
c04_mn2 <- tidyxjh(c04_mn2, xjh = "ID")

### >>Add XJH to **c04_20050128qm** ----

# Left_join XJH from all source files to the target file
c04_20050128qm <- c04_20050128qm %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20050128qm <- fillna(c04_20050128qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20050128qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20050128qm, c04_base, xhv1 = "XH", xhv2 = "xh0506")

# Perform the replacement
c04_20050128qm$ssid[c04_20050128qm$cls_name == "10_李振" & c04_20050128qm$XH == 17] <- c04_base$ssid[c04_base$cls_name == "10_李振" & c04_base$xh0506 == "17"]

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_20050128qm$ssid[c04_20050128qm$cls_name == "8_张帅"] <- NA

# Further make sure "ssid" is tidy
c04_20050128qm <- tidyxjh(c04_20050128qm, xjh = "ssid")

### >>Add XJH to **c04_20050324yk** ----

# Left_join XJH from all source files to the target file
c04_20050324yk <- c04_20050324yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20050324yk <- fillna(c04_20050324yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20050324yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20050324yk, c04_base, xhv1 = "D_XH", xhv2 = "xh0506")

# Perform the replacement using function 'dup'
c04_20050324yk <- dup(c04_20050324yk, c04_base, xhv1 = "D_XH", xhv2 = "xh0506")

# Further make sure "ssid" is tidy
c04_20050324yk <- tidyxjh(c04_20050324yk, xjh = "ssid")

### >>Add XJH to **c04_20050630qm** ----

# Left_join XJH from all source files to the target file
c04_20050630qm <- c04_20050630qm %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20050630qm <- fillna(c04_20050630qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20050630qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20050630qm, c04_base, xhv1 = "XH", xhv2 = "xh0506")

# Perform the replacement using function 'dup'
c04_20050630qm <- dup(c04_20050630qm, c04_base, xhv1 = "XH", xhv2 = "xh0506")

# Further make sure "ssid" is tidy
c04_20050630qm <- tidyxjh(c04_20050630qm, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c04_zkg1g2 <- cui(c04_zkg1g2, bh = "bh", xm = "市姓名")
c04_20050924yk <- cui(c04_20050924yk, bh = "BH", xm = "姓名") # Target file
c04_20051104qz <- cui(c04_20051104qz, bh = "BH", xm = "姓名") # Target file
c04_20051216yk <- cui(c04_20051216yk, bh = "BH", xm = "姓名") # Target file
c04_20060115qm <- cui(c04_20060115qm, bh = "BH", xm = "姓名") # Target file
c04_20060212yk <- cui(c04_20060212yk, bh = "BH", xm = "姓名") # Target file
c04_20060324yk <- cui(c04_20060324yk, bh = "BH", xm = "姓名") # Target file
c04_20060426qz <- cui(c04_20060426qz, bh = "BH", xm = "姓名") # Target file
c04_20060612yk <- cui(c04_20060612yk, bh = "BH", xm = "姓名") # Target file
c04_20060712qm <- cui(c04_20060712qm, bh = "BH", xm = "姓名") # Target file
c04_20060902yk <- cui(c04_20060902yk, bh = "BH", xm = "姓名") # Target file
c04_20061012yk <- cui(c04_20061012yk, bh = "BH", xm = "姓名") # Target file
c04_jc1 <- cui(c04_jc1, bh = "BH", xm = "姓名") # Target file
c04_jc2 <- cui(c04_jc2, bh = "BH", xm = "姓名") # Target file
c04_jc3 <- cui(c04_jc3, bh = "BH", xm = "姓名") # Target file
c04_jc4 <- cui(c04_jc4, bh = "BH", xm = "姓名") # Target file
c04_mn1 <- cui(c04_mn1, bh = "BH", xm = "姓名") # Target file
c04_gk <- cui(c04_gk, bh = "BJ", xm = "XM")
c04_base <- cui(c04_base, bh = "bh", xm = "姓名")

# Tidy XJH values in source files
c04_gk <- tidyxjh(c04_gk, xjh = "HKKH")

### >>Add XJH to **c04_20050924yk** ----

# Left_join XJH from all source files to the target file
c04_20050924yk <- c04_20050924yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20050924yk <- fillna(c04_20050924yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20050924yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20050924yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20050924yk <- dup(c04_20050924yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20050924yk <- tidyxjh(c04_20050924yk, xjh = "ssid")

### >>Add XJH to **c04_20051104qz** ----

# Left_join XJH from all source files to the target file
c04_20051104qz <- c04_20051104qz %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20051104qz <- fillna(c04_20051104qz)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20051104qz %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20051104qz, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20051104qz <- dup(c04_20051104qz, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20051104qz <- tidyxjh(c04_20051104qz, xjh = "ssid")

### >>Add XJH to **c04_20051216yk** ----

# Left_join XJH from all source files to the target file
c04_20051216yk <- c04_20051216yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20051216yk <- fillna(c04_20051216yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20051216yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20051216yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20051216yk <- dup(c04_20051216yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20051216yk <- tidyxjh(c04_20051216yk, xjh = "ssid")

### >>Add XJH to **c04_20060115qm** ----

# Left_join XJH from all source files to the target file
c04_20060115qm <- c04_20060115qm %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060115qm <- fillna(c04_20060115qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060115qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060115qm, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060115qm <- dup(c04_20060115qm, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060115qm <- tidyxjh(c04_20060115qm, xjh = "ssid")

### >>Add XJH to **c04_20060212yk** ----

# Left_join XJH from all source files to the target file
c04_20060212yk <- c04_20060212yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060212yk <- fillna(c04_20060212yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060212yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060212yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060212yk <- dup(c04_20060212yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060212yk <- tidyxjh(c04_20060212yk, xjh = "ssid")

### >>Add XJH to **c04_20060324yk** ----

# Left_join XJH from all source files to the target file
c04_20060324yk <- c04_20060324yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060324yk <- fillna(c04_20060324yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060324yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060324yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060324yk <- dup(c04_20060324yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060324yk <- tidyxjh(c04_20060324yk, xjh = "ssid")

### >>Add XJH to **c04_20060426qz** ----

# Left_join XJH from all source files to the target file
c04_20060426qz <- c04_20060426qz %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060426qz <- fillna(c04_20060426qz)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060426qz %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060426qz, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060426qz <- dup(c04_20060426qz, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060426qz <- tidyxjh(c04_20060426qz, xjh = "ssid")

### >>Add XJH to **c04_20060612yk** ----

# Left_join XJH from all source files to the target file
c04_20060612yk <- c04_20060612yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060612yk <- fillna(c04_20060612yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060612yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060612yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060612yk <- dup(c04_20060612yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060612yk <- tidyxjh(c04_20060612yk, xjh = "ssid")

### >>Add XJH to **c04_20060712qm** ----

# Left_join XJH from all source files to the target file
c04_20060712qm <- c04_20060712qm %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060712qm <- fillna(c04_20060712qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060712qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060712qm, c04_base, xhv1 = "XH", xhv2 = "xh")

# Perform the replacement using function 'dup'
c04_20060712qm <- dup(c04_20060712qm, c04_base, xhv1 = "XH", xhv2 = "xh")

# Further make sure "ssid" is tidy
c04_20060712qm <- tidyxjh(c04_20060712qm, xjh = "ssid")

### >>Add XJH to **c04_20060902yk** ----

# Left_join XJH from all source files to the target file
c04_20060902yk <- c04_20060902yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20060902yk <- fillna(c04_20060902yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20060902yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20060902yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_20060902yk$ssid[c04_20060902yk$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_20060902yk <- tidyxjh(c04_20060902yk, xjh = "ssid")

### >>Add XJH to **c04_20061012yk** ----

# Left_join XJH from all source files to the target file
c04_20061012yk <- c04_20061012yk %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_20061012yk <- fillna(c04_20061012yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_20061012yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_20061012yk, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_20061012yk$ssid[c04_20061012yk$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_20061012yk <- tidyxjh(c04_20061012yk, xjh = "ssid")

### >>Add XJH to **c04_jc1** ----

# Left_join XJH from all source files to the target file
c04_jc1 <- c04_jc1 %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_jc1 <- fillna(c04_jc1)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_jc1 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_jc1, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_jc1$ssid[c04_jc1$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_jc1 <- tidyxjh(c04_jc1, xjh = "ssid")

### >>Add XJH to **c04_jc2** ----

# Left_join XJH from all source files to the target file
c04_jc2 <- c04_jc2 %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_jc2 <- fillna(c04_jc2)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_jc2 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_jc2, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_jc2$ssid[c04_jc2$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_jc2 <- tidyxjh(c04_jc2, xjh = "ssid")

### >>Add XJH to **c04_jc3** ----

# Left_join XJH from all source files to the target file
c04_jc3 <- c04_jc3 %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_jc3 <- fillna(c04_jc3)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_jc3 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_jc3, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_jc3$ssid[c04_jc3$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_jc3 <- tidyxjh(c04_jc3, xjh = "ssid")

### >>Add XJH to **c04_jc4** ----

# Left_join XJH from all source files to the target file
c04_jc4 <- c04_jc4 %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_jc4 <- fillna(c04_jc4)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_jc4 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_jc4, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_jc4$ssid[c04_jc4$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_jc4 <- tidyxjh(c04_jc4, xjh = "ssid")

### >>Add XJH to **c04_mn1** ----

# Left_join XJH from all source files to the target file
c04_mn1 <- c04_mn1 %>%
  left_join(select(c04_zkg1g2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c04_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c04_mn1 <- fillna(c04_mn1)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c04_mn1 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c04_mn1, c04_base, xhv1 = "XH", xhv2 = "xh")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c04_mn1$ssid[c04_mn1$cls_name == "10_李振"] <- NA

# Further make sure "ssid" is tidy
c04_mn1 <- tidyxjh(c04_mn1, xjh = "ssid")

# Cohort 2005 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c05_zkg1g2g3 <- cui(c05_zkg1g2g3, bh = "bh", xm = "xm")
c05_zk <- cui(c05_zk, bh = "BH", xm = "姓名") # Target file
c05_20060818yk <- cui(c05_20060818yk, bh = "BH", xm = "姓名")
c05_20061005yk <- cui(c05_20061005yk, bh = "BH", xm = "姓名")
c05_20061106qz <- cui(c05_20061106qz, bh = "BH", xm = "姓名")
c05_20061129yk <- cui(c05_20061129yk, bh = "BH", xm = "姓名")
c05_20070206qm <- cui(c05_20070206qm, bh = "BH", xm = "姓名")
c05_20070329yk <- cui(c05_20070329yk, bh = "BH", xm = "姓名")
c05_20070429qz <- cui(c05_20070429qz, bh = "BH", xm = "姓名")
c05_20070701qm_hnl <- cui(c05_20070701qm_hnl, bh = "BH", xm = "姓名")
c05_20070901yk_wj <- cui(c05_20070901yk_wj, bh = "BH", xm = "姓名")
c05_20070928jc_wj <- cui(c05_20070928jc_wj, bh = "BH", xm = "姓名")
c05_20071110qz_wj <- cui(c05_20071110qz_wj, bh = "BH", xm = "姓名")
c05_20071217jc_wj <- cui(c05_20071217jc_wj, bh = "BH", xm = "姓名")
c05_20080201qm_wj <- cui(c05_20080201qm_wj, bh = "BH", xm = "姓名") # Target file
c05_20080229mn1_wj_zh <- cui(c05_20080229mn1_wj_zh, bh = "BH", xm = "姓名") # Target file
c05_20080430mn2_wj <- cui(c05_20080430mn2_wj, bh = "BH", xm = "姓名") # Target file
c05_base <- cui(c05_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c05_zkg1g2g3 <- tidyxjh(c05_zkg1g2g3, xjh = "zcxh")
c05_20060818yk <- tidyxjh(c05_20060818yk, xjh = "XJH")
c05_20061005yk <- tidyxjh(c05_20061005yk, xjh = "XJH")
c05_20061106qz <- tidyxjh(c05_20061106qz, xjh = "XJH")
c05_20061129yk <- tidyxjh(c05_20061129yk, xjh = "XJH")
c05_20070206qm <- tidyxjh(c05_20070206qm, xjh = "XJH")
c05_20070329yk <- tidyxjh(c05_20070329yk, xjh = "XJH")
c05_20070429qz <- tidyxjh(c05_20070429qz, xjh = "XJH")
c05_20070701qm_hnl <- tidyxjh(c05_20070701qm_hnl, xjh = "XJH")
c05_20070901yk_wj <- tidyxjh(c05_20070901yk_wj, xjh = "xjh")
c05_20070928jc_wj <- tidyxjh(c05_20070928jc_wj, xjh = "XJH")
c05_20071110qz_wj <- tidyxjh(c05_20071110qz_wj, xjh = "XJH")
c05_20071217jc_wj <- tidyxjh(c05_20071217jc_wj, xjh = "XJH")
c05_base <- tidyxjh(c05_base, xjh = "zcxh")

# Tidy XJH value for c05_gk (no class number variable)
c05_gk <- tidyxjh(c05_gk, xjh = "注册学籍号")

### >>Add XJH to **c05_zk** ----

# Left_join XJH from all source files to the target file
c05_zk <- c05_zk %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20060818yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061005yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061106qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061129yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070206qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070329yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070429qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070701qm_hnl, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070901yk_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070928jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071110qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071217jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_zk <- fillna(c05_zk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_zk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_zk, c05_20060818yk, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c05_zk <- dup(c05_zk, c05_20060818yk, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c05_zk <- tidyxjh(c05_zk, xjh = "ssid")

### >>Add XJH to **c05_20080201qm_wj** ----

# Left_join XJH from all source files to the target file
c05_20080201qm_wj <- c05_20080201qm_wj %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20060818yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061005yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061106qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061129yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070206qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070329yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070429qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070701qm_hnl, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070901yk_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070928jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071110qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071217jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20080201qm_wj <- fillna(c05_20080201qm_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20080201qm_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20080201qm_wj, c05_20071217jc_wj, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c05_20080201qm_wj <- dup(c05_20080201qm_wj, c05_20071217jc_wj, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c05_20080201qm_wj <- tidyxjh(c05_20080201qm_wj, xjh = "ssid")

### >>Add XJH to **c05_20080229mn1_wj_zh** ----

# Left_join XJH from all source files to the target file
c05_20080229mn1_wj_zh <- c05_20080229mn1_wj_zh %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20060818yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061005yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061106qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061129yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070206qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070329yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070429qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070701qm_hnl, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070901yk_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070928jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071110qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071217jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20080229mn1_wj_zh <- fillna(c05_20080229mn1_wj_zh)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20080229mn1_wj_zh %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20080229mn1_wj_zh, c05_20071217jc_wj, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c05_20080229mn1_wj_zh <- dup(c05_20080229mn1_wj_zh, c05_20071217jc_wj, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c05_20080229mn1_wj_zh <- tidyxjh(c05_20080229mn1_wj_zh, xjh = "ssid")

### >>Add XJH to **c05_20080430mn2_wj** ----

# Left_join XJH from all source files to the target file
c05_20080430mn2_wj <- c05_20080430mn2_wj %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20060818yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061005yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061106qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20061129yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070206qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070329yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070429qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070701qm_hnl, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070901yk_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20070928jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071110qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_20071217jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20080430mn2_wj <- fillna(c05_20080430mn2_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20080430mn2_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20080430mn2_wj, c05_20071217jc_wj, xhv1 = "XH", xhv2 = "XH")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c05_20080430mn2_wj$ssid[c05_20080430mn2_wj$cls_name == "28_孙荣雪"] <- NA
c05_20080430mn2_wj$ssid[c05_20080430mn2_wj$cls_name == "6_李晓"] <- NA
c05_20080430mn2_wj$ssid[c05_20080430mn2_wj$cls_name == "7_王宁"] <- NA

# Further make sure "ssid" is tidy
c05_20080430mn2_wj <- tidyxjh(c05_20080430mn2_wj, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c05_zkg1g2g3 <- cui(c05_zkg1g2g3, bh = "bh0", xm = "xm")
c05_20051106qz <- cui(c05_20051106qz, bh = "BH", xm = "姓名") # Target file
c05_20051222yk <- cui(c05_20051222yk, bh = "BH", xm = "姓名") # Target file
c05_20060115qm <- cui(c05_20060115qm, bh = "BH", xm = "姓名") # Target file
c05_20060212yk <- cui(c05_20060212yk, bh = "BH", xm = "姓名") # Target file
c05_20060324yk <- cui(c05_20060324yk, bh = "BH", xm = "姓名") # Target file
c05_base <- cui(c05_base, bh = "bh1", xm = "xm")

### >>Add XJH to **c05_20051106qz** ----

# Left_join XJH from all source files to the target file
c05_20051106qz <- c05_20051106qz %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20051106qz <- fillna(c05_20051106qz)

# Further make sure "ssid" is tidy
c05_20051106qz <- tidyxjh(c05_20051106qz, xjh = "ssid")

### >>Add XJH to **c05_20051222yk** ----

# Left_join XJH from all source files to the target file
c05_20051222yk <- c05_20051222yk %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20051222yk <- fillna(c05_20051222yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20051222yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20051222yk, c05_zkg1g2g3, xhv1 = "XH", xhv2 = "xh1")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c05_20051222yk$ssid[c05_20051222yk$cls_name == "13_王浩铭"] <- NA

# Further make sure "ssid" is tidy
c05_20051222yk <- tidyxjh(c05_20051222yk, xjh = "ssid")

### >>Add XJH to **c05_20060115qm** ----

# Left_join XJH from all source files to the target file
c05_20060115qm <- c05_20060115qm %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20060115qm <- fillna(c05_20060115qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20060115qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20060115qm, c05_zkg1g2g3, xhv1 = "XH", xhv2 = "xh1")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c05_20060115qm$ssid[c05_20060115qm$cls_name == "13_王浩铭"] <- NA

# Further make sure "ssid" is tidy
c05_20060115qm <- tidyxjh(c05_20060115qm, xjh = "ssid")

### >>Add XJH to **c05_20060212yk** ----

# Left_join XJH from all source files to the target file
c05_20060212yk <- c05_20060212yk %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20060212yk <- fillna(c05_20060212yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20060212yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20060212yk, c05_zkg1g2g3, xhv1 = "XH", xhv2 = "xh1")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c05_20060212yk$ssid[c05_20060212yk$cls_name == "13_王浩铭"] <- NA

# Further make sure "ssid" is tidy
c05_20060212yk <- tidyxjh(c05_20060212yk, xjh = "ssid")

### >>Add XJH to **c05_20060324yk** ----

# Left_join XJH from all source files to the target file
c05_20060324yk <- c05_20060324yk %>%
  left_join(select(c05_zkg1g2g3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c05_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c05_20060324yk <- fillna(c05_20060324yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c05_20060324yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c05_20060324yk, c05_zkg1g2g3, xhv1 = "XH", xhv2 = "xh1")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c05_20060324yk$ssid[c05_20060324yk$cls_name == "13_王浩铭"] <- NA

# Further make sure "ssid" is tidy
c05_20060324yk <- tidyxjh(c05_20060324yk, xjh = "ssid")

# Cohort 2006 ####

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c06_20061106qz_xb <- cui(c06_20061106qz_xb, bh = "BH", xm = "姓名") #Target file
c06_20061226yk_qb <- cui(c06_20061226yk_qb, bh = "BH", xm = "姓名")
c06_20070206qm_qb <- cui(c06_20070206qm_qb, bh = "BH", xm = "姓名")
c06_base <- cui(c06_base, bh = "bh2", xm = "xm")

# Tidy XJH values in source files
c06_20061226yk_qb <- tidyxjh(c06_20061226yk_qb, xjh = "XJH")
c06_20070206qm_qb <- tidyxjh(c06_20070206qm_qb, xjh = "XJH")
c06_base <- tidyxjh(c06_base, xjh = "zcxh")

### >>Add XJH to **c06_20061106qz_xb** ----

# Left_join XJH from all source files to the target file
c06_20061106qz_xb <- c06_20061106qz_xb %>%
  left_join(select(c06_20061226yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20070206qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20061106qz_xb <- fillna(c06_20061106qz_xb)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20061106qz_xb %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20061106qz_xb, c06_20061226yk_qb, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c06_20061106qz_xb <- dup(c06_20061106qz_xb, c06_20061226yk_qb, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c06_20061106qz_xb <- tidyxjh(c06_20061106qz_xb, xjh = "ssid")

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c06_zk <- cui(c06_zk, bh = "BH", xm = "姓名") #Target file
c06_20061005yk <- cui(c06_20061005yk, bh = "BH", xm = "姓名") #Target file
c06_20061106qz_xb <- cui(c06_20061106qz_xb, bh = "BH2", xm = "姓名")
c06_base <- cui(c06_base, bh = "bh1", xm = "xm")

### >>Add XJH to **c06_zk** ----

# Left_join XJH from all source files to the target file
c06_zk <- c06_zk %>%
  left_join(select(c06_20061106qz_xb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_zk <- fillna(c06_zk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_zk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_zk, c06_base, xhv1 = "XH", xhv2 = "xh1")

# Perform the replacement
c06_zk$ssid[c06_zk$cls_name == "21_张悦" & c06_zk$XH == 61] <- c06_base$ssid[c06_base$cls_name == "21_张悦" & c06_base$xh1 == "61"]

# Further make sure "ssid" is tidy
c06_zk <- tidyxjh(c06_zk, xjh = "ssid")

### >>Add XJH to **c06_20061005yk** ----

# Left_join XJH from all source files to the target file
c06_20061005yk <- c06_20061005yk %>%
  left_join(select(c06_20061106qz_xb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20061005yk <- fillna(c06_20061005yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20061005yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20061005yk, c06_base, xhv1 = "XH", xhv2 = "xh1")

# Perform the replacement using function 'dup'
c06_20061005yk <- dup(c06_20061005yk, c06_base, xhv1 = "XH", xhv2 = "xh1")

# Further make sure "ssid" is tidy
c06_20061005yk <- tidyxjh(c06_20061005yk, xjh = "ssid")

## Class Number Set 3 ====

# No target file in this set

### Tidy XJH in Exam Files ----

c06_20070310yk_qb <- tidyxjh(c06_20070310yk_qb, xjh = "XJH")
c06_20070405yk_qb <- tidyxjh(c06_20070405yk_qb, xjh = "XJH")
c06_20070429qz_qb <- tidyxjh(c06_20070429qz_qb, xjh = "XJH")
c06_20070701qm_qb <- tidyxjh(c06_20070701qm_qb, xjh = "XJH")

## Class Number Set 4 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c06_20070917yk_qb <- cui(c06_20070917yk_qb, bh = "BH", xm = "姓名")
c06_20071115qz_qb <- cui(c06_20071115qz_qb, bh = "BH", xm = "姓名")
c06_20071227yk_qb <- cui(c06_20071227yk_qb, bh = "BH", xm = "姓名")
c06_20080126qm_qb <- cui(c06_20080126qm_qb, bh = "BH", xm = "姓名")
c06_20080505qz_qbkm <- cui(c06_20080505qz_qbkm, bh = "BH", xm = "姓名")
c06_20080707qm_qb <- cui(c06_20080707qm_qb, bh = "BH", xm = "姓名")
c06_20081007jc_qb <- cui(c06_20081007jc_qb, bh = "BH", xm = "姓名") #Target file
c06_20081106jc_before <- cui(c06_20081106jc_before, bh = "BH", xm = "姓名") #Target file
c06_20090115qm <- cui(c06_20090115qm, bh = "BH", xm = "姓名") #Target file
c06_20090215jc <- cui(c06_20090215jc, bh = "BH", xm = "姓名") #Target file
c06_20090315mn1 <- cui(c06_20090315mn1, bh = "BH", xm = "姓名") #Target file
c06_20090429mn2 <- cui(c06_20090429mn2, bh = "BH", xm = "姓名") #Target file
c06_gk <- cui(c06_gk, bh = "班号", xm = "xm_a")
c06_base <- cui(c06_base, bh = "bh5", xm = "xm")

# Tidy XJH values in source files
c06_20070917yk_qb <- tidyxjh(c06_20070917yk_qb, xjh = "XJH")
c06_20071115qz_qb <- tidyxjh(c06_20071115qz_qb, xjh = "XJH")
c06_20071227yk_qb <- tidyxjh(c06_20071227yk_qb, xjh = "XJH")
c06_20080126qm_qb <- tidyxjh(c06_20080126qm_qb, xjh = "XJH")
c06_20080505qz_qbkm <- tidyxjh(c06_20080505qz_qbkm, xjh = "XJH")
c06_20080707qm_qb <- tidyxjh(c06_20080707qm_qb, xjh = "XJH")
c06_gk <- tidyxjh(c06_gk, xjh = "hkkh")

### >>Add XJH to **c06_20081007jc_qb** ----
c06_20081007jc_qb <- c06_20081007jc_qb %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20081007jc_qb <- fillna(c06_20081007jc_qb)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20081007jc_qb %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20081007jc_qb, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20081007jc_qb$ssid[c06_20081007jc_qb$cls_name == "25_李悦"] <- NA
c06_20081007jc_qb$ssid[c06_20081007jc_qb$cls_name == "9_石君怡"] <- NA

# Further make sure "ssid" is tidy
c06_20081007jc_qb <- tidyxjh(c06_20081007jc_qb, xjh = "ssid")

### >>Add XJH to **c06_20081106jc_before** ----

# Left_join XJH from all source files to the target file
c06_20081106jc_before <- c06_20081106jc_before %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20081106jc_before <- fillna(c06_20081106jc_before)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20081106jc_before %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20081106jc_before, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20081106jc_before$ssid[c06_20081106jc_before$cls_name == "25_李悦"] <- NA

# Further make sure "ssid" is tidy
c06_20081106jc_before <- tidyxjh(c06_20081106jc_before, xjh = "ssid")

### >>Add XJH to **c06_20090115qm** ----

# Left_join XJH from all source files to the target file
c06_20090115qm <- c06_20090115qm %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20090115qm <- fillna(c06_20090115qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20090115qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20090115qm, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20090115qm$ssid[c06_20090115qm$cls_name == "25_李悦"] <- NA

# Further make sure "ssid" is tidy
c06_20090115qm <- tidyxjh(c06_20090115qm, xjh = "ssid")

### >>Add XJH to **c06_20090215jc** ----

# Left_join XJH from all source files to the target file
c06_20090215jc <- c06_20090215jc %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20090215jc <- fillna(c06_20090215jc)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20090215jc %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20090215jc, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20090215jc$ssid[c06_20090215jc$cls_name == "25_李悦"] <- NA

# Further make sure "ssid" is tidy
c06_20090215jc <- tidyxjh(c06_20090215jc, xjh = "ssid")

### >>Add XJH to **c06_20090315mn1** ----

# Left_join XJH from all source files to the target file
c06_20090315mn1 <- c06_20090315mn1 %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20090315mn1 <- fillna(c06_20090315mn1)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20090315mn1 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20090315mn1, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20090315mn1$ssid[c06_20090315mn1$cls_name == "25_李悦"] <- NA

# Further make sure "ssid" is tidy
c06_20090315mn1 <- tidyxjh(c06_20090315mn1, xjh = "ssid")

### >>Add XJH to **c06_20090429mn2** ----

# Left_join XJH from all source files to the target file
c06_20090429mn2 <- c06_20090429mn2 %>%
  left_join(select(c06_20070917yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071115qz_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20071227yk_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080126qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080505qz_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_20080707qm_qb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c06_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c06_20090429mn2 <- fillna(c06_20090429mn2)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c06_20090429mn2 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c06_20090429mn2, c06_base, xhv1 = "XH", xhv2 = "xh5")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c06_20090429mn2$ssid[c06_20090429mn2$cls_name == "25_李悦"] <- NA
c06_20090429mn2$ssid[c06_20090429mn2$cls_name == "23_刘洋"] <- NA

# Further make sure "ssid" is tidy
c06_20090429mn2 <- tidyxjh(c06_20090429mn2, xjh = "ssid")

# Cohort 2007 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c07_zkg1gk <- cui(c07_zkg1gk, bh = "bh0710", xm = "xm")
c07_20071013yk_qbkm <- cui(c07_20071013yk_qbkm, bh = "BH", xm = "姓名")
c07_20071118qz <- cui(c07_20071118qz, bh = "BH", xm = "姓名")
c07_20071220yk <- cui(c07_20071220yk, bh = "BH", xm = "姓名")
c07_20080125qm_qbkm <- cui(c07_20080125qm_qbkm, bh = "BH", xm = "姓名")
c07_20080506qz <- cui(c07_20080506qz, bh = "BH", xm = "姓名")
c07_20080706qm <- cui(c07_20080706qm, bh = "BH", xm = "姓名")
c07_base <- cui(c07_base, bh = "bh0710", xm = "xm")

# Tidy XJH values in source files
c07_zkg1gk <- tidyxjh(c07_zkg1gk, xjh = "zcxh")
c07_20071013yk_qbkm <- tidyxjh(c07_20071013yk_qbkm, xjh = "XJH")
c07_20071118qz <- tidyxjh(c07_20071118qz, xjh = "XJH")
c07_20071220yk <- tidyxjh(c07_20071220yk, xjh = "XJH")
c07_20080125qm_qbkm <- tidyxjh(c07_20080125qm_qbkm, xjh = "XJH")
c07_20080506qz <- tidyxjh(c07_20080506qz, xjh = "XJH")
c07_20080706qm <- tidyxjh(c07_20080706qm, xjh = "XJH")
c07_base <- tidyxjh(c07_base, xjh = "zcxh")

# Correct XJH values in two exam files using 'repmis' function
check0701 <- select(c07_20071013yk_qbkm, cls_name, ssid) %>%
  full_join(select(c07_20071118qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_20071220yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_20080125qm_qbkm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_20080506qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_20080706qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_zkg1gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# c07_zkg1gk
# repmis_print(check0701, "ssid.x.x.x.x", "ssid.x")
c07_zkg1gk <- repmis(c07_zkg1gk, "ssid", check0701, "ssid.x.x.x.x", "ssid.x")

# Further make sure "ssid" is tidy
c07_zkg1gk <- tidyxjh(c07_zkg1gk, xjh = "ssid")

# c07_base
# repmis_print(check0701, "ssid.y.y.y.y", "ssid.x")
c07_base <- repmis(c07_base, "ssid", check0701, "ssid.y.y.y.y", "ssid.x")

# Further make sure "ssid" is tidy
c07_base <- tidyxjh(c07_base, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c07_20081103qz <- cui(c07_20081103qz, bh = "BH", xm = "姓名")
c07_20090118qm <- cui(c07_20090118qm, bh = "BH", xm = "姓名")
c07_20090415qz <- cui(c07_20090415qz, bh = "BH", xm = "姓名")
c07_20090708qm <- cui(c07_20090708qm, bh = "BH", xm = "姓名") # Target file
c07_20091009jc <- cui(c07_20091009jc, bh = "BH", xm = "姓名") # Target file
c07_20091111qz <- cui(c07_20091111qz, bh = "BH", xm = "姓名") # Target file
c07_20100203qm_wj <- cui(c07_20100203qm_wj, bh = "BH", xm = "姓名") # Target file
c07_20100430mn2_wj <- cui(c07_20100430mn2_wj, bh = "BH", xm = "姓名") # Target file
c07_gk <- cui(c07_gk, bh = "bh", xm = "xm") # Target file
c07_base <- cui(c07_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c07_20081103qz <- tidyxjh(c07_20081103qz, xjh = "XJH")
c07_20090118qm <- tidyxjh(c07_20090118qm, xjh = "XJH")
c07_20090415qz <- tidyxjh(c07_20090415qz, xjh = "XJH")

### >>Add XJH to **c07_20090708qm** ----

# Left_join XJH from all source files to the target file
c07_20090708qm <- c07_20090708qm %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_20090708qm <- fillna(c07_20090708qm)

# Further make sure "ssid" is tidy
c07_20090708qm <- tidyxjh(c07_20090708qm, xjh = "ssid")

### >>Add XJH to **c07_20091009jc** ----

# Left_join XJH from all source files to the target file
c07_20091009jc <- c07_20091009jc %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_20091009jc <- fillna(c07_20091009jc)

# Further make sure "ssid" is tidy
c07_20091009jc <- tidyxjh(c07_20091009jc, xjh = "ssid")

### >>Add XJH to **c07_20091111qz** ----

# Left_join XJH from all source files to the target file
c07_20091111qz <- c07_20091111qz %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_20091111qz <- fillna(c07_20091111qz)

# Further make sure "ssid" is tidy
c07_20091111qz <- tidyxjh(c07_20091111qz, xjh = "ssid")

### >>Add XJH to **c07_20100203qm_wj** ----

# Left_join XJH from all source files to the target file
c07_20100203qm_wj <- c07_20100203qm_wj %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_20100203qm_wj <- fillna(c07_20100203qm_wj)

# Further make sure "ssid" is tidy
c07_20100203qm_wj <- tidyxjh(c07_20100203qm_wj, xjh = "ssid")

### >>Add XJH to **c07_20100430mn2_wj** ----

# Left_join XJH from all source files to the target file
c07_20100430mn2_wj <- c07_20100430mn2_wj %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_20100430mn2_wj <- fillna(c07_20100430mn2_wj)

# Further make sure "ssid" is tidy
c07_20100430mn2_wj <- tidyxjh(c07_20100430mn2_wj, xjh = "ssid")

### >>Add XJH to **c07_gk** ----

# Left_join XJH from all source files to the target file
c07_gk <- c07_gk %>%
  left_join(select(c07_20090118qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20081103qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c07_20090415qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c07_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c07_gk <- fillna(c07_gk)

# Further make sure "ssid" is tidy
c07_gk <- tidyxjh(c07_gk, xjh = "ssid")

# Cohort 2008 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c08_20081106qz <- cui(c08_20081106qz, bh = "BH", xm = "姓名") # Target file
c08_20090116qm <- cui(c08_20090116qm, bh = "BH", xm = "姓名") # Target file
c08_20090416qz <- cui(c08_20090416qz, bh = "BH", xm = "姓名")
c08_20090709qm <- cui(c08_20090709qm, bh = "BH", xm = "姓名")
c08_20091111qz_xzb <- cui(c08_20091111qz_xzb, bh = "BH", xm = "姓名") # Target file 
c08_base <- cui2(c08_base, bh = "bh0908", xm = "xm")

# Tidy XJH values in source files
c08_20090416qz <- tidyxjh(c08_20090416qz, xjh = "XJH")
c08_20090709qm <- tidyxjh(c08_20090709qm, xjh = "XJH")
c08_base <- tidyxjh(c08_base, xjh = "zcxh")

### >>Add XJH to **c08_20081106qz** ----

# Left_join XJH from all source files to the target file
c08_20081106qz <- c08_20081106qz %>%
  left_join(select(c08_20090416qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20090709qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20081106qz <- fillna(c08_20081106qz)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20081106qz %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20081106qz, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c08_20081106qz <- dup(c08_20081106qz, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c08_20081106qz <- tidyxjh(c08_20081106qz, xjh = "ssid")

### >>Add XJH to **c08_20090116qm** ----

# Left_join XJH from all source files to the target file
c08_20090116qm <- c08_20090116qm %>%
  left_join(select(c08_20090416qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20090709qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20090116qm <- fillna(c08_20090116qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20090116qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20090116qm, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c08_20090116qm <- dup(c08_20090116qm, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c08_20090116qm <- tidyxjh(c08_20090116qm, xjh = "ssid")

### >>Add XJH to **c08_20091111qz_xzb** ----

# Left_join XJH from all source files to the target file
c08_20091111qz_xzb <- c08_20091111qz_xzb %>%
  left_join(select(c08_20090416qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20090709qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20091111qz_xzb <- fillna(c08_20091111qz_xzb)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20091111qz_xzb %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20091111qz_xzb, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c08_20091111qz_xzb <- dup(c08_20091111qz_xzb, c08_20090416qz, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c08_20091111qz_xzb <- tidyxjh(c08_20091111qz_xzb, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c08_20091111qz_zb_dy <- cui(c08_20091111qz_zb_dy, bh = "BH", xm = "姓名") # Target file
c08_20100201qm_dy_2 <- cui(c08_20100201qm_dy_2, bh = "BH", xm = "姓名") # Target file
c08_20100429qz <- cui(c08_20100429qz, bh = "BH", xm = "姓名") # Target file
c08_20100715qm <- cui(c08_20100715qm, bh = "BH", xm = "姓名") # Target file
c08_base <- cui2(c08_base, bh = "bh0909", xm = "xm")

### >>Add XJH to **c08_20091111qz_zb_dy** ----
c08_20091111qz_zb_dy <- c08_20091111qz_zb_dy %>% filter(!duplicated(KSH)) %>%
  left_join(select(c08_20091111qz_xzb, KSH, ssid),
            by = "KSH",
            na_matches = "never",
            relationship = "one-to-one")

# Further make sure "ssid" is tidy
c08_20091111qz_zb_dy <- tidyxjh(c08_20091111qz_zb_dy, xjh = "ssid")

### >>Add XJH to **c08_20100201qm_dy_2** ----

# Left_join XJH from all source files to the target file
c08_20100201qm_dy_2 <- c08_20100201qm_dy_2 %>%
  left_join(select(c08_20091111qz_zb_dy, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20100201qm_dy_2 <- fillna(c08_20100201qm_dy_2)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20100201qm_dy_2 %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20100201qm_dy_2, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Perform the replacement using function 'dup'
c08_20100201qm_dy_2 <- dup(c08_20100201qm_dy_2, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Further make sure "ssid" is tidy
c08_20100201qm_dy_2 <- tidyxjh(c08_20100201qm_dy_2, xjh = "ssid")

### >>Add XJH to **c08_20100429qz** ----

# Left_join XJH from all source files to the target file
c08_20100429qz <- c08_20100429qz %>%
  left_join(select(c08_20091111qz_zb_dy, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20100429qz <- fillna(c08_20100429qz)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20100429qz %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20100429qz, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Perform the replacement using function 'dup'
c08_20100429qz <- dup(c08_20100429qz, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Further make sure "ssid" is tidy
c08_20100429qz <- tidyxjh(c08_20100429qz, xjh = "ssid")

### >>Add XJH to **c08_20100715qm** ----

# Left_join XJH from all source files to the target file
c08_20100715qm <- c08_20100715qm %>%
  left_join(select(c08_20091111qz_zb_dy, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20100715qm <- fillna(c08_20100715qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20100715qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20100715qm, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Perform the replacement using function 'dup'
c08_20100715qm <- dup(c08_20100715qm, c08_base, xhv1 = "XH", xhv2 = "xh0909")

# Further make sure "ssid" is tidy
c08_20100715qm <- tidyxjh(c08_20100715qm, xjh = "ssid")

## Class Number Set 3 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c08_20101008jc <- cui(c08_20101008jc, bh = "BH", xm = "姓名")
c08_20101112qz_yj <- cui(c08_20101112qz_yj, bh = "BH", xm = "姓名")
c08_20101222jc_yj <- cui(c08_20101222jc_yj, bh = "BH", xm = "姓名")
c08_20110122jc_wj <- cui(c08_20110122jc_wj, bh = "BH", xm = "姓名") # XJH of students from previous cohorts are problematic
c08_20110318mn1_wj <- cui(c08_20110318mn1_wj, bh = "BH", xm = "姓名") # Target file
c08_20110427mn2_wj <- cui(c08_20110427mn2_wj, bh = "BH", xm = "姓名")
c08_20110528mn3_wj <- cui(c08_20110528mn3_wj, bh = "BH", xm = "姓名")
c08_base <- cui2(c08_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c08_20101008jc <- tidyxjh(c08_20101008jc, xjh = "XJH")
c08_20101112qz_yj <- tidyxjh(c08_20101112qz_yj, xjh = "XJH")
c08_20101222jc_yj <- tidyxjh(c08_20101222jc_yj, xjh = "XJH")
c08_20110122jc_wj <- tidyxjh(c08_20110122jc_wj, xjh = "XJH")
c08_20110427mn2_wj <- tidyxjh(c08_20110427mn2_wj, xjh = "XJH")
c08_20110528mn3_wj <- tidyxjh(c08_20110528mn3_wj, xjh = "XJH")

### >>Add XJH to **c08_20110122jc_wj** (for students from previous cohorts) ----

# Left_join XJH from relevant source files to the target file
c08_20110122jc_wj <- c08_20110122jc_wj %>%
  left_join(select(c08_20110427mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20110528mn3_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20110122jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20110122jc_wj <- fillna(c08_20110122jc_wj)

# Further make sure "ssid" is tidy
c08_20110122jc_wj <- tidyxjh(c08_20110122jc_wj, xjh = "ssid")

### >>Add XJH to **c08_20110318mn1_wj** ----

# Left_join XJH from all source files to the target file
c08_20110318mn1_wj <- c08_20110318mn1_wj %>%
  left_join(select(c08_20101008jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20101112qz_yj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20101222jc_yj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20110122jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20110427mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_20110528mn3_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c08_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c08_20110318mn1_wj <- fillna(c08_20110318mn1_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c08_20110318mn1_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c08_20110318mn1_wj, c08_20110528mn3_wj, xhv1 = "XH", xhv2 = "XH")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c08_20110318mn1_wj$ssid[c08_20110318mn1_wj$cls_name == "2_李晨"] <- NA

# Further make sure "ssid" is tidy
c08_20110318mn1_wj <- tidyxjh(c08_20110318mn1_wj, xjh = "ssid")

# Cohort 2009 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c09_20091111qz_2p <- cui(c09_20091111qz_2p, bh = "BH", xm = "姓名") # Target file
c09_20100201qm_2p <- cui(c09_20100201qm_2p, bh = "BH", xm = "姓名")
c09_20100304yk <- cui(c09_20100304yk, bh = "BH", xm = "姓名")
c09_20100430qz <- cui(c09_20100430qz, bh = "BH", xm = "姓名") # Target file
c09_20100715qm <- cui(c09_20100715qm, bh = "BH", xm = "姓名")
c09_base <- cui2(c09_base, bh = "bh1007", xm = "xm")

# Tidy XJH values in source files
c09_20100201qm_2p <- tidyxjh(c09_20100201qm_2p, xjh = "XJH")
c09_20100304yk <- tidyxjh(c09_20100304yk, xjh = "XJH")
c09_20100715qm <- tidyxjh(c09_20100715qm, xjh = "XJH")
c09_base <- tidyxjh(c09_base, xjh = "zcxh")

### >>Add XJH to **c09_20091111qz_2p** ----

# Left_join XJH from all source files to the target file
c09_20091111qz_2p <- c09_20091111qz_2p %>%
  left_join(select(c09_20100201qm_2p, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20100304yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20100715qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20091111qz_2p <- fillna(c09_20091111qz_2p)

# Further make sure "ssid" is tidy
c09_20091111qz_2p <- tidyxjh(c09_20091111qz_2p, xjh = "ssid")

### >>Add XJH to **c09_20100430qz** ----

# Left_join XJH from all source files to the target file
c09_20100430qz <- c09_20100430qz %>%
  left_join(select(c09_20100201qm_2p, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20100304yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20100715qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20100430qz <- fillna(c09_20100430qz)

# Further make sure "ssid" is tidy
c09_20100430qz <- tidyxjh(c09_20100430qz, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c09_20101015yk <- cui(c09_20101015yk, bh = "BH", xm = "姓名") 
c09_20101112qz <- cui(c09_20101112qz, bh = "BH", xm = "姓名") # Target file
c09_20110122qm_xzb <- cui(c09_20110122qm_xzb, bh = "BH", xm = "姓名")
c09_base <- cui2(c09_base, bh = "bh1009", xm = "xm")

# Tidy XJH values in source files
c09_20101015yk <- tidyxjh(c09_20101015yk, xjh = "XJH")
c09_20110122qm_xzb <- tidyxjh(c09_20110122qm_xzb, xjh = "XJH") 

### >>Add XJH to **c09_20101112qz** ----

# Left_join XJH from all source files to the target file
c09_20101112qz <- c09_20101112qz %>%
  left_join(select(c09_20101015yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110122qm_xzb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20101112qz <- fillna(c09_20101112qz)

# Further make sure "ssid" is tidy
c09_20101112qz <- tidyxjh(c09_20101112qz, xjh = "ssid")

## Class Number Set 3 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c09_20110428qz <- cui(c09_20110428qz, bh = "BH", xm = "姓名")
c09_20110708qm <- cui(c09_20110708qm, bh = "BH", xm = "姓名")
c09_20111007jc_wj <- cui(c09_20111007jc_wj, bh = "BH", xm = "姓名")
c09_20111106qz_wj <- cui(c09_20111106qz_wj, bh = "BH", xm = "姓名")
c09_20111212jc_wj <- cui(c09_20111212jc_wj, bh = "BH", xm = "姓名")
c09_20120113qm_wj_bzh <- cui(c09_20120113qm_wj_bzh, bh = "BH", xm = "XM") # Target file
c09_20120209jc_wj <- cui(c09_20120209jc_wj, bh = "BH", xm = "姓名")
c09_20120303mn1_wj <- cui(c09_20120303mn1_wj, bh = "BH", xm = "姓名") # Target file 
c09_20120326jc_wj <- cui(c09_20120326jc_wj, bh = "BH", xm = "姓名") # Target file 
c09_20120427mn2_wj <- cui(c09_20120427mn2_wj, bh = "BH", xm = "姓名") # Target file 
c09_20120527mn3_wj <- cui(c09_20120527mn3_wj, bh = "BH", xm = "姓名") # Target file 
c09_base <- cui2(c09_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c09_20110428qz <- tidyxjh(c09_20110428qz, xjh = "XJH")
c09_20110708qm <- tidyxjh(c09_20110708qm, xjh = "XJH")
c09_20111007jc_wj <- tidyxjh(c09_20111007jc_wj, xjh = "XJH")
c09_20111106qz_wj <- tidyxjh(c09_20111106qz_wj, xjh = "XJH") 
c09_20111212jc_wj <- tidyxjh(c09_20111212jc_wj, xjh = "XJH") 
c09_20120209jc_wj <- tidyxjh(c09_20120209jc_wj, xjh = "XJH")

### >>Add XJH to **c09_20120113qm_wj_bzh** ----

# Left_join XJH from all source files to the target file
c09_20120113qm_wj_bzh <- c09_20120113qm_wj_bzh %>%
  left_join(select(c09_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111007jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111106qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111212jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20120209jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20120113qm_wj_bzh <- fillna(c09_20120113qm_wj_bzh)

# Further make sure "ssid" is tidy
c09_20120113qm_wj_bzh <- tidyxjh(c09_20120113qm_wj_bzh, xjh = "ssid")

### >>Add XJH to **c09_20120303mn1_wj** ----

# Left_join XJH from all source files to the target file
c09_20120303mn1_wj <- c09_20120303mn1_wj %>%
  left_join(select(c09_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111007jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111106qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111212jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20120209jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20120303mn1_wj <- fillna(c09_20120303mn1_wj)

# Further make sure "ssid" is tidy
c09_20120303mn1_wj <- tidyxjh(c09_20120303mn1_wj, xjh = "ssid")

### >>Add XJH to **c09_20120326jc_wj** ----

# Left_join XJH from all source files to the target file
c09_20120326jc_wj <- c09_20120326jc_wj %>%
  left_join(select(c09_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111007jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111106qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111212jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20120209jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20120326jc_wj <- fillna(c09_20120326jc_wj)

# Further make sure "ssid" is tidy
c09_20120326jc_wj <- tidyxjh(c09_20120326jc_wj, xjh = "ssid")

### >>Add XJH to **c09_20120427mn2_wj** ----

# Left_join XJH from all source files to the target file
c09_20120427mn2_wj <- c09_20120427mn2_wj %>%
  left_join(select(c09_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111007jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111106qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111212jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20120209jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20120427mn2_wj <- fillna(c09_20120427mn2_wj)

# Further make sure "ssid" is tidy
c09_20120427mn2_wj <- tidyxjh(c09_20120427mn2_wj, xjh = "ssid")

### >>Add XJH to **c09_20120527mn3_wj** ----

# Left_join XJH from all source files to the target file
c09_20120527mn3_wj <- c09_20120527mn3_wj %>%
  left_join(select(c09_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20110708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111007jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111106qz_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20111212jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_20120209jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c09_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c09_20120527mn3_wj <- fillna(c09_20120527mn3_wj)

# Further make sure "ssid" is tidy
c09_20120527mn3_wj <- tidyxjh(c09_20120527mn3_wj, xjh = "ssid")

# Cohort 2010 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c10_20101110yk <- cui(c10_20101110yk, bh = "BH", xm = "姓名")
c10_20110122qm <- cui(c10_20110122qm, bh = "BH", xm = "姓名") # Target file
c10_20110428qz <- cui(c10_20110428qz, bh = "BH", xm = "姓名")
c10_20110708qm <- cui(c10_20110708qm, bh = "BH", xm = "姓名") # Target file
c10_20111104qz <- cui(c10_20111104qz, bh = "BH1", xm = "姓名")
c10_base <- cui2(c10_base, bh = "bhx", xm = "xm")

# Tidy XJH values in source files
c10_20101110yk <- tidyxjh(c10_20101110yk, xjh = "XJH")
c10_20110428qz <- tidyxjh(c10_20110428qz, xjh = "XJH")
c10_20111104qz <- tidyxjh(c10_20111104qz, xjh = "XJH")
c10_base <- tidyxjh(c10_base, xjh = "zcxh")

### >>Add XJH to **c10_20110122qm** ----

# Left_join XJH from all source files to the target file
c10_20110122qm <- c10_20110122qm %>%
  left_join(select(c10_20101110yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20111104qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c10_20110122qm <- fillna(c10_20110122qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c10_20110122qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c10_20110122qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c10_20110122qm <- dup(c10_20110122qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c10_20110122qm <- tidyxjh(c10_20110122qm, xjh = "ssid")

### >>Add XJH to **c10_20110708qm** ----

# Left_join XJH from all source files to the target file
c10_20110708qm <- c10_20110708qm %>%
  left_join(select(c10_20101110yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20110428qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20111104qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c10_20110708qm <- fillna(c10_20110708qm)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c10_20110708qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c10_20110708qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c10_20110708qm <- dup(c10_20110708qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c10_20110708qm <- tidyxjh(c10_20110708qm, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c10_20111104qz <- cui(c10_20111104qz, bh = "BH", xm = "姓名")
c10_20120113qm <- cui(c10_20120113qm, bh = "BH", xm = "姓名")
c10_20120419qz <- cui(c10_20120419qz, bh = "BH", xm = "姓名")
c10_20120706qm <- cui(c10_20120706qm, bh = "BH", xm = "姓名")
c10_20121009jc_wj <- cui(c10_20121009jc_wj, bh = "BH", xm = "姓名") 
c10_20121108jc_wj <- cui(c10_20121108jc_wj, bh = "BH", xm = "姓名") # Target file
c10_20121214jc_wj <- cui(c10_20121214jc_wj, bh = "BH", xm = "姓名")
c10_20130125qm_wj <- cui(c10_20130125qm_wj, bh = "BH", xm = "姓名")
c10_20130307mn1_wj <- cui(c10_20130307mn1_wj, bh = "BH", xm = "姓名")
c10_20130401yk_wj <- cui(c10_20130401yk_wj, bh = "BH", xm = "姓名") # Target file
c10_20130426mn2_wj <- cui(c10_20130426mn2_wj, bh = "BH", xm = "姓名")
c10_20130527mn3_wj <- cui(c10_20130527mn3_wj, bh = "BH", xm = "姓名") # Target file
c10_gk <- cui(c10_gk, bh = "班", xm = "姓名")
c10_base <- cui2(c10_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c10_20120113qm <- tidyxjh(c10_20120113qm, xjh = "XJH")
c10_20120419qz <- tidyxjh(c10_20120419qz, xjh = "XJH") 
c10_20120706qm <- tidyxjh(c10_20120706qm, xjh = "XJH")
c10_20121009jc_wj <- tidyxjh(c10_20121009jc_wj, xjh = "XJH")
c10_20121214jc_wj <- tidyxjh(c10_20121214jc_wj, xjh = "XJH")
c10_20130125qm_wj <- tidyxjh(c10_20130125qm_wj, xjh = "XJH")
c10_20130307mn1_wj <- tidyxjh(c10_20130307mn1_wj, xjh = "XJH")
c10_20130426mn2_wj <- tidyxjh(c10_20130426mn2_wj, xjh = "XJH")
c10_gk <- tidyxjh(c10_gk, xjh = "zcxjh")

# Correct XJH values in one exam file using 'repmis' function
check1002 <- select(c10_20111104qz, cls_name, ssid) %>%
  full_join(select(c10_20120113qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_20120419qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  full_join(select(c10_20120706qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_20121009jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  full_join(select(c10_20121214jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  full_join(select(c10_20130125qm_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_20130307mn1_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_20130426mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# c10_20130426mn2_wj
# repmis_print(check1002, "ssid.x.x.x.x.x", "ssid.x")
c10_20130426mn2_wj <- repmis(c10_20130426mn2_wj, "ssid", check1002, "ssid.x.x.x.x.x", "ssid.x")

# Further make sure "ssid" is tidy
c10_20130426mn2_wj <- tidyxjh(c10_20130426mn2_wj, xjh = "ssid")

### >>Add XJH to **c10_20121108jc_wj** ----

# Left_join XJH from all source files to the target file
c10_20121108jc_wj <- c10_20121108jc_wj %>% 
  left_join(select(c10_20111104qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20120113qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120419qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120706qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121009jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121214jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130125qm_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130307mn1_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130426mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c10_20121108jc_wj <- fillna(c10_20121108jc_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c10_20121108jc_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c10_20121108jc_wj, c10_20121009jc_wj, xhv1 = "XH", xhv2 = "XH")

# Perform the replacement using function 'dup'
c10_20121108jc_wj <- dup(c10_20121108jc_wj, c10_20121009jc_wj, xhv1 = "XH", xhv2 = "XH")

# Further make sure "ssid" is tidy
c10_20121108jc_wj <- tidyxjh(c10_20121108jc_wj, xjh = "ssid")

### >>Add XJH to **c10_20130401yk_wj** ----

# Left_join XJH from all source files to the target file
c10_20130401yk_wj <- c10_20130401yk_wj %>% 
  left_join(select(c10_20111104qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20120113qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120419qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120706qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121009jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121214jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130125qm_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130307mn1_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130426mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c10_20130401yk_wj <- fillna(c10_20130401yk_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c10_20130401yk_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c10_20130401yk_wj, c10_20130307mn1_wj, xhv1 = "XH", xhv2 = "XH")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c10_20130401yk_wj$ssid[c10_20130401yk_wj$cls_name == "26_张帅"] <- NA
c10_20130401yk_wj$ssid[c10_20130401yk_wj$cls_name == "29_陈阳"] <- NA
c10_20130401yk_wj$ssid[c10_20130401yk_wj$cls_name == "4_赵文豪"] <- NA

# Further make sure "ssid" is tidy
c10_20130401yk_wj <- tidyxjh(c10_20130401yk_wj, xjh = "ssid")

### >>Add XJH to **c10_20130527mn3_wj** ----

# Left_join XJH from all source files to the target file
c10_20130527mn3_wj <- c10_20130527mn3_wj %>% 
  left_join(select(c10_20111104qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20120113qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120419qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20120706qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121009jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20121214jc_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130125qm_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130307mn1_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20130426mn2_wj, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_gk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c10_20130527mn3_wj <- fillna(c10_20130527mn3_wj)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c10_20130527mn3_wj %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c10_20130527mn3_wj, c10_20130307mn1_wj, xhv1 = "XH", xhv2 = "XH")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c10_20130527mn3_wj$ssid[c10_20130527mn3_wj$cls_name == "26_张帅"] <- NA
c10_20130527mn3_wj$ssid[c10_20130527mn3_wj$cls_name == "29_陈阳"] <- NA
c10_20130527mn3_wj$ssid[c10_20130527mn3_wj$cls_name == "4_赵文豪"] <- NA

# Further make sure "ssid" is tidy
c10_20130527mn3_wj <- tidyxjh(c10_20130527mn3_wj, xjh = "ssid")

# Cohort 2011 ####

## Class Number Set 1 ====

# No target file in this set

### Tidy XJH in Exam Files ----

c11_zk <- tidyxjh(c11_zk, xjh = "XJH")
c11_20111110qz <- tidyxjh(c11_20111110qz, xjh = "XJH")
c11_20120113qm <- tidyxjh(c11_20120113qm, xjh = "XJH")
c11_20120420qz <- tidyxjh(c11_20120420qz, xjh = "XJH")
c11_20120701qm <- tidyxjh(c11_20120701qm, xjh = "XJH") 
c11_20121015yk_xzb <- tidyxjh(c11_20121015yk_xzb, xjh = "XJH")
c11_20121114qz_xzb <- tidyxjh(c11_20121114qz_xzb, xjh = "XJH")
c11_base <- tidyxjh(c11_base, xjh = "zcxh")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c11_20121015yk_xzb <- cui(c11_20121015yk_xzb, bh = "BHJ", xm = "姓名")
c11_20121114qz_xzb <- cui(c11_20121114qz_xzb, bh = "BHJ", xm = "姓名")
c11_20130130qm <- cui(c11_20130130qm, bh = "BH", xm = "姓名")
c11_20130325yk <- cui(c11_20130325yk, bh = "BH", xm = "姓名") # Target file
c11_20130507qz <- cui(c11_20130507qz, bh = "BH", xm = "姓名") 
c11_20130707qm <- cui(c11_20130707qm, bh = "BH", xm = "姓名") 
c11_20131012jc <- cui(c11_20131012jc, bh = "BH", xm = "姓名")
c11_20131107qz <- cui(c11_20131107qz, bh = "BH", xm = "姓名")
c11_20131222jc_zh <- cui(c11_20131222jc_zh, bh = "BH", xm = "姓名")
c11_20140117jc <- cui(c11_20140117jc, bh = "BH", xm = "姓名")
c11_20140120qm <- cui(c11_20140120qm, bh = "BH", xm = "姓名")
c11_20140306mn1 <- cui(c11_20140306mn1, bh = "BH", xm = "姓名")
c11_20140422mn2 <- cui(c11_20140422mn2, bh = "BH", xm = "姓名")
c11_base <- cui2(c11_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c11_20130130qm <- tidyxjh(c11_20130130qm, xjh = "XJH")
c11_20130507qz <- tidyxjh(c11_20130507qz, xjh = "XJH") 
c11_20130707qm <- tidyxjh(c11_20130707qm, xjh = "XJH")
c11_20131012jc <- tidyxjh(c11_20131012jc, xjh = "XJH")
c11_20131107qz <- tidyxjh(c11_20131107qz, xjh = "XJH")
c11_20131222jc_zh <- tidyxjh(c11_20131222jc_zh, xjh = "XJH")
c11_20140117jc <- tidyxjh(c11_20140117jc, xjh = "XJH")
c11_20140120qm <- tidyxjh(c11_20140120qm, xjh = "XJH")
c11_20140306mn1 <- tidyxjh(c11_20140306mn1, xjh = "XJH")
c11_20140422mn2 <- tidyxjh(c11_20140422mn2, xjh = "XJH")

### >>Add XJH to **c11_20130325yk** ----

# Left_join XJH from all source files to the target file
c11_20130325yk <- c11_20130325yk %>%
  left_join(select(c11_20121015yk_xzb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20121114qz_xzb, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20130130qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c11_20130507qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20130707qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20131012jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20131107qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20131222jc_zh, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20140117jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20140120qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20140306mn1, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_20140422mn2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c11_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c11_20130325yk <- fillna(c11_20130325yk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c11_20130325yk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c11_20130325yk, c11_20130130qm, xhv1 = "XH", xhv2 = "XH")

# Assign NA to "ssid" of students with duplicate "cls_name" values (unable to distinguish those students)
c11_20130325yk$ssid[c11_20130325yk$cls_name == "29_刘琦"] <- NA

# Further make sure "ssid" is tidy
c11_20130325yk <- tidyxjh(c11_20130325yk, xjh = "ssid")

### >>Add XJH to **c11_gk** ----

c11_gk <- c11_gk %>% filter(!duplicated(ksh)) %>% 
  left_join(select(c11_gk_info, ksh, zcxjh), 
            by = "ksh", 
            na_matches = "never",
            relationship = "one-to-one") %>% 
  rename(ssid = zcxjh)

# Further make sure "ssid" is tidy
c11_gk <- tidyxjh(c11_gk, xjh = "ssid")

# Cohort 2012 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c12_20121011yk <- cui(c12_20121011yk, bh = "BH", xm = "姓名") # Target file
c12_20121115qz <- cui(c12_20121115qz, bh = "BH", xm = "姓名")
c12_20130105yk <- cui(c12_20130105yk, bh = "BH", xm = "姓名") 
c12_20130130qm <- cui(c12_20130130qm, bh = "BH", xm = "姓名")
c12_20130407yk <- cui(c12_20130407yk, bh = "BH", xm = "姓名")
c12_20130504qz <- cui(c12_20130504qz, bh = "BH", xm = "姓名")
c12_20130620yk <- cui(c12_20130620yk, bh = "BH", xm = "姓名")
c12_20130708qm <- cui(c12_20130708qm, bh = "BH", xm = "姓名")
c12_base <- cui2(c12_base, bh = "bhx", xm = "xm") 

# Tidy XJH values in source files
c12_20121115qz <- tidyxjh(c12_20121115qz, xjh = "XJH")
c12_20130105yk <- tidyxjh(c12_20130105yk, xjh = "XJH")
c12_20130130qm <- tidyxjh(c12_20130130qm, xjh = "XJH") 
c12_20130407yk <- tidyxjh(c12_20130407yk, xjh = "XJH")
c12_20130504qz <- tidyxjh(c12_20130504qz, xjh = "XJH")
c12_20130620yk <- tidyxjh(c12_20130620yk, xjh = "XJH")
c12_20130708qm <- tidyxjh(c12_20130708qm, xjh = "XJH")
c12_base <- tidyxjh(c12_base, xjh = "zcxh")

### >>Add XJH to **c12_20121011yk** ----

# Left_join XJH from all source files to the target file
c12_20121011yk <- c12_20121011yk %>%
  left_join(select(c12_20121115qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20130105yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20130130qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c12_20130407yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20130504qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20130620yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20130708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c12_20121011yk <- fillna(c12_20121011yk)

# Further make sure "ssid" is tidy
c12_20121011yk <- tidyxjh(c12_20121011yk, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c12_20131008yk <- cui(c12_20131008yk, bh = "BH", xm = "姓名")
c12_20131112qz <- cui(c12_20131112qz, bh = "BH", xm = "姓名") 
c12_20140115qm <- cui(c12_20140115qm, bh = "BH", xm = "姓名")
c12_20140218yk <- cui(c12_20140218yk, bh = "BH", xm = "姓名")
c12_20140320yk <- cui(c12_20140320yk, bh = "BH", xm = "姓名")
c12_20140422qz <- cui(c12_20140422qz, bh = "BH", xm = "姓名")
c12_20140528yk <- cui(c12_20140528yk, bh = "BH", xm = "姓名")
c12_20140708qm <- cui(c12_20140708qm, bh = "BH", xm = "姓名")
c12_20140901jc <- cui(c12_20140901jc, bh = "BH", xm = "姓名")
c12_20141006jc <- cui(c12_20141006jc, bh = "BH", xm = "姓名")
c12_20141112qz <- cui(c12_20141112qz, bh = "BH", xm = "姓名")
c12_20141211jc <- cui(c12_20141211jc, bh = "BH", xm = "姓名")
c12_20150108jc <- cui(c12_20150108jc, bh = "BH", xm = "姓名")
c12_20150202qm_b <- cui(c12_20150202qm_b, bh = "BH", xm = "姓名")
c12_20150315mn1 <- cui(c12_20150315mn1, bh = "BH", xm = "姓名")
c12_20150413jc <- cui(c12_20150413jc, bh = "BH", xm = "姓名")
c12_20150506mn2 <- cui(c12_20150506mn2, bh = "BH", xm = "姓名")
c12_20150526mn3 <- cui(c12_20150526mn3, bh = "BH", xm = "姓名")
c12_gk <- cui(c12_gk, bh = "bh", xm = "姓名") # Target file
c12_base <- cui2(c12_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c12_20131008yk <- tidyxjh(c12_20131008yk, xjh = "XJH")
c12_20131112qz <- tidyxjh(c12_20131112qz, xjh = "XJH")
c12_20140115qm <- tidyxjh(c12_20140115qm, xjh = "XJH")
c12_20140218yk <- tidyxjh(c12_20140218yk, xjh = "XJH") 
c12_20140320yk <- tidyxjh(c12_20140320yk, xjh = "XJH")
c12_20140422qz <- tidyxjh(c12_20140422qz, xjh = "XJH")
c12_20140528yk <- tidyxjh(c12_20140528yk, xjh = "XJH")
c12_20140708qm <- tidyxjh(c12_20140708qm, xjh = "XJH")
c12_20140901jc <- tidyxjh(c12_20140901jc, xjh = "XJH")
c12_20141006jc <- tidyxjh(c12_20141006jc, xjh = "XJH")
c12_20141112qz <- tidyxjh(c12_20141112qz, xjh = "XJH")
c12_20141211jc <- tidyxjh(c12_20141211jc, xjh = "XJH")
c12_20150108jc <- tidyxjh(c12_20150108jc, xjh = "XJH")
c12_20150202qm_b <- tidyxjh(c12_20150202qm_b, xjh = "XJH")
c12_20150315mn1 <- tidyxjh(c12_20150315mn1, xjh = "XJH")
c12_20150413jc <- tidyxjh(c12_20150413jc, xjh = "XJH")
c12_20150506mn2 <- tidyxjh(c12_20150506mn2, xjh = "XJH")
c12_20150526mn3 <- tidyxjh(c12_20150526mn3, xjh = "XJH")

### >>Add XJH to **c12_gk** ----

# Left_join XJH from all source files to the target file
c12_gk <- c12_gk %>%
  left_join(select(c12_20131008yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20131112qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140115qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c12_20140218yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140320yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140422qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140528yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140708qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20140901jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20141006jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20141112qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20141211jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150108jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150202qm_b, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150315mn1, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150413jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150506mn2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_20150526mn3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c12_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c12_gk <- fillna(c12_gk)

# Further make sure "ssid" is tidy
c12_gk <- tidyxjh(c12_gk, xjh = "ssid")

# Cohort 2013 ####

## Class Number Set 1 ====

# No target file in this set

### Tidy XJH in Exam Files ----

c13_20131012yk <- tidyxjh(c13_20131012yk, xjh = "XJH")
c13_20131105qz <- tidyxjh(c13_20131105qz, xjh = "XJH")
c13_20140120qm <- tidyxjh(c13_20140120qm, xjh = "XJH")
c13_20140220yk <- tidyxjh(c13_20140220yk, xjh = "XJH")
c13_20140420qz <- tidyxjh(c13_20140420qz, xjh = "XJH") 
c13_20140526yk <- tidyxjh(c13_20140526yk, xjh = "XJH")
c13_20140703qm <- tidyxjh(c13_20140703qm, xjh = "XJH")
c13_base <- tidyxjh(c13_base, xjh = "zcxh")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c13_20140912yk <- cui(c13_20140912yk, bh = "BH", xm = "姓名")
c13_20141008yk <- cui(c13_20141008yk, bh = "BH", xm = "姓名") 
c13_20141117qz <- cui(c13_20141117qz, bh = "BH", xm = "姓名")
c13_20150104yk <- cui(c13_20150104yk, bh = "BH", xm = "姓名")
c13_20150201qm <- cui(c13_20150201qm, bh = "BH", xm = "姓名")
c13_20150307yk <- cui(c13_20150307yk, bh = "BH", xm = "姓名")
c13_20150401yk <- cui(c13_20150401yk, bh = "BH", xm = "姓名")
c13_20150508qz <- cui(c13_20150508qz, bh = "BH", xm = "姓名")
c13_20150707qm <- cui(c13_20150707qm, bh = "BH", xm = "姓名")
c13_20150928jc <- cui(c13_20150928jc, bh = "BH", xm = "姓名")
c13_20151102qz <- cui(c13_20151102qz, bh = "BH", xm = "姓名")
c13_20151217jc <- cui(c13_20151217jc, bh = "BH", xm = "姓名")
c13_20160128qm <- cui(c13_20160128qm, bh = "BH", xm = "姓名")
c13_20160223jc <- cui(c13_20160223jc, bh = "BH", xm = "姓名")
c13_20160306mn1 <- cui(c13_20160306mn1, bh = "BH", xm = "姓名")
c13_20160403jc <- cui(c13_20160403jc, bh = "BH", xm = "姓名")
c13_20160504mn2 <- cui(c13_20160504mn2, bh = "BH", xm = "姓名")
c13_gk <- cui(c13_gk, bh = "bh", xm = "姓名") # Target file
c13_base <- cui2(c13_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c13_20140912yk <- tidyxjh(c13_20140912yk, xjh = "XJH")
c13_20141008yk <- tidyxjh(c13_20141008yk, xjh = "XJH")
c13_20141117qz <- tidyxjh(c13_20141117qz, xjh = "XJH")
c13_20150104yk <- tidyxjh(c13_20150104yk, xjh = "XJH") 
c13_20150201qm <- tidyxjh(c13_20150201qm, xjh = "XJH")
c13_20150307yk <- tidyxjh(c13_20150307yk, xjh = "XJH")
c13_20150401yk <- tidyxjh(c13_20150401yk, xjh = "XJH")
c13_20150508qz <- tidyxjh(c13_20150508qz, xjh = "XJH")
c13_20150707qm <- tidyxjh(c13_20150707qm, xjh = "XJH")
c13_20150928jc <- tidyxjh(c13_20150928jc, xjh = "XJH")
c13_20151102qz <- tidyxjh(c13_20151102qz, xjh = "XJH")
c13_20151217jc <- tidyxjh(c13_20151217jc, xjh = "XJH")
c13_20160128qm <- tidyxjh(c13_20160128qm, xjh = "XJH")
c13_20160223jc <- tidyxjh(c13_20160223jc, xjh = "XJH")
c13_20160306mn1 <- tidyxjh(c13_20160306mn1, xjh = "XJH")
c13_20160403jc <- tidyxjh(c13_20160403jc, xjh = "XJH")
c13_20160504mn2 <- tidyxjh(c13_20160504mn2, xjh = "XJH")

### >>Add XJH to **c13_gk** ----

# Left_join XJH from all source files to the target file
c13_gk <- c13_gk %>%
  left_join(select(c13_20140912yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20141008yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20141117qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150104yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150201qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150307yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150401yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150508qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150707qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20150928jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20151102qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20151217jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20160128qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20160223jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20160306mn1, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20160403jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_20160504mn2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c13_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c13_gk <- fillna(c13_gk)

#### Deal with Cases with Duplicate Values of Unique Identifier ----

# >>> Quick check for duplicate values in "cls_name"
# c13_gk %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
# dup_print(c13_gk, c13_20151217jc, xhv1 = "xh", xhv2 = "XH")

# Perform the replacement using function 'dup'
c13_gk <- dup(c13_gk, c13_20151217jc, xhv1 = "xh", xhv2 = "XH")

# Further make sure "ssid" is tidy
c13_gk <- tidyxjh(c13_gk, xjh = "ssid")

# Cohort 2014 ####

## Class Number Set 1 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c14_zk <- cui(c14_zk, bh = "BH", xm = "姓名")
c14_20141012yk <- cui(c14_20141012yk, bh = "BH", xm = "姓名") 
c14_20141121qz <- cui(c14_20141121qz, bh = "BH", xm = "姓名")
c14_20150105yk <- cui(c14_20150105yk, bh = "BH", xm = "姓名")
c14_20150205qm <- cui(c14_20150205qm, bh = "BH", xm = "姓名")
c14_20150305yk <- cui(c14_20150305yk, bh = "BH", xm = "姓名") # Target file
c14_20150427qz <- cui(c14_20150427qz, bh = "BH", xm = "姓名")
c14_20150612yk <- cui(c14_20150612yk, bh = "BH", xm = "姓名")
c14_20150705qm <- cui(c14_20150705qm, bh = "BH", xm = "姓名")
c14_base <- cui2(c14_base, bh = "bh1", xm = "xm") 

# Tidy XJH values in source files
c14_zk <- tidyxjh(c14_zk, xjh = "XJH")
c14_20141012yk <- tidyxjh(c14_20141012yk, xjh = "XJH")
c14_20141121qz <- tidyxjh(c14_20141121qz, xjh = "XJH")
c14_20150105yk <- tidyxjh(c14_20150105yk, xjh = "XJH") 
c14_20150205qm <- tidyxjh(c14_20150205qm, xjh = "XJH")
c14_20150427qz <- tidyxjh(c14_20150427qz, xjh = "XJH")
c14_20150612yk <- tidyxjh(c14_20150612yk, xjh = "XJH")
c14_20150705qm <- tidyxjh(c14_20150705qm, xjh = "XJH")
c14_base <- tidyxjh(c14_base, xjh = "zcxh")

### >>Add XJH to **c14_20150305yk** ----

# Left_join XJH from all source files to the target file
c14_20150305yk <- c14_20150305yk %>%
  left_join(select(c14_zk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20141012yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20141121qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20150105yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c14_20150205qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20150427qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20150612yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20150705qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c14_20150305yk <- fillna(c14_20150305yk)

# Further make sure "ssid" is tidy
c14_20150305yk <- tidyxjh(c14_20150305yk, xjh = "ssid")

## Class Number Set 2 ====

### Create Unique Identifier & Tidy XJH in Source Files ----

# Create unique identifiers in each exam file
c14_20150901yk <- cui(c14_20150901yk, bh = "BH", xm = "姓名")
c14_20151009yk <- cui(c14_20151009yk, bh = "BH", xm = "姓名") 
c14_20151112qz <- cui(c14_20151112qz, bh = "BH", xm = "姓名")
c14_20160127qm <- cui(c14_20160127qm, bh = "BH", xm = "姓名")
c14_20160403yk <- cui(c14_20160403yk, bh = "BH", xm = "姓名")
c14_20160510qz <- cui(c14_20160510qz, bh = "BH", xm = "姓名")
c14_20160607qm <- cui(c14_20160607qm, bh = "BH", xm = "姓名")
c14_20160830jc <- cui(c14_20160830jc, bh = "BH", xm = "姓名")
c14_20161008jc <- cui(c14_20161008jc, bh = "BH", xm = "姓名")
c14_20161116qz <- cui(c14_20161116qz, bh = "BH", xm = "姓名")
c14_20161215jc <- cui(c14_20161215jc, bh = "BH", xm = "姓名")
c14_20170114qm <- cui(c14_20170114qm, bh = "BH", xm = "姓名")
c14_20170211jc <- cui(c14_20170211jc, bh = "BH", xm = "姓名")
c14_20170305mn1 <- cui(c14_20170305mn1, bh = "BH", xm = "姓名")
c14_20170401jc <- cui(c14_20170401jc, bh = "BH", xm = "姓名")
c14_20170505mn2 <- cui(c14_20170505mn2, bh = "BH", xm = "姓名")
c14_20170525mn3 <- cui(c14_20170525mn3, bh = "BH", xm = "姓名")
c14_gk <- cui(c14_gk, bh = "班级", xm = "姓名") # Target file
c14_base <- cui2(c14_base, bh = "bh", xm = "xm")

# Tidy XJH values in source files
c14_20150901yk <- tidyxjh(c14_20150901yk, xjh = "XJH")
c14_20151009yk <- tidyxjh(c14_20151009yk, xjh = "XJH")
c14_20151112qz <- tidyxjh(c14_20151112qz, xjh = "XJH")
c14_20160127qm <- tidyxjh(c14_20160127qm, xjh = "XJH") 
c14_20160403yk <- tidyxjh(c14_20160403yk, xjh = "XJH")
c14_20160510qz <- tidyxjh(c14_20160510qz, xjh = "XJH")
c14_20160607qm <- tidyxjh(c14_20160607qm, xjh = "XJH")
c14_20160830jc <- tidyxjh(c14_20160830jc, xjh = "XJH")
c14_20161008jc <- tidyxjh(c14_20161008jc, xjh = "XJH")
c14_20161116qz <- tidyxjh(c14_20161116qz, xjh = "XJH")
c14_20161215jc <- tidyxjh(c14_20161215jc, xjh = "XJH")
c14_20170114qm <- tidyxjh(c14_20170114qm, xjh = "XJH")
c14_20170211jc <- tidyxjh(c14_20170211jc, xjh = "XJH")
c14_20170305mn1 <- tidyxjh(c14_20170305mn1, xjh = "XJH")
c14_20170401jc <- tidyxjh(c14_20170401jc, xjh = "XJH")
c14_20170505mn2 <- tidyxjh(c14_20170505mn2, xjh = "XJH")
c14_20170525mn3 <- tidyxjh(c14_20170525mn3, xjh = "XJH")

### >>Add XJH to **c14_gk** ----

# Left_join XJH from all source files to the target file
c14_gk <- c14_gk %>%
  left_join(select(c14_20150901yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20151009yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20151112qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c14_20160127qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20160403yk, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20160510qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20160607qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20160830jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20161008jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20161116qz, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20161215jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170114qm, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170211jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170305mn1, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170401jc, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170505mn2, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_20170525mn3, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c14_base, cls_name, ssid), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna'
c14_gk <- fillna(c14_gk)

# Further make sure "ssid" is tidy
c14_gk <- tidyxjh(c14_gk, xjh = "ssid")