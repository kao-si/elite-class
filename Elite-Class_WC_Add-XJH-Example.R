

library(readxl)
library(tidyverse)

c10_base <- read_excel("Raw-Data/2010/ZBYZ2010_Demographics&Grades.xlsx", col_types = "text", trim_ws = TRUE)

c10_20101110yk <- read_excel("Raw-Data/2010/G2010/2010年11月10日测试.xlsx")

c10_20110122qm <- read_excel("Raw-Data/2010/G2010/2011年1月22日期末.xlsx")

c10_20110428qz <- read_excel("Raw-Data/2010/G2010/2011年4月28日期中.xlsx")

c10_20110708qm <- read_excel("Raw-Data/2010/G2010/2011年7月8日期末.xlsx")

c10_20111104qz <- read_excel("Raw-Data/2010/G2010/2011年11月4日期中.xlsx")



# Add XJH Example - Cohort 2010 Class Number Set 1


## Functions ====


# Function that facilitates the creation of unique identifier

# The unique identifier is named "cls_name" and is made by combining a student's
# class number (bh) and name (xm)
cui <- function(df, bh, xm, uiv = "cls_name"){
  
  df[[uiv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  
  return(df)
}

# Function that facilitates the creation of unique identifier for base files with untidy names

cui2 <- function(df, bh, xm, uiv = "cls_name"){
  
  # remove numeric string and then extract the first 3 characters in name
  df[[xm]] <- substr(gsub("[0-9]", "", df[[xm]]), 1, 3)
  
  df[[uiv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  
  return(df)
}

# Function that replaces duplicate or incorrect values of XJH in source files with NAs 

# Number of digits of XJH in each cohort (n):
# 2003: 10
# 2004-2007: 12
# 2008-2014: 19

# xjh is the name of XJH variable in df
# n is the number of digits of XJH in a particular cohort
tidyxjh <- function(df, xjh, n){
  
  # capture the name of df
  df_name <- deparse(substitute(df))
  
  # get the number of XJH values that are being replaced
  m <- df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]], fromLast = TRUE) | nchar(df[[xjh]]) != n] %>% length()
  
  df[[xjh]][duplicated(df[[xjh]]) | duplicated(df[[xjh]], fromLast = TRUE) | nchar(df[[xjh]]) != n] <- NA
  
  cat("Number of XJH values being replaced in", df_name, "is", m, "\n")
  
  return(df)
}

# Function that performs checks on potential errors in XJH across the source files 

mis <- function(df){
  
  # create the output vector
  mismatch <- numeric(length = ncol(df) - 2)
  
  for(i in 3:ncol(df)){
    
    # get the number of mismatches between XJH of the first source file and XJH of the other source files
    mismatch[[i - 2]] <- which(df[[2]] != df[[i]]) %>% length()
  }
  
  return(mismatch)
}

# Function that corrects XJH values in source files with relatively more mismatched XJH values

# df is the file with relatively more mismatched XJH values
# xjh is the name of XJH variable in df
# check is the check file
# dfxjh is the name of XJH variable from df in check
# fxjh is the name of the first XJH variable in check
repmis <- function(df, xjh, check, dfxjh, fxjh){
  
  misdf <- dplyr::filter(check, !!rlang::sym(dfxjh) != !!rlang::sym(fxjh))
  
  df[[xjh]][match(misdf[[dfxjh]], df[[xjh]])] <- misdf[[fxjh]]
  
  return(df)
}

# Use repmis_print function to see all the cases with mismatched XJH values
repmis_print <- function(check, dfxjh, fxjh, uiv = "cls_name"){
  
  misdf <- dplyr::filter(check, !!rlang::sym(dfxjh) != !!rlang::sym(fxjh)) %>% 
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(fxjh), !!rlang::sym(dfxjh))
  
  print(misdf)
}

# Function that replaces NAs in XJH column extracted from the first source file
# with values in XJH columns extracted from all the other source files

fillna <- function(df, uiv = "cls_name"){
  
  # n is the position of "cls_name" column
  # n + 1 is the position of XJH extracted from the first source file
  # n + 2 is the position of XJH extracted from the second source file
  n <- which(colnames(df) == uiv)
  
  # check and print the number of NAs in XJH extracted from the first source file
  # before replacement
  m_before <- sum(is.na(df[[n + 1]]))
  
  cat("Number of NAs in XJH from the 1st source file before replacement:", m_before, "\n")
  
  for(i in (n + 2):ncol(df)){
    
    df[[n + 1]][which(is.na(df[[n + 1]]))] <- df[[i]][which(is.na(df[[n + 1]]))]
  }
  
  # check and print the number of NAs in XJH extracted from the first source file
  # after replacement
  m_after <- sum(is.na(df[[n + 1]]))
  
  cat("Number of NAs in XJH from the 1st source file after replacement:", m_after, "\n")
  
  # rename the focal XJH column to "ssid"
  colnames(df)[n + 1] <- "ssid"
  
  # remove the other extracted XJH columns
  df[(n + 2):ncol(df)] <- NULL
  
  return(df)
}

# Function that replaces incorrect XJH values in the target file that has
# duplicate values of the unique identifier

# df1 is the target file
# df2 is a chosen source file
# xhv1 is the name of the XH variable in df1 
# xhv2 is the name of the XH variable in df2
# xjhv1 is the name of the XJH variable in df1 ("ssid")
# xjhv2 is the name of the XJH variable in df2

# Use dup_print function to see if the target file has duplicate values in "cls_name"
# and if the chosen source file is appropriate
dup_print <- function(df1, df2, xhv1, xhv2, xjhv2, 
                      xjhv1 = "ssid", uiv = "cls_name"){
  
  # Capture the name of df1
  df1_name <- deparse(substitute(df1))
  
  # Capture the name of df2
  df2_name <- deparse(substitute(df2))
  
  df1_dup <- df1 %>% 
    dplyr::group_by(!!rlang::sym(uiv)) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1), !!rlang::sym(xjhv1)) %>% 
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv1))
  
  df2_dup <- df2 %>% 
    dplyr::group_by(!!rlang::sym(uiv)) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv2), !!rlang::sym(xjhv2)) %>% 
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv2))
  
  cat("Students with duplicate values in cls_name in the target file", df1_name, "\n")
  
  print(df1_dup)
  
  cat("Students with duplicate values in cls_name in the source file", df2_name, "\n")
  
  print(df2_dup)
  
}

dup <- function(df1, df2, xhv1, xhv2, xjhv2, 
                xjhv1 = "ssid", uiv = "cls_name"){
  
  # Capture the name of df1
  df1_name <- deparse(substitute(df1))
  
  df1_dup <- df1 %>% 
    dplyr::group_by(!!rlang::sym(uiv)) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1), !!rlang::sym(xjhv1))
  
  for (i in 1:nrow(df1_dup)){
    
    ui <- df1_dup[[i, 1]]
    
    xh <- df1_dup[[i, 2]]
    
    df1[[xjhv1]][df1[[uiv]] == ui & df1[[xhv1]] == xh] <- 
      df2[[xjhv2]][df2[[uiv]] == ui & df2[[xhv2]] == xh]
    
  }
  
  df1_dup_after <- df1 %>% 
    dplyr::group_by(!!rlang::sym(uiv)) %>% 
    dplyr::filter(n() > 1) %>% 
    dplyr::select(!!rlang::sym(uiv), !!rlang::sym(xhv1), !!rlang::sym(xjhv1)) %>% 
    dplyr::arrange(!!rlang::sym(uiv), !!rlang::sym(xhv1))
  
  cat("Students with duplicate values in cls_name in the target file", 
      df1_name, "after replacement\n")
  
  print(df1_dup_after)
  
  return(df1)
}


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Perform Step 1 & 2 for each class number set


## Step 1: Create Unique Identifier & Tidy XJH in Source Files ====

# Create unique identifiers in each exam file
c10_20101110yk <- cui(c10_20101110yk, bh = "BH", xm = "姓名")
c10_20110122qm <- cui(c10_20110122qm, bh = "BH", xm = "姓名")
c10_20110428qz <- cui(c10_20110428qz, bh = "BH", xm = "姓名")
c10_20110708qm <- cui(c10_20110708qm, bh = "BH", xm = "姓名") # Target file (without XJH)
c10_20111104qz <- cui(c10_20111104qz, bh = "BH1", xm = "姓名")
c10_base <- cui2(c10_base, bh = "bhx", xm = "xm")

# Tidy XJH values in source files
c10_20101110yk <- tidyxjh(c10_20101110yk, xjh = "XJH", n = 19) 
c10_20110122qm <- tidyxjh(c10_20110122qm, xjh = "XJH", n = 19)
c10_20110428qz <- tidyxjh(c10_20110428qz, xjh = "XJH", n = 19)
c10_20111104qz <- tidyxjh(c10_20111104qz, xjh = "XJH", n = 19)
c10_base <- tidyxjh(c10_base, xjh = "zcxh", n = 19)


## Step 2: Check XJH ====
## >>> DO NOT SAVE THIS SECTION IN R SCRIPT 

# Double confirm that XJH values are consistent across all source files

# Gather XJH from all source files using full_join
check1001 <- select(c10_20101110yk, cls_name, XJH) %>% 
  full_join(select(c10_20110122qm, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>%
  full_join(select(c10_20110428qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  full_join(select(c10_20111104qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  full_join(select(c10_base, cls_name, zcxh), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the check using function 'mis'
mis(check1001)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## In this case, turns out that the file "c10_20110122qm" contains incorrect XJH

## ACTIONS:
## 1. Exclude "c10_20110122qm" as a valid source file and treat it as another target file to be processed
## 2. Update README file (add "$" before "c10_20110122qm")

## ALTERNATIVELY: (when the mismatched XJH values are below around 10%)
## Deal with the `repmis` function and continue to treat the file "c10_20110122qm" as a source file

repmis_print(check1001, "XJH.y", "XJH.x")

c10_20110122qm <- repmis(c10_20110122qm, "XJH", check1001, "XJH.y", "XJH.x")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Perform Step 3 - 5 for each target file


## Step 3: Add XJH ====

# Left_join XJH from all source files to the target file
c10_20110708qm <- c10_20110708qm %>% 
  left_join(select(c10_20101110yk, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20110428qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20111104qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_base, cls_name, zcxh), by = "cls_name", na_matches = "never", multiple = "any")

# Perform the function 'fillna', which will
# (1) keep only the XJH values extracted from the first source file above 
# (2) fill NAs in that column using XJH values from other XJH columns extracted from other source files
# (3) rename the remaining XJH column as "ssid"
c10_20110708qm <- fillna(c10_20110708qm)


## Step 4: Duplicate Values in Unique Identifier ====

# >>> Quick check for duplicate values in "cls_name"
c10_20110708qm %>% count(cls_name) %>% filter(n > 1)

# >>> Make sure the chosen source file is appropriate using function `dup_print`
dup_print(c10_20110708qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH", xjhv2 = "XJH")

# Perform the replacement using function 'dup'
c10_20110708qm <- dup(c10_20110708qm, c10_20101110yk, xhv1 = "XH", xhv2 = "XH", xjhv2 = "XJH")


## Step 5: Check the Completed Target File ====
## >>> DO NOT SAVE THIS SECTION IN R SCRIPT
 
c10_20110708qm %>% select(cls_name, ssid, 1:15) %>%
  group_by(BH) %>% 
  slice(1:5) %>% 
  print(n = Inf)
