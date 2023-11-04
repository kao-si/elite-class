

library(tidyverse)


# Functions ####


# Function that facilitates the creation of unique identifier
cui <- function(df, bh = "BH", xm = "姓名", nv = "cls_name"){
  df[[nv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  return(df)
}

# Function that facilitates the creation of unique identifier for base files with untidy names
cui2 <- function(df, bh, xm, nv = "cls_name"){
  # Remove numeric string and then extract the first 3 characters in name
  df[[xm]] <- substr(gsub("[0-9]", "", df[[xm]]), 1, 3)
  df[[nv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  return(df)
}

# Function that performs checks on potential errors in XJH across the source files 
mis <- function(df){
  
  # create the output vector
  mismatch <- numeric(length = ncol(df) - 2)
  
  for(i in 3:ncol(df)){
    
    for(j in 1:length(mismatch)){
      
      # get the number of mismatches between XJH of the first source file and XJH of the other source files
      mismatch[[j]] <- which(df[[2]] != df[[i]]) %>% length()
      
      return(mismatch)
    }
  }
}

# Function that replaces NAs in XJH extracted from the first source file
# with values in XJH extracted from all the other source files
fillna <- function(df, nv = "cls_name"){
  
  # n is the position of "cls_name" column
  n <- which(colnames(df) == nv)
  
  # n + 1 is the position of XJH extracted from the first source file
  # n + 2 is the position of XJH extracted from the second source file
  for(i in (n + 2):ncol(df)){
    usecol <- df[[i]]
    df[[n + 1]][which(is.na(df[[n + 1]]))] <- usecol[which(is.na(df[[n + 1]]))]
  }
  return(df)
}

# Function that replaces incorrect XJH in the target file that has
# duplicate values of unique identifier

# df1 is the target file
# df2 is the source file
# ui is the value of the unique identifier
# xh is the XH of the student with an incorrect XJH
dup <- function(df1, df2, ui, xh, 
                xhv1 = "XH", xhv2 = "XH", xjhv1 = "XJH", xjhv2 = "XJH", uiv = "cls_name"){
  df1[[xjhv1]][df1[[uiv]] == ui & df1[[xhv1]] == xh] <- 
    df2[[xjhv2]][df2[[uiv]] == ui & df2[[xhv2]] == xh]
  return(df1)
}


# $$ ANY IMPORTANT NOTE $$


# Cohort 2010 ####


## Class Number Set 1 ====


### Create Unique Identifier in Every Exam Files ----

### >>Add XJH to **c10_20110122qm** ----

#### Cases with Duplicate Values of Unique Identifier ----

### >>Add XJH to **c10_20110708qm** ----

#### Cases with Duplicate Values of Unique Identifier ----
