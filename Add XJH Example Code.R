

# Add XJH Example - Cohort 2010


## Step 1: Unique Identifier ====

# For each class number set within each cohort, create an unique identifier
# (班号_姓名) in each exam file


# Create a function to facilitate creating unique identifier
cui <- function(df, bh = "BH", xm = "姓名", nv = "cls_name"){
  df[[nv]] <- paste(df[[bh]], df[[xm]], sep = "_")
  return(df)
}

# Create unique identifiers in the exam files
c10_20101110yk <- cui(c10_20101110yk) # Source file (with XJH)
c10_20110122qm <- cui(c10_20110122qm)
c10_20110428qz <- cui(c10_20110428qz)
c10_20110708qm <- cui(c10_20110708qm) # Target file (without XJH)
c10_20111104qz <- cui(c10_20111104qz, bh = "BH1")


## Step 2: Check XJH ====

# Double confirm that XJH is correct across all source files


# Gather XJH of all source files
check1 <- select(c10_20101110yk, cls_name, XJH) %>% 
  left_join(select(c10_20110122qm, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20110428qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20111104qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any")

# Create a function to perform the check on potential errors in XJH across the source files 
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

# Perform the check using function 'mis'
mis(check1)

# In this case, turns out that the file 'c10_20110122qm' contains incorrect XJH

# Action: Exclude 'c10_20110122qm' as a valid source file and treat it as another target file,
# and also update README file


# <<<<<< Perform steps 1-2 for each class number set >>>>>> 


## Step 3: Add XJH ====


# Left_join XJH of all source files to the target file
c10_20110708qm <- c10_20110708qm %>% 
  left_join(select(c10_20101110yk, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>%
  left_join(select(c10_20110428qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any") %>% 
  left_join(select(c10_20111104qz, cls_name, XJH), by = "cls_name", na_matches = "never", multiple = "any")

# Create a function to replace NAs in XJH extracted from the first source file
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

# Get the positions of relevant XJH columns for checks and later operations
# To return column names, use 'value = TRUE'
grep("XJH", colnames(c10_20110708qm))

# Check number of NAs in XJH extracted from the first source file before replacement
sum(is.na(c10_20110708qm[84]))

# Perform the replacement using function 'fillna'
c10_20110708qm <- fillna(c10_20110708qm)

# Check number of NAs in XJH extracted from the first source file after replacement
sum(is.na(c10_20110708qm[84]))

# Rename the focal XJH column
colnames(c10_20110708qm)[84] <- "XJH"

# Remove the other XJH columns
c10_20110708qm[c(2, 85, 86)] <- NULL


## Step 4: Duplicate Values in Unique Identifier ====


# Check for duplicate values in "cls_name"
c10_20110708qm %>% count(cls_name) %>% filter(n > 1)

# Input correct XJH for these cases (e.g., cls_name == "26_张帅")

# Extract the XHs of the students and their correct XJHs from one source file
c10_20110708qm %>% filter(cls_name == "26_张帅") %>% select(cls_name, XH, XJH)

c10_20101110yk %>% filter(cls_name == "26_张帅") %>% select(cls_name, XH, XJH)

# Create a function to replace incorrect XJH in the target file
# df1 is the target file
# df2 is the source file
# ui is the value of the unique identifier
# xh is the XH of the student with an incorrect XJH
dup <- function(df1, df2, ui, xh, xhv = "XH", uiv = "cls_name", xjhv1 = "XJH", xjhv2 = "XJH"){
  df1[[xjhv1]][df1[[uiv]] == ui & df1[[xhv]] == xh] <- 
    df2[[xjhv2]][df2[[uiv]] == ui & df2[[xhv]] == xh]
  return(df1)
}

# Perform the replacement using function 'dup'
c10_20110708qm <- dup(c10_20110708qm, c10_20101110yk, "26_张帅", 33)

# Double check to confirm the change
c10_20110708qm %>% filter(cls_name == "26_张帅") %>% select(cls_name, XH, XJH)


## Step 5: Check the Completed Target File ====


c10_20110708qm %>% select(cls_name, XJH, 1:15) %>%
  group_by(BH) %>% 
  slice(1:5) %>% 
  print(n = Inf)


# <<<<<< Perform steps 3-5 for each target file >>>>>>

