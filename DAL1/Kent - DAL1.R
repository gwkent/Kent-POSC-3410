# Title: Kent - POSC-3410 #### # Author: Garrett Kent # Date: 01/18/2020
# Lesson 1 ####
# Pulp Fiction
#Lesson 2 ####
# Let's Try Some Basic Arithmetic ####
# Sum of 1 and 1
1+1
# Divide 365 by 12
365/12
# Your turn, multiply 10 by 12
10*12
# Your turn, add 28 + 38
28 + 38
# Order of Operations in R
1+1*(365/12)
# Six times six
6*6
# Nine divided by 3.142
9/3.142
# Learning to assign variables ####
#Run this line of code to assign your first variable.
myFirstVar<- "Hello world!"
# Run myFirstVar in the console.
myFirstVar
# Code a second variable with a number.
mySecondVar<-16
# Run myFirstVar in the console.
mySecondVar
#Now let's practice assigning variables.
# Assign "Clemson" to `home`
home <- "Clemson"
# Assign "Tigers" to `h_mascot`
h_mascot <- "Tigers"
# Assign "Ohio State" to `away
away <- "Ohio State"
# Assign 'Buckeyes' to `a_mascot`
a_mascot <- "Buckeyes"
# Assign 31 to ``
homeScore <- 31
# Assign 0 to `awayScore`
awayScore <- 0
# Assign TRUE to homeWin
homeWin<- TRUE
# Assign FALSE to awayWin
awayWin <- FALSE
# Use class to identify the data type for the following variables.
class(h_mascot)
class(homeScore)
class(homeWin)
# Use str to identify the data types for the following variables
str(h_mascot)
str(homeScore)
str(homeWin)
# Can sports scores ever be represented as decimals? No. We need to convert the `homeScore` and `awayScore# Convert `homeScore` to integer and assign to same var name.
homeScore <- as.integer(homeScore)
homeScore
# Now it is your turn.
# Convert `awayScore` to integer and assign to same var name.
awayScore <- as.integer(awayScore)
awayScore
# Make a numeric vector by using the syntax `c(#,#,#,#)`; check its structure.
vector_numeric <- c(12, 8, 16, 4, 15)
str(vector_numeric)
#Make a numeric vector, `myNumericVector`, on your own; check its structure.
myNumericVector <- c(13, 9, 17, 5, 16)
str(myNumericVector)
# Make a logical vector; check its structure.
vector_logical <- c(TRUE, TRUE, FALSE, T, F)
str(vector_logical)
# Make a logical vector, `myLogicalVector` on your own; check its structure.
myLogicalVector <- c(TRUE, FALSE, TRUE, FALSE, F)
str(myLogicalVector)
# Make a character vector; check it's structure.
vector_character <- c("Montana", "Aikman", "Manning", "Favre", "Mahomes")
str(vector_character)
# Make a character vector, `myCharVector` on your own; check its structure.
myCharVector <- c("Lawrence", "Etienne", "Ross", "Higgins", "Watson")
str(myCharVector)
# Make a list of the vectors that I created: drAllardList; check its structure.
drAllardList <- list(vector_numeric, vector_logical, vector_character)
str(drAllardList)
# Notice a few things about the list: (1) A list is made up of vectors with different types, (2) the list 
# Make a list of the vectors YOU created: myList; check its structure.
myList <- list(myNumericVector, myLogicalVector, myCharVector)
str(myList)
# Create data fame: QB_df; print in console; check structure
QB_df <- data.frame(vector_character, vector_numeric, vector_logical)
QB_df
str(QB_df)
# Note: It is a good practice to add '_df" to the names of objects that are data frames.
# print the numeric column to the console; use syntax: dataframe$columnName
QB_df$vector_numeric
# print the character column to the console; use syntax: dataframe$columnName
QB_df$vector_character
# Rename QB_df$vector_character as QB_df$qbName
names(QB_df)[names(QB_df)=="vector_character"] <- "qbName"
str(QB_df)
# What is going on here?
# Select the first row of QB_df
QB_df[1,]
# Select the first column of QB_df
QB_df[,1]
# Select the `qbName` column using ""
QB_df[,"qbName"]
# If you want to keep the complexity of the data frame, you can use the following formats.
QB_df[1]
QB_df["qbName"]
# It is important to know the difference between these approaches because we will use each for different 
# Select the cell that is at the intersection of the 3rd row and 2nd column
QB_df[3,2]
#Now it is your turn.
myData_df <- data.frame(myNumericVector, myLogicalVector, myCharVector)
# Select the first row of your dataframe
myData_df[1,]
# Select the 3rd column of your dataframe
myData_df[,3]
# Select the cell that is at the intersection of the 1st row and 2nd column of your data frame
myData_df[1,2]
# What type of data structure is returned by calling names(QB_df)?
names(QB_df)
# If we want to rename a specific element of the vector, then we need to use indexing to select the element
names(QB_df)[names(QB_df)=="vector_numeric"]
# Now we need to assign a new value to it.
names(QB_df)[names(QB_df)=="vector_numeric"] <- "jerseyNum"
# Repeat this process for the 3rd column: HoFer (Hall of Famer)
names(QB_df)[names(QB_df)=="vector_logical"] <-"HoFer"
