## Read in the data files:

x_test <- read.table("x_test.txt")
subject_test <- read.table("subject_test.txt")
y_test <- read.table("y_test.txt")
x_train <- read.table("x_train.txt")
subject_train <- read.table("subject_train.txt")
y_train <- read.table("y_train.txt")

## Join the train and test files, and append both into one file:
test_temp <- bind_cols(subject_test, y_test)
test_data <- bind_cols(test_temp, x_test)
train_temp <- bind_cols(subject_train, y_train)
train_data <- bind_cols(train_temp, x_train)
data <- bind_rows(test_data, train_data)

## Assign sensical column names from features.txt to the data
features <- read.table("features.txt", colClasses = c(rep("NULL", 1), rep("character",1)), header = FALSE)
names <- features[,1]
colnames(data) <- names

## Extract the columns with mean() in column name:
data_condensed <- data[ , grepl( "mean\\(\\)" , names( data_test[,3:563]))]

## Run a simple function to assign activity names in place of activity numbers:
act_name <- function(data) {

## read in a data frame. for each value 1-6 in column 2 (activity),
## substitute in the activity name:
## 1 WALKING
## 2 WALKING_UPSTAIRS
## 3 WALKING_DOWNSTAIRS
## 4 SITTING
## 5 STANDING
## 6 LAYING

rows <- nrow(data)   ## get number of observations
n <- 1               ## set up row counter

while (n <= rows) {
        if (data[n,2] == 1) {data[n,2] <- "WALKING"}
        if (data[n,2] == 2) {data[n,2] <- "WALKING_UPSTAIRS"}
        if (data[n,2] == 3) {data[n,2] <- "WALKING_DOWNSTAIRS"}
        if (data[n,2] == 4) {data[n,2] <- "SITTING"}
        if (data[n,2] == 5) {data[n,2] <- "STANDING"}
        if (data[n,2] == 6) {data[n,2] <- "LAYING"}
        
        n <- n + 1
        }
assign('data',data,envir=.GlobalEnv)
}

## Run the function -
act_name(data_condensed)
data_cond_act <- data

## Convert data_cond_act to a table for dplyr friendliness
final_data <- tbl_df(data_cond_act)

## Group and summarize 
output <- final_data %>% group_by(Subject, Activity) %>% summarize_all(funs(mean))

## Save the output to a .csv files
write.csv(output, file="Results.csv")
