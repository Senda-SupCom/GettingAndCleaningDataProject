## First verifying that the required libraries get installed if needed
verify_deps <- function(...) {
  lapply(list(...), function(lib) {
    if (!lib %in% installed.packages()) 
      install.packages(lib)
  })
}

## Verify dependencies
verify_deps("dplyr", "reshape2")

## Load the dependencies
library("dplyr")
library("reshape2")

## The main data directory
dir <- "UCI HAR Dataset"

## Loading the files
### filename: name of the file to be loaded
load_file <- function(filename, ...) {
  file.path(..., filename) %>%
  read.table(header = FALSE)
}

## Loads a training file
load_training_file <- function(filename) {
  load_file(filename,dir, "train")
}

## Loads a test file
load_testing_file <- function(filename) {
  load_file(filename, dir, "test")
}

## Uses list of activity values to describe the test and training labels
## DS:label dataset
## output: the original dataset with readable column name and values
describe_label_DS <- function(DS) {
  names(DS) <- activity_col  
  DS$Activity <- factor(DS$Activity, levels = activity_label$V1, labels = activity_label$V2)
  DS
}

## Takes a dataset capturing results of feature tests and associates columns with individual features
## Params: ds .. activity dataset
## Returns: the original dataset with columns indicating which feature it describes
describe_activity_DS <- function(DS) {
  col_names <- gsub("-", "_", features$V2)
  col_names <- gsub("[^a-zA-Z\\d_]", "", col_names)
  names(DS) <- make.names(names = col_names, unique = TRUE, allow_ = TRUE)
  DS
}

## Adjusts column name in the dataset 
describe_sub_DS <- function(DS) {
  names(DS) <- subject_col
  DS
}

## Download and extract a zip file 
if (!file.exists(dir)) {
  URLsource <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  destination_file <- "Dataset.zip"
  download.file(URLsource, destfile = destination_file)
  unzip(destination_file)
  if (!file.exists(dir)) 
    stop("The downloaded dataset doesn't have the expected structure!")
}

## Custom columns
subject_col <- "Subject"
activity_col <- "Activity"

## Load features as a readable ones
features <- load_file("features.txt", dir)

## Load activity labels
activity_label <- load_file("activity_labels.txt", dir)

## Use descriptive activity names for the activities in the dataset
#### Training data
train_set <- load_training_file("X_train.txt") %>% describe_activity_DS
train_label <- load_training_file("y_train.txt") %>% describe_label_DS
train_sub <- load_training_file("subject_train.txt") %>% describe_sub_DS

#### Test data
test_set <- load_testing_file("X_test.txt") %>% describe_activity_DS
test_label <- load_testing_file("y_test.txt") %>% describe_label_DS
test_sub <- load_testing_file("subject_test.txt") %>% describe_sub_DS

## Merge the training and the test sets to create one dataset (Extract only the measurements on the mean and standard deviation)
merge_data <- rbind(
                cbind(train_set, train_label, train_sub),
                cbind(test_set, test_label, test_sub)
              ) %>%
              select(
                matches("mean|std"), 
                one_of(subject_col, activity_col)
              )

## here we create a second and independent tidy data set using the average of each variable for each activity and each subject
ID_cols <- c(subject_col, activity_col)
tidy_data <- melt(
               merge_data, 
               id = ID_cols, 
               measure.vars = setdiff(colnames(merge_data), ID_cols)
             ) %>%
             dcast(Subject + Activity ~ variable, mean)
             
## Save the result as a text and csv files
###save as 'txt' 
write.table(tidy_data, file = "tidy_data.txt", sep = ",",row.name= FALSE)

###save as 'csv' 
write.table(tidy_data, file = "tidy_data.csv", sep = ",",row.name= FALSE)
