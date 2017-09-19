run_analysis <- function() {
    
    library(dplyr)
    
    #############################################################################
    # Merging together all files to create one dataset with all data
    #############################################################################
    
    #Read in all data files - subject and activity files both marked as factors
    features <- read.table("./UCI HAR Dataset/features.txt")
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", colClasses = 'factor')
    X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", colClasses = 'factor')
    
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", colClasses = 'factor')
    X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", colClasses = 'factor')
    
    
    
    #match feature labels vector to the column names of both X datasets
    #rename subject and y datasets to be more descriptive
    colnames(subject_test) <- 'subject'
    colnames(subject_train) <- 'subject'
    colnames(X_test) <- features[,2]
    colnames(X_train) <- features[,2]
    colnames(y_test) <- 'activity'
    colnames(y_train) <- 'activity'
    
    #filter out columns from X's that don't have mean() or std()
    #the \\ allows R to read parentheses literally instead of as part of regular expression
    X_test <- X_test[,which(grepl('mean\\(\\)|std\\(\\)', names(X_test)))]
    X_train <- X_train[,which(grepl('mean\\(\\)|std\\(\\)', names(X_train)))]
    
    
    #bind all test and all train columns together
    merged_test <- cbind(subject_test, y_test, X_test)
    merged_train <- cbind(subject_train, y_train, X_train)
    
    #bind the rows from test and train together to form the full total dataset
    total_data <- rbind(merged_test, merged_train)
    
    #rename labels in activity to be more descriptive
    #convert to character to change and then back to factor
    total_data$activity <- as.character(total_data$activity)
    total_data[,'activity'][total_data[,'activity'] == 1] <- 'walking'
    total_data[,'activity'][total_data[,'activity'] == 2] <- 'walking_up'
    total_data[,'activity'][total_data[,'activity'] == 3] <- 'walking_down'
    total_data[,'activity'][total_data[,'activity'] == 4] <- 'sitting'
    total_data[,'activity'][total_data[,'activity'] == 5] <- 'standing'
    total_data[,'activity'][total_data[,'activity'] == 6] <- 'laying'
    total_data$activity <- as.factor(total_data$activity)
    
    #############################################################################
    # Now create tidy'summarized' dataset which has the mean of each variable for 
    # each activity and each subject.
    #############################################################################
    
    #convert to table to use dplyr package
    tbl_total_data <- tbl_df(total_data)
    
    #group by subject and activity
    tbl_total_data <- group_by(tbl_total_data, subject, activity)
    
    #use summarize all to apply mean on each variable for each group
    summary <- summarize_all(tbl_total_data, funs(mean))
    
    #sort by subject ID, must first conver to integer
    summary$subject <- as.integer(summary$subject)
    summary <- arrange(summary, subject, activity)
    
    #convert subject ID back to factor and then convert whole table back to dataframe
    summary$subject <- as.factor(summary$subject)
    summary <- as.data.frame(summary)
    
    #returns list that includes both the total data set and summarized one
    list(total_data, summary)
    
}