
library(dplyr)

setwd("../UCI HAR Dataset/train")

#1. Merges the training and the test sets to create one data set.

# Training dataset

subject1 <- read.table("subject_train.txt", col.names = "id")
activity1 <- read.table("y_train.txt", col.names = "activity")
values1 <- read.table("X_train.txt")

train <- cbind(subject1, activity1, values1) %>% mutate(group = "train")

# Test dataset

setwd("../test")

subject2 <- read.table("subject_test.txt", col.names = "id")
activity2 <- read.table("y_test.txt", col.names = "activity")
values2 <- read.table("X_test.txt")

test <- cbind(subject2, activity2, values2) %>% mutate(group = "test")

# Combine training and test datasets

full <- rbind(train, test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 

# Getting the names of the features

setwd("..")

features <- read.table("features.txt")
features$V1 <- paste0("V", features$V1)

# Get the features with "mean()" and "std()" in their names

index <- grep("mean\\()|std\\()", features$V2, ignore.case = T)
features_mean_std <- features$V1[index]

# Retain only features with mean and std in the full dataset

full2 <- full %>% select(id, activity, group, features_mean_std)


#3. Uses descriptive activity names to name the activities in the data set

activity_name <- read.table("activity_labels.txt")

full3 <- left_join(full2, activity_name, by = c("activity" = "V1")) %>% select(-activity) %>%
  rename(activity = V2.y, V2 = V2.x) %>% select(id, group, activity, V1:V543)


#4. Appropriately labels the data set with descriptive variable names. 

# Cleaning up the features name

features$V2[index] <- gsub("-|\\(|\\)", "", features$V2[index])
features$V2[index] <- gsub("mean", "Mean", features$V2[index])
features$V2[index] <- gsub("std", "Std", features$V2[index])
features$V2[index] <- gsub("^t", "time", features$V2[index])
features$V2[index] <- gsub("^f", "freq", features$V2[index])

colnames(full3)[4:69] <- features$V2[index]


#5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

tidy <- full3 %>% group_by(id, activity, group) %>% summarise_all(mean)

write.table(tidy, "tidy.txt",row.name=FALSE)



