# setwd("../Desktop/Coursera R/course 3/week 4")
# Part 1
test.subject <- read.table("./dataset/test/subject_test.txt")
# dim(test.subject)
# 2947 1
test.set <- read.table("./dataset/test/X_test.txt")
# dim(test.set)
# 2947 561
test.label <- read.table("./dataset/test/y_test.txt")
# dim(test.label)
# 2947 1
train.subject <- read.table("./dataset/train/subject_train.txt")
# dim(train.subject)
# 7352 1
train.set <- read.table("./dataset/train/X_train.txt")
# dim(train.set)
# 7352 561
train.label <- read.table("./dataset/train/y_train.txt")
# dim(train.label)
# 7352 1
features <- read.table("./dataset/features.txt")
# dim(features)
# 561 2
# head(features)
activity <- read.table("./dataset/activity_labels.txt")

test <- cbind(test.subject, test.label, test.set)
# dim(test)
# 2947 563
train <- cbind(train.subject, train.label, train.set)
# dim(train)
# 7352 563
dataset <- rbind(test, train)
# dim(dataset)
# 10299 563
# dataset is the merged training and test sets in a single data frame.

# Part 2
# Identifying which columns have the word containing mean and std in them.
# The indices will need to be offsetted by 2 since the dataset is prepended with the subject and labels (1 column each).
cols.indices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
cols <- cols.indices + 2
# Extracts only columns containing mean and std from dataset.
dataset.new <- dataset[, cols]
# dim(dataset.new)
# 10299 66

# Part 3
# Extract the column which contains the label for test and train from dataset, matched against 
# corresponding activity label
# Combine mean and std data with activity labels
# Changed format of activity for easier readability
label <- dataset[, 2]
# Using regular expressions in gsub and the Perl extension, any underscore followed by the first letter is 
# replaced
# by the uppercase of the first letter match.
activity[, 2] <- gsub("_([A-z])", "\\U\\1", tolower(activity[, 2]), perl = TRUE)
label.descriptive <- activity[label, 2]
dataset.new <- cbind(label.descriptive, dataset.new)
# dim(dataset.new)
# 10299 67

# Part 4
# Using cols.indices (not prepended), match to corresponding variable names in features
# Changed variable names to make them more readable
# Assigned the variable names to the column names in dataset.new
variable.descriptive <- features[cols.indices, 2]
# Two step string replace process - change all hypens and following letter to the uppercase (removing hypen), 
# then remove parantheses
variable.descriptive <- gsub("-([A-z])", "\\U\\1",variable.descriptive, perl = TRUE)
variable.descriptive <- gsub("\\(\\)", "", variable.descriptive)
colnames(dataset.new) <- (c("activity", variable.descriptive))
# dim(dataset.new)
# 10299 67

# Part 5
# Added the column of subject names to the left of dataset.new
# Using dplyr's group_by and summarise_all function - stratified the dataset according to subject and activity,
# then applied the mean function over all other non-grouped columns (each of the variables), according to each stratum
dataset.new <- cbind(subject = dataset[, 1], dataset.new)
dataset.new.grouped <- dplyr::group_by(dataset.new, subject, activity)
data.mean <- as.data.frame(dplyr::summarise_all(dataset.new.grouped, .funs = mean))
# dim(data.mean)
# 180 68
write.table(data.mean, "tidy_data_average.txt")