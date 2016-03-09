# 0. Preparation steps
library(dplyr)
library(stringr)

folderpath <- 'change to your local directory'
setwd(folderpath)

activity_labels <- read.delim('activity_labels.txt', header=FALSE, sep="")
names(activity_labels) <- c('activity_id', 'activity_name')

features <- read.delim('features.txt', header=FALSE, sep="")
names(features) <- c('feature_id', 'feature_name')

x_train <- read.delim('train/X_train.txt', header=FALSE, sep="")
y_train <- read.delim('train/y_train.txt', header=FALSE, sep="")

x_test <- read.delim('test/X_test.txt', header=FALSE, sep="")
y_test <- read.delim('test/y_test.txt', header=FALSE, sep="")

fixname <- function(x) {
  sub( '^([a-zA-Z]+)-([a-zA-Z]+)\\(\\)-?([A-Z]+)?,?[0-9]?|([A-Z])?', '\\1\\2\\3', x, fixed=FALSE) %>% tolower()
}

# 1. Merges the training and the test sets to create one data set.
merged <- merge(x_train, x_test, all = TRUE)
names(merged) <- features$feature_name

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features.f1 <- filter(features, grepl('mean|std', features$feature_name))
merged.f1 <- merged[,features.f1$feature_id]

# 3. Uses descriptive activity names to name the activities in the data set
merged_activity_names <- rbind(y_train, y_test)
labels <- merge(merged_activity_names, activity_labels, by.y = 'activity_id', by.x = 'V1')
merged.f1_labeled <- cbind(labels$activity_name, merged.f1)

# 4. Appropriately labels the data set with descriptive variable names.
merged_names <- names(merged.f1_labeled)
merged_new_names <- lapply(merged_names, fixname)
merged_new_names[1] <- 'activityname'
names(merged.f1_labeled) <- merged_new_names

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata <- data.frame(merged_new_names, stringsAsFactors=FALSE)
tidydata <- tidydata[-1,]
names(tidydata) <- merged_new_names

walking <- filter(merged.f1_labeled, activityname == "WALKING")
datacolumns <- length(names(walking))
tidydata[1,1] <- 'WALKING'
for (i in 2:datacolumns) {
  tidydata[1,i] <- as.double ( mean(walking[,i]) )
}

walking_upstair <- filter(merged.f1_labeled, activityname == "WALKING_UPSTAIRS")
datacolumns <- length(names(walking_upstair))
tidydata[2,1] <- 'WALKING_UPSTAIRS'
for (i in 2:datacolumns) {
  tidydata[2,i] <- as.double ( mean(walking_upstair[,i]) )
}

walking_downstairs <- filter(merged.f1_labeled, activityname == "WALKING_DOWNSTAIRS")
datacolumns <- length(names(walking_downstairs))
tidydata[3,1] <- 'WALKING_DOWNSTAIRS'
for (i in 2:datacolumns) {
  tidydata[3,i] <- as.double ( mean(walking_downstairs[,i]) )
}

sitting <- filter(merged.f1_labeled, activityname == "SITTING")
datacolumns <- length(names(sitting))
tidydata[4,1] <- 'SITTING'
for (i in 2:datacolumns) {
  tidydata[4,i] <- as.double ( mean(sitting[,i]) )
}

standing <- filter(merged.f1_labeled, activityname == "STANDING")
datacolumns <- length(names(standing))
tidydata[5,1] <- 'STANDING'
for (i in 2:datacolumns) {
  tidydata[5,i] <- as.double ( mean(standing[,i]) )
}

laying <- filter(merged.f1_labeled, activityname == "LAYING")
datacolumns <- length(names(laying))
tidydata[6,1] <- 'LAYING'
for (i in 2:datacolumns) {
  tidydata[6,i] <- as.double ( mean(laying[,i]) )
}

write.csv(tidydata, file = 'tidydata.csv')