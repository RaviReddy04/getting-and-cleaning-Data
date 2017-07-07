#download url 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- file.path(getwd(),"dataset.zip")

#check if the file is already downloadedor not
if(!exists(f)){download.file(url, f)}

#Unzip the dataset
fileNames<- unzip(f,exdir =getwd(),list = TRUE)
#dataDir <- file.path(getwd(), "UCI HAR Dataset")

#read xtest and xtrain
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")

#bind xtest and xtrain
x <- rbind(X_test,X_train)

#add labels to the x table
featureNames <- read.table("UCI HAR Dataset/features.txt")[,2]
names(x) <- featureNames

#get only mean and std column
matches <- grep("(mean|std)", featureNames)
limited <- x[,matches]

#read ytest and ytrain
yTrain <- read.table("UCI HAR Dataset/train/y_train.txt")
yTest <- read.table("UCI HAR Dataset/test/y_test.txt")

#merge ytrain and ytest
yMerged <- rbind(yTrain, yTest)[, 1]

#add labels to yMerged 
activityNames <- c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying")
activities <- activityNames[yMerged]



names(limited) <- gsub("^t", "Time", names(limited))
names(limited) <- gsub("^f", "Frequency", names(limited))
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited))
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited))
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))


# Add activities and subject with nice names
subjectTrain <- read("train/subject_train.txt")
subjectTest  <- read("test/subject_test.txt")
subjects <- rbind(subjectTrain, subjectTest)[, 1]

tidy <- cbind(Subject = subjects, Activity = activities, limited)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject.
library(plyr)
# Column means for all but the subject and activity columns
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) }
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans)
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])

# Write file
write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE)


tidyMeans

