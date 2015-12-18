# Project for Getting and Cleaning Data

# R script called run_analysis.R does the following:
# 
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set.
# 4) Appropriately labels the data set with descriptive activity names.
# 5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject.



# Credits: this code is stronly inspired by the work of Benjamin Chan, 
# present at the url https://github.com/benjamin-chan/GettingAndCleaningData/tree/master/Project


# load required packages
# create a list of packages 
packages <- c("data.table", "reshape2")

# apply to each package the function require that load the package
sapply(packages, require, character.only=TRUE, quietly=TRUE)


# insert a flag to set if the data should be downloaded: it should be done only once.
download_data = FALSE


# set the address of the file
url.file <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# working dir
path.wd <- getwd()

# name of the data folder (if empty string or NULL data in the current directory)
dir.data <- ""

# path where data folder will be placed
path.data <- file.path(path.wd, dir.data)

# if the directory is set and does not exist, than create it
if(!nchar(dir.data) | !is.null(dir.data)){
        if (!file.exists(dir.data)){
                dir.create(dir.data)
        }
}

# name of the zip file containing the data
file.data <- "dataset.zip"

# full path of the data zip
data.fullpath <- file.path(path.data,file.data)

# if data were still not downloaded
if(download_data) {
        # download data
        download.file(url = url.file,destfile = data.fullpath)
        # unzip data
        unzip(data.fullpath,exdir=path.data)
}

# set the path where txt files will be searched
input.path <- file.path(path.data, "UCI HAR Dataset")

# get the list of files to be read
list.files(input.path, recursive=TRUE)


# read files of subject. use fread from read.table beacuse it is faster than read.table
dtSubjectTrain <- fread(file.path(input.path , "train", "subject_train.txt"))
dtSubjectTest  <- fread(file.path(input.path , "test" , "subject_test.txt" ))

# read files with activity
dtActivityTrain <- fread(file.path(input.path, "train", "y_train.txt"))
dtActivityTest  <- fread(file.path(input.path, "test" , "y_test.txt" ))


dtTrain <- fread(file.path(input.path, "train", "X_train.txt"))
dtTest <- fread(file.path(input.path, "test" , "X_test.txt" ))



# concatenate data tables
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtTrain, dtTest)


# merge columns
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
# set key
setkey(dt, subject, activityNum)

# extract mean and sd


# features.txt contains variables in dt for the mean and standard deviation.

dtFeatures <- fread(file.path(input.path, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))


# use regular expressions to subset only measurements for the mean and standard deviation.
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]



# convert the column numbers to a vector of variable names matching columns in dt.
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
# head(dtFeatures)
# dtFeatures$featureCode



# subset these variables using variable names.
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with=FALSE]



# use activity_labels.txt to add descriptive names.
dtActivityNames <- fread(file.path(input.path, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))




# merge activity labels.
dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)

# add activityName as a key.
setkey(dt, subject, activityNum, activityName)

# melt the data table to reshape it from a short and wide format to a tall and narrow format.
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))

# Merge activity name.

dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

# Create a new variable, activity that is equivalent to activityName as a factor class. Create a new variable, feature that is equivalent to featureName as a factor class.

dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

# Seperate features from featureName using the helper function grepthis.

grepthis <- function (regex) {
        grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

# Check to make sure all possible combinations of feature are accounted for by all possible combinations of the factor class variables.

r1 <- nrow(dt[, .N, by=c("feature")])
r2 <- nrow(dt[, .N, by=c("featDomain", "featAcceleration", "featInstrument", "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2



# Create a tidy data set

# Create a data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

# export the tidy data to a tab delimited text file
write.table(dtTidy, file.path(path.data,"dtTidy.txt"), sep="\t",row.names = FALSE) 
