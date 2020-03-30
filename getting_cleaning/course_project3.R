activityLabels = read.table("UCI HAR Dataset/activity_labels.txt")
activityLabels[,2] = as.character(activityLabels[,2])
features = read.table("UCI HAR Dataset/features.txt")
features[,2] = as.character(features[,2])

featuresdivide = grep(".*mean.*|.*std.*", features[,2])
featuresdivide.names = features[featuresdivide,2]
featuresdivide.names = gsub('-mean', 'Mean', featuresdivide.names)
featuresdivide.names = gsub('-std', 'Std', featuresdivide.names)
featuresdivide.names = gsub('[-()]', '', featuresdivide.names)


train = read.table("UCI HAR Dataset/train/X_train.txt")[featuresdivide]
trainActivities = read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects = read.table("UCI HAR Dataset/train/subject_train.txt")
train = cbind(trainSubjects, trainActivities, train)

test = read.table("UCI HAR Dataset/test/X_test.txt")[featuresdivide]
testActivities = read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects = read.table("UCI HAR Dataset/test/subject_test.txt")
test = cbind(testSubjects, testActivities, test)


traintest = rbind(train, test)
colnames(traintest) = c("subject", "activity", featuresdivide.names)


traintest$activity = factor(traintest$activity, levels = activityLabels[,1], labels = activityLabels[,2])
traintest$subject = as.factor(traintest$subject)

traintest.mel = melt(traintest, id = c("subject", "activity"))
traintest.mean = dcast(traintest.mel, subject + activity ~ variable, mean)

write.table(traintest.mean, "tidy.txt", row.names = FALSE, quote = FALSE)