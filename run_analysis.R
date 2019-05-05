# run_analysis performs the 5 tasks set forth in the course Project Instructions
# Code performs actions to merge the data into one data set, extract the important values and summarize everything into a tidy data table
# Read in each of the data files provided
#
# Read in the Test Data Section (30%)
subject_test <- read.table("subject_test.txt", quote="\"", comment.char="")
y_test <- read.table("y_test.txt", quote="\"", comment.char="")
X_test <- read.table("X_test.txt", quote="\"", comment.char="")
#
# Read in the Training Data Section (70%)
subject_train <- read.table("subject_train.txt", quote="\"", comment.char="")
y_train <- read.table("y_train.txt", quote="\"", comment.char="")
X_train <- read.table("X_train.txt", quote="\"", comment.char="")
#
# Using the descriptions in the features file to rename the columns of the test and training sets according to the name of their measurement
features <- read.table("features.txt", quote="\"", comment.char="")
#
#Rename the column names of the training and test sets by the values in the features data set
colnames(X_train)=features[,2]
colnames(X_test)=features[,2]
#
# Read in the descriptions of the Activities
activity_labels <- read.table("activity_labels.txt", quote="\"", comment.char="")
#
# label the activity variable in the data set with it's name (ex activity 1 = "Walking"...)
# This Satisfies Criteria 3 of the assignment
y_test[y_test==1]<-"WALKING"
y_test[y_test==2]<-"WALKING_UPSTAIRS"
y_test[y_test==3]<-"WALKING_DOWNSTAIRS"
y_test[y_test==4]<-"SITTING"
y_test[y_test==5]<-"STANDING"
y_test[y_test==6]<-"LAYING"
y_train[y_train==1]<-"WALKING"
y_train[y_train==2]<-"WALKING_UPSTAIRS"
y_train[y_train==3]<-"WALKING_DOWNSTAIRS"
y_train[y_train==4]<-"SITTING"
y_train[y_train==5]<-"STANDING"
y_train[y_train==6]<-"LAYING"
#
# Label the 2nd column Name Activity to describe the 6 activities total
colnames(y_train)="Activity"
colnames(y_test)="Activity"
#
# Label the 1st column Name to describe the Subject (30 subjects total)
colnames(subject_train)="Subject"
colnames(subject_test)="Subject"
#
# Combine the Training Data Files into one Training Data Set with Columns Properly named
Whole_Training_Set=cbind(subject_train,y_train,X_train)
#
# Combine the Test Data Files into one  Test Data Set with Columns Properly named
Whole_Test_Set=cbind(subject_test,y_test,X_test)
#
# Combine the Test Data set and Training Data Set into one Complete Data Set with All Columns named  by Subject, Activity, and Measurement Name
# This Satisfies Criteria 1 of the assignment and Criteria 4 of the assignment
Whole_Data_Set=rbind(Whole_Test_Set,Whole_Training_Set)
#
# Extract the mean and standard deviation values of the Whole Data Set
values_mean=grep("mean",colnames(Whole_Data_Set))
#
#some values have "meanFreq" and we want to remove them from the data frame
#
values_meanFreq=grep("Freq",colnames(Whole_Data_Set))
#
#Keep the elements in values_mean that don't have "meanFreq" in the description
values_mean=values_mean[!values_mean %in% c(values_meanFreq)]
#
values_std=grep("std",colnames(Whole_Data_Set))
#
#combine columns for mean and standard deviation of the measurements and keep the original order
#
important_values=sort(c(values_mean,values_std))
#
#add the subject and activity to the important values
#
important_values=c(1,2,important_values)
#
#Extract the Subject,Activity,mean, and standard deviation columns from the whole data set and create a Partial Data Set
#This Satisfies Criteria 2 of the assignment
Partial_Data_Set=Whole_Data_Set[,important_values]
#
#Sort the Values of the Partial Data Set first by Subject and then by Activity for easier reading
Partial_Data_Set_Sorted=arrange(Partial_Data_Set,Subject,Activity)
#
#Create the final tidy data set with the average of each variable for each activity and each subject
# This Satisfies Criteria 5 of the assignment
Tidy_Data_Set=Partial_Data_Set_Sorted %>% group_by(Subject, Activity) %>% summarize_each(funs(mean= mean(., na.rm=TRUE)))
#
#Create .csv files in the working directory of the user 
write.csv(Whole_Data_Set, file = "Whole_Data_Set.csv")
write.csv(Partial_Data_Set, file = "Partial_Data_Set.csv")
write.csv(Partial_Data_Set_Sorted, file = "Partial_Data_Set_Sorted.csv")
write.csv(Tidy_Data_Set, file = "Tidy_Data_Set.csv")
# Create Tidy Data set in .txt format for review by the grader
write.table(Tidy_Data_Set, file="Tidy_Data_Set.txt",row.names=FALSE)
