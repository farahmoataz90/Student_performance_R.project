#import librares

#mode
install.packages("modeest")
library(modeest)
#create partition that divides data
install.packages("caret")
library(caret)
#decision tree
install.packages("rpart")
library(rpart)
#for plotting
install.packages("ggplot2")
library(ggplot2)

#read file 
#set empty cells to NULL 
students_data <- read.csv("C:/Users/dell/Downloads/Data Sets/StudentsPerformance.csv",na.strings="")

#To see number of rows and columns
dim(students_data)

#To see the columns name
names(students_data)

#To know the data type of each column 
str(students_data)

#cleaning the column sex
students_data$sex[students_data$sex == "Female"] <- 'F'
students_data$sex[students_data$sex == "Male"] <- 'M'

#check if there is any Null
any_null_values <- any(is.na(students_data))
print(any_null_values)


#filling the null values with the mode in column internet
mode = mfv(students_data$internet)
print(mode)
students_data$internet[is.na(students_data$internet)] <- mode

#no nulls now
any_null_values <- any(is.na(students_data))
print(any_null_values)

#to see if there is duplicated rows
dublicated_rows <- duplicated(students_data)
#count the dublicated rows
sum(dublicated_rows)

#To see summary about data
summary(students_data)


#plot the whole dataset except first column named "x"
plot(students_data[-c(1)])

#game3 anwa3 plots
plot(sort(students_data$age))

hist(students_data$goout, col = "lightblue", xlab = "go out",main="Times that students go out")

boxplot(students_data$G1,students_data$G2,students_data$G3, col = rainbow(13), ylab = "The Three Grades")


#replace the outliers 
replace_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - (1.5 * iqr)
  upper_bound <- q[2] + (1.5 * iqr)
  
  x[x <= lower_bound] <- q[1]  # Replace values below lower_bound with Q1
  x[x >= upper_bound] <- q[2]  # Replace values above upper_bound with Q3
  
  return(x)
}


#Replace outliers 

students_data$G2 <- replace_outliers(students_data$G2)
students_data$G3 <- replace_outliers(students_data$G3)


#plot between study time and failures columns
plot(students_data$studytime, students_data$failures, main = "Study Time VS Failure",xlim = c(0,5), ylim = c(0,4),  xlab = "study time", ylab = "failure")
#plot between failures and absences
plot(students_data$failures, students_data$absences, main = "Failure VS Absence" ,xlim = c(0,4), xlab = "failure", ylab = "absence")
#plot between health and absences
plot(students_data$health, students_data$absences, main = "Health VS Absence" , xlab = "health", ylab = "absence")



#linear algorithm
#To predict G3 using G1 and G2
#divide data into 80% train and 20% test
train_indices <- createDataPartition(students_data$G3, p = 0.8, list = FALSE)
train_data <- students_data[train_indices, ]
test_data <- students_data[-train_indices, ]
#predict G3 based on G1 and G2 
lm_model <- lm(students_data$G3 ~ students_data$G1 + students_data$G2)

#make predictions on the test set
predictions <- predict(lm_model, newdata = test_data)

#The output will be a value between 0 and 1, where a higher value indicates a better fit of the model to the data

rsquared <- summary(lm_model)$r.squared
print(paste("R-squared:", rsquared))



#Classification algorithm



#Create a new column for total score
students_data$total_score <- students_data$G1 + students_data$G2 + students_data$G3

#Set a threshold for pass/fail
threshold <- 35

#Create a new column for pass/fail based on the total score
students_data$status <- ifelse(students_data$total_score >= threshold, "Pass", "Fail")

#get number of pass and fail
table(students_data$status)

# store the Count of pass and fail
pass_fail_counts <- table(students_data$status)

# Compute percentages
pass_fail_percentages <- prop.table(pass_fail_counts) * 100

# Display the percentages
print(pass_fail_percentages)



#divide data into 80% train and 20% testing
train_indices2 <- createDataPartition(students_data$status, p = 0.8, list = FALSE)
train_data2 <- students_data[train_indices2, ]
test_data2 <- students_data[-train_indices2, ]

#Train a decision tree model based on these attributes to determine whether pass or fail

dt_model <- rpart(status ~ G1 + G2 + G3 + studytime + failures, data = train_data2, method = "class")

#Make predictions on the test data
predictions2 <- predict(dt_model, newdata = test_data2, type = "class")

#calculate accuracy

conf_matrix <- table(test_data2$status,predictions2)

# Convert the confusion matrix to a data frame
conf_matrix_df <- as.data.frame(as.table(conf_matrix))

# Rename the columns
names(conf_matrix_df) <- c("Actual", "Predicted", "Freq")

# Set the levels for the factors
conf_matrix_df$Actual <- factor(conf_matrix_df$Actual, levels = c("Fail", "Pass"))
conf_matrix_df$Predicted <- factor(conf_matrix_df$Predicted, levels = c("Fail", "Pass"))

# Plot using ggplot2
ggplot(data = conf_matrix_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 1) +  # Add text annotations
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Confusion Matrix",
       x = "Predicted",
       y = "Actual")


print(conf_matrix)

#calculate accuracy 
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(paste("Accuracy:", accuracy))

