# Data manipulation and analysis
install.packages("dplyr")
install.packages("tidyr")
install.packages("data.table")

# Data visualization
install.packages("ggplot2")
install.packages("ggmap")

# Machine learning and modeling
install.packages("caret")
install.packages("randomForest")
install.packages("xgboost")

# Geospatial analysis (if applicable)
install.packages("sf")
install.packages("spdep")

# Reporting and document generation
install.packages("rmarkdown")
install.packages("knitr")

# Neural networks (if needed)
install.packages("nnet")

# Support vector machines (if needed)
install.packages("e1071")

# Regression diagnostics (if needed)
install.packages("car")

# Regularized regression (if needed)
install.packages("glmnet")

# Data manipulation and analysis
library(dplyr)
library(tidyr)
library(data.table)

# Data visualization
library(ggplot2)
library(ggmap)

# Machine learning and modeling
library(caret)
library(randomForest)
library(xgboost)

# Geospatial analysis (if applicable)
library(sf)
library(spdep)

# Reporting and document generation
library(rmarkdown)
library(knitr)

# Neural networks (if needed)
library(nnet)

# Support vector machines (if needed)
library(e1071)

# Regression diagnostics (if needed)
library(car)

# Regularized regression (if needed)
library(glmnet)


setwd("C:/Users/Haroon Muhammad Arif/Desktop/BCU/Applied Statistics/StatsProject")

datasett <- read.csv("stroke_datasett.csv")

#initial data exploration 
head(datasett)
str(datasett)


#checking for missing values
any(is.na(datasett))

#checking for missing values in columns
colSums(is.na(datasett))

#unique values, and spelling checks
unique_bmi <- unique(datasett$bmi)
print(unique_bmi)

unique_Residence <- unique(datasett$Residence_type)
print(unique_Residence)

unique_smoking <- unique(datasett$smoking_status)
print(unique_smoking)

unique_gender <- unique(datasett$gender)
print(unique_gender)

unique_work_type <- unique(datasett$work_type)
print(unique_work_type)


unique_hemisphere <- unique(dataset$Hemisphere)
print(unique_hemisphere)

unique_BloodPressure <- unique(dataset$BloodPressure)
print(unique_BloodPressure)

#duplicate in Patient ID column
duplicate <- dataset$`Patient ID`[duplicated(dataset$`Patient ID`)]
print(duplicate)

# Export the new dataset to a CSV file
write.csv(new_data, file = "strokee_datasett.csv", row.names = FALSE)


#Descriptive Statistics:

# What is the summary statistics of age, average glucose level, and BMI?

summary(datasett$avg_glucose_level)
summary(datasett$bmi)
summary(datasett$age)

detailed_summary <- describe(datasett)
print(detailed_summary)

ggplot(datasett, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age",
       x = "Age",
       y = "Count")

#What is the distribution of gender in the dataset?
table(datasett$gender)

#Age and Stroke: (Hypothesis Testing)

#Is there a significant difference in the mean age of patients who had a stroke compared to those who didn't?

age_stroke_1 <- datasett$age[datasett$stroke == 1]
age_stroke_0 <- datasett$age[datasett$stroke == 0]

# Perform the t-test
result <- t.test(age_stroke_1, age_stroke_0)

# View the results
print(result)

barplot(c(result$estimate[1], result$estimate[2]), 
        names.arg = c("Stroke", "No Stroke"),
        col = c("blue", "green"),
        main = "Mean Age Comparison",
        ylab = "Mean Age")

# Create a density plot to visualize the age distribution
plot(density(age_stroke_1), col = "blue", main = "Age Distribution - Stroke", xlab = "Age")
lines(density(age_stroke_0), col = "green")


#Is there a significant difference in average glucose levels between patients who had a stroke and those who did not?

t.test(datasett[datasett$stroke == 1, "avg_glucose_level"], datasett[datasett$stroke == 0, "avg_glucose_level"])

# Create an ECDF plot for the two groups
ggplot(datasett, aes(x = avg_glucose_level, color = factor(stroke))) +
  stat_ecdf(geom = "step") +
  labs(
    x = "Average Glucose Level",
    y = "ECDF",
    title = "ECDF Plot of Average Glucose Level by Stroke Status"
  ) +
  scale_color_discrete(name = "Stroke")


#Work Type and Stroke:
#Is there a significant difference in the proportion of stroke cases among different work types?

table(datasett$work_type, datasett$stroke)
# Calculate the percentage of stroke cases for each work type
percentage_stroke_cases <- data.frame(
  Work_Type = c("children", "Govt_job", "Never_worked", "Private", "Self-employed"),
  Percentage_Stroke = c(0.149, 4.44, 0, 4.52, 6.84)
)

# Create a bar plot
library(ggplot2)

ggplot(percentage_stroke_cases, aes(x = Work_Type, y = Percentage_Stroke)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.5) +
  labs(
    x = "Work Type",
    y = "Percentage of Stroke Cases",
    title = "Percentage of Stroke Cases by Work Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Create a contingency table
work_type_vs_stroke <- table(datasett$work_type, datasett$stroke)

# Perform Fisher's exact test with simulation
fisher_test_result <- fisher.test(work_type_vs_stroke, simulate.p.value = TRUE)

# View the results
fisher_test_result
#
#
#
#
#Hypertension and Heart Disease: (Categorical analysis)
#forpractice
#What is the proportion of patients with hypertension?
mean(datasett$hypertension)

#What is the proportion of patients with heart disease?
mean(datasett$heart_disease)
###


#Marital Status and Stroke:

#What is the proportion of stroke cases among patients who are married?
mean(datasett[datasett$ever_married == "Yes", "stroke"])

#Residence Type and Stroke:
#What is the proportion of stroke cases in urban and rural areas?
table(datasett$Residence_type, datasett$stroke)

#Smoking Status and Stroke:
#What is the proportion of stroke cases among different smoking status categories?

table(datasett$smoking_status, datasett$stroke)



# Proportion of stroke cases among different smoking status categories
barplot(table(datasett$smoking_status, datasett$stroke), beside = TRUE, col = c("lightblue", "lightgreen"), legend.text = TRUE, main = "Proportion of Stroke Cases by Smoking Status")

# Proportion of stroke cases in urban and rural areas
barplot(table(datasett$Residence_type, datasett$stroke), beside = TRUE, col = c("lightblue", "lightgreen"), legend.text = TRUE, main = "Proportion of Stroke Cases by Residence Type")

# Proportion of patients with hypertension
barplot(mean(datasett$hypertension), names.arg = "Hypertension", col = "lightblue", main = "Proportion of Patients with Hypertension")


# Marital status and stroke pie chart
marital_stroke_proportion <- table(datasett$stroke, datasett$ever_married)
labels <- c("No Stroke - Not Married", "No Stroke - Married", "Stroke - Not Married", "Stroke - Married")
sizes <- c(marital_stroke_proportion[1, 1], marital_stroke_proportion[1, 2], marital_stroke_proportion[2, 1], marital_stroke_proportion[2, 2])
pie(sizes, labels = labels, col = c("lightblue", "lightgreen", "skyblue", "lightgreen"), main = "Marital Status and Stroke", hole = 0.5)


#Correlation:



#Logistic Regression (for Binary Outcomes):

#Can we predict the likelihood of having a stroke based on age and hypertension status?

logit_model <- glm(stroke ~ age + hypertension, data=datasett, family = "binomial")
summary(logit_model)

library(kableExtra)

# Create a summary table
summary_table <- tidy(logit_model)

# Display the summary table with kable
kable(summary_table, "html", caption = "Logistic Regression Summary") %>%
  kable_styling(full_width = FALSE)

# Load the necessary package if not already loaded
library(pROC)

# Predict the probabilities of stroke
predicted_probabilities <- predict(logit_model, type = "response")

# Create the ROC curve and plot it with legacy axes
roc_curve <- roc(datasett$stroke, predicted_probabilities)

# Plot the ROC curve with adjusted labeling
plot(roc_curve, main = "ROC Curve for Stroke Prediction", asp = NA, legacy.axes = TRUE)


#Data Visualization:
#Create a bar plot to visualize the distribution of stroke cases based on gender.

library(ggplot2)
ggplot(datasett, aes(x = gender, fill = factor(stroke))) + geom_bar()

library(ggplot2)

# Create a new data frame for visualization
plot_data <- data.frame(Gender = factor(datasett$gender), Stroke = factor(datasett$stroke))

# Create the bar plot for cases by Gender
ggplot(plot_data, aes(x = Gender, fill = Stroke)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Stroke Cases by Gender",
       x = "Gender",
       y = "Count") +
  scale_fill_discrete(name = "Stroke", labels = c("No", "Yes"))




