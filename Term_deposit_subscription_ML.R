#Import dataset
install.packages("ggplot2")
getwd()
setwd("C:/Users/joshd/ML/Coursework")
marketing_data<-read.csv(file="Bank_Marketing_Campaign.csv", header=T)
names(marketing_data)
summary(marketing_data)

#Show NAs per column
colSums(is.na(marketing_data))

col_missing <- colSums(is.na(marketing_data))  
total_rows <- nrow(marketing_data)

percent_missing <- (col_missing / total_rows) * 100  # Convert to percentage

NA_summary <- data.frame(Missing_Values = col_missing,
                              Percentage_Missing = round(percent_missing, 2))

print(NA_summary)
marketing_data_cleaned <- na.omit(marketing_data)


#create a boxplot for all numeric data types
library(ggplot2)


ggplot(marketing_data_cleaned, aes(x = y, y = balance, fill = y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Balance by Subscription Status",
       x = "Subscription Status (Y)",
       y = "Balance (£)") +
  theme_minimal()

#Boxplot for age
ggplot(marketing_data_cleaned, aes(x = y, y = age, fill = y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age by Subscription Status",
       x = "Subscription Status (y)",
       y = "Age") +
  theme_minimal()

#Boxplot for Duration
ggplot(marketing_data_cleaned, aes(x = y, y = duration, fill = y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Call Duration by Subscription Status",
       x = "Subscription Status (Y)",
       y = "Call Duration in seconds") +
  theme_minimal()

#Boxplot for Campaign
ggplot(marketing_data_cleaned, aes(x = y, y = campaign, fill = y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Number of Contacts by Subscription Status",
       x = "Subscription Status (y)",
       y = "Number of Contacts (Campaign)") +
  theme_minimal()

#Boxplot for Pdays
ggplot(marketing_data_cleaned, aes(x = y, y = pdays, fill = y)) +
  geom_boxplot() +
  labs(title = "Box Plot of Days Since Last Contact by Subscription Status",
       x = "Subscription Status (y)",
       y = "Days Since Last Contact (pdays)") +
  theme_minimal()


# Function to remove outliers using IQR 
remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  df <- df[df[[col]] >= lower_bound & df[[col]] <= upper_bound, ]
  return(df)
}

# Apply outlier removal for selected numeric variables
marketing_data_cleaned <- remove_outliers(marketing_data_cleaned, "balance")
marketing_data_cleaned <- remove_outliers(marketing_data_cleaned, "age")
marketing_data_cleaned <- remove_outliers(marketing_data_cleaned, "duration")
marketing_data_cleaned <- remove_outliers(marketing_data_cleaned, "campaign")
#should be at 34596 obs by now

cor_matrix <- cor(marketing_data_cleaned[, sapply(marketing_data, is.numeric)])
print(cor_matrix)


library(ggplot2)
library(reshape2)

#Matrix for ggplot and create heatmap
cor_data <- melt(cor_matrix)
ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint=0) +
  labs(title = "Correlation heatmap of all numeric independant variables",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#EDA
head(marketing_data_cleaned)
dim(marketing_data_cleaned)
summary(marketing_data_cleaned)

#Histograms for visualisation

ggplot(marketing_data_cleaned, aes(x=age)) + geom_histogram(bins=30, fill="blue", color="black") + 
  labs(title="Distribution of Age", x="Age", y="Frequency")

ggplot(marketing_data_cleaned, aes(x=balance)) + geom_histogram(bins=30, fill="green", color="black") + 
  labs(title="Distribution of Balance", x="Balance (£)", y="Frequency")

ggplot(marketing_data_cleaned, aes(x=duration)) + geom_histogram(bins=30, fill="purple", color="black") + 
  labs(title="Distribution of Duration", x="Duration of call (S)", y="Frequency")


#bar plot for visualisation
ggplot(marketing_data_cleaned, aes(x=job)) + geom_bar(fill="lightblue") + 
  labs(title="Job Distribution", x="Job", y="Count")

ggplot(marketing_data_cleaned, aes(x=marital)) + geom_bar(fill="lightgreen") + 
  labs(title="Marital Status Distribution", x="Marital", y="Count")

ggplot(marketing_data_cleaned, aes(x=education)) + geom_bar(fill="lightcoral") + 
  labs(title="Education Level Distribution", x="Education", y="Count")



#Standardise data for PCA
scaled_data <- scale(marketing_data_cleaned[, c("age", "balance")])

#PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
summary(pca_result)

#extract importance of PCs
importance <- summary(pca_result)$importance


#PCA Scatter Plot
pca_data <- data.frame(PC1 = pca_result$x[,1], PC2 = pca_result$x[,2], Subscription = marketing_data_cleaned$y)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Subscription)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA - 2D Scatter Plot", x = "PC1", y = "PC2") +
  theme_minimal()


#Chi-squared test
#Contingency table
contingency_table <- table(marketing_data_cleaned$job, marketing_data_cleaned$y)
print(contingency_table)

chi_test <- chisq.test(contingency_table)
print(chi_test)

ggplot(marketing_data_cleaned, aes(x=job, fill=y))+
  geom_bar(position="dodge") + 
  labs(title = "subscription to term deposit by jobs", x="job", y="count")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#--------------------------------------------------PART2------------------------------------------------------#
#Start by using LR using glm()
#Change y to 0,1 instead of no,yes.


marketing_data_cleaned$y_binary <- ifelse(marketing_data_cleaned$y == "yes", 1, 0)
logit_model <- glm(y_binary ~ balance, family=binomial, data = marketing_data_cleaned)
summary(logit_model)

#Repeat using multiple variables
marketing_data_cleaned$y_binary <- ifelse(marketing_data_cleaned$y == "yes", 1, 0)
logit_model <- glm(y_binary ~ age + job + marital + education + balance + housing + loan + duration , family=binomial, data = marketing_data_cleaned)
summary(logit_model)

#Plotting for visualisation
library(tidyverse)

# Step 1: Convert 'y' to binary (1 = "yes", 0 = "no")
bank_data_plot <- mutate(marketing_data_cleaned, prob = ifelse(y == "yes", 1, 0))

# Step 2: Plot logistic regression curve for 'balance'
ggplot(bank_data_plot, aes(balance, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model",
    x = "Customer Account Balance",
    y = "Probability of Subscription"
  )

























