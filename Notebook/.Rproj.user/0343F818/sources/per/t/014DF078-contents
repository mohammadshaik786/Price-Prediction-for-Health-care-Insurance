#************* LIBRARIES ******************

# Install required Packages
#install.packages('cowplot')
#install.packages('Hmisc')
#install.packages('WVPlots')
#install.packages('GGally')
#install.packages('caret')
#install.packages("glue")

# Import the required packages
library(dplyr)    #Data Wrangling
library(Hmisc)    #Data Analysis
library(ggplot2)  #Plotting Graphs
library(cowplot)  #Plot annotations for ggplot
library(WVPlots)  #Plot for Analysis
library(GGally)   #Correlation plot
library(caret)    #Data Splitting

#************* DATASET ******************

cat("Data Processing following ETL approach")
cat("Reading(Extracting) the INSURANCE dataet")
# set.seed(123)
# Get current Working Directory to fetch dataset
getwd()
# Read CSV file
#InsData_df <- read.csv("/Users/mohammadshaik/Desktop/UMKC/Econometrics of Data Science/Project/Cost/insurance.csv")
InsData_df<-read.csv("insurance.csv")

# View Dataset
View(InsData_df)
# Dimension of Dataset
dim(InsData_df)
# Reading First 5 Rows from the dataframe
head(InsData_df,5)
#InsData_df[1:5,]
#sample_n(InsData_df, 5)


#************* ETL ******************


cat("Cleaning(Transformation) the Data")
cat("Structure of Data before processing",str(InsData_df))

# Check Missing Values
colSums(is.na(InsData_df))

# Find duplicates in data 
InsData_df[duplicated(InsData_df), ]

# Remove Duplicates
Pro_InsData_df <- InsData_df %>% distinct()

cat("Structure of Data after processing",str(Pro_InsData_df))

#describe(Pro_InsData_df)



#************* Exploratory Data Analysis ******************

#summary from the dataset
summary(Pro_InsData_df)

# Checking each feature correlation with target feature
cat("Correlation Plots")
x <- ggplot(Pro_InsData_df, aes(age, charges)) +
  geom_jitter(color = "red", alpha = 0.5) +
  theme_light()

y <- ggplot(Data, aes(sex, charges)) +
  geom_jitter(aes(color = sex), alpha = 0.7) +
  theme_light()

z <- ggplot(Pro_InsData_df, aes(bmi, charges)) +
  geom_jitter(color = "skyblue", alpha = 0.5) +
  theme_light()

u <- ggplot(Data, aes(children, charges)) +
  geom_jitter(aes(color = children), alpha = 0.7) +
  theme_light()

v <- ggplot(Pro_InsData_df, aes(smoker, charges)) +
  geom_jitter(aes(color = smoker), alpha = 0.7) +
  theme_light()

w <- ggplot(Pro_InsData_df, aes(region, charges)) +
  geom_jitter(aes(color = region), alpha = 0.7) +
  theme_light()

plot_var <- plot_grid(x, y, z, u, v, w) 
title <- ggdraw() + draw_label("Correlation between Explorartory Features and Taregt Feature", fontface='bold')
plot_grid(title, plot_var, ncol=1, rel_heights=c(0.1, 1))

a <- plot <- ggplot(data = Pro_InsData_df, aes_string(x = 'age', y = 'charges', group = 'smoker', fill = 'smoker', col = 'smoker')) + 
  geom_jitter() + 
  geom_smooth(method = 'loess') +
  ggtitle(glue::glue("Charges vs age"))

b <- plot <- ggplot(data = Pro_InsData_df, aes_string(x = 'bmi', y = 'charges', group = 'smoker', fill = 'smoker', col = 'smoker')) + 
  geom_jitter() + 
  geom_smooth(method = 'loess') +
  ggtitle(glue::glue("Charges vs bmi"))

c <- plot <- ggplot(data = Pro_InsData_df, aes_string(x = 'children', y = 'charges', group = 'smoker', fill = 'smoker', col = 'smoker')) + 
  geom_jitter() + 
  geom_smooth(method = 'loess') +
  ggtitle(glue::glue("Charges vs children"))

plot_var <- plot_grid(a, b, c) 
title <- ggdraw() + draw_label("Analysis of charges by age, bmi and children as per the smoker factor.", fontface='bold')
plot_grid(title, plot_var, ncol=1, rel_heights=c(0.1, 1))

# Correlation Plot
ggcorr(Pro_InsData_df %>% mutate_if(is.factor, as.numeric), label = TRUE)

#library(corrplot)
#M = cor(Pro_InsData_df)
#corrplot(M, method = 'number') # colorful number

#data_new <- sapply(Pro_InsData_df, unclass)           # Convert categorical variables
#head(data_new,5) 

#Pro_InsData_df[, c('sex', 'smoker', 'region')] <- sapply(Pro_InsData_df[, c('sex', 'smoker', 'region')], unclass)
#head(Pro_InsData_df,5)

#Convert categorical variables into numerical
Pro_InsData_df$sex <- factor(Pro_InsData_df$sex)
contrasts(Pro_InsData_df$sex)
Pro_InsData_df$smoker <- factor(Pro_InsData_df$smoker)
contrasts(Pro_InsData_df$smoker)
Pro_InsData_df$region <- factor(Pro_InsData_df$region)
contrasts(Pro_InsData_df$region)
head(Pro_InsData_df,5)

# Correlation Plot for selected features
ggcorr(Pro_InsData_df %>% mutate_if(is.factor, as.numeric), label = TRUE)


#************* Data Modeling ******************

#Split the dataset for training and testing
inTrain <- createDataPartition(y = Pro_InsData_df$charges, p = .80, list = FALSE)
training <- Pro_InsData_df[inTrain,]
testing <- Pro_InsData_df[-inTrain,]

#Dimensions of splitted data
dim(training)
dim(testing)

#formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")

#Build the multilinear model
lm_model_1 <- lm(charges ~ ., data = training)
summary(lm_model_1)

#Saving R-squared
r_sq_1<- summary(lm_model_1)$r.squared
#Saving Residual standard error
res_std_err_1<- summary(lm_model_1)$sigma

#Prediction on the testing data
pred_1 <- predict(lm_model_1, newdata = testing)
#calculating the residuals
#residuals_1 <- testing$charges - pred_1
#calculating Mean Squared Error
mae_1 <- MAE(pred_1, testing$charges)
#calculating Root Mean Squared Error
rmse_1 <- RMSE(pred_1, testing$charges)



#Binding the model values
lin_reg_1 <- cbind("Res Std Error" = res_std_err_1, "R-squared" = r_sq_1, "MAE" = mae_1, "RMSE" = rmse_1)
cat("Evaluation Metrics for Model 1:")
lin_reg_1

#Build the new multilinear model
lm_model_2 <- lm(charges ~ age + bmi + children + smoker + region, data = training)
summary(lm_model_2)

#Saving R-squared
r_sq_2<- summary(lm_model_2)$r.squared
#Saving Residual standard error
res_std_err_2<- summary(lm_model_2)$sigma
#Prediction on the testing data
pred_2 <- predict(lm_model_2, newdata = testing)
#calculating the residuals
#residuals_2 <- testing$charges - pred_2
#calculating Mean Squared Error
mae_2 <- MAE(pred_2, testing$charges)
#calculating Root Mean Squared Error
rmse_2 <- RMSE(pred_2, testing$charges)

#Binding the model values
lin_reg_2 <- cbind("Res Std Error" = res_std_err_2, "R-squared" = r_sq_2, "MAE" = mae_2, "RMSE" = rmse_2)
cat("Evaluation Metrics for Model 2:")
lin_reg_2

#Build the new multilinear model
lm_model_3 <- lm(charges ~ age + bmi + children + smoker , data = training)
summary(lm_model_3)

#Saving R-squared
r_sq_3<- summary(lm_model_3)$r.squared
#Saving Residual standard error
res_std_err_3<- summary(lm_model_3)$sigma
#Prediction on the testing data
pred_3 <- predict(lm_model_3, newdata = testing)
#calculating the residuals
#residuals_3 <- testing$charges - pred_3
#calculating Mean Squared Error
mae_3 <- MAE(pred_3, testing$charges)
#calculating Root Mean Squared Error
rmse_3 <- RMSE(pred_3, testing$charges)

#Binding the model values
lin_reg_3 <- cbind("Res Std Error" = res_std_err_3, "R-squared" = r_sq_3, "MAE" = mae_3, "RMSE" = rmse_3)
cat("Evaluation Metrics for Model 3:")
lin_reg_3


#************* Evaluation Metrics ******************


#Binding all the models
result <- rbind(lin_reg_1, lin_reg_2, lin_reg_3)
rownames(result) <- c("MultiLinear Regression 1", "MultiLinear Regression 2", "MultiLinear Regression 3")
cat("Evaluation Metrics for all the Models:")
result

#Based on the metrics, we finalized model 2

#************* Model Performance ******************

# Relation between Prediction and Real values
testing$prediction <- predict(lm_model_2, newdata = testing)
ggplot(data = testing, aes(x = prediction, y = charges)) + 
  geom_point(color = "green", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction V/S Real values")

# Relation between Residuals and Linear model prediction
testing$residuals <- testing$charges - testing$prediction

ggplot(data = testing, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "green", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals V/S Linear model prediction")

ggplot(data = testing, aes(x = residuals)) + 
  geom_histogram(bins = 15, fill = "skyblue") +
  ggtitle("Histogram of residuals")


#GainCurvePlot(testing, "prediction", "charges", "Model")


#************* Prediction on new Data ******************


#Crete new data for prediction
features <- data.frame(age=c(30,25, 28), bmi=c(40.5,38,41.2), children=c(1,2,3), smoker=c("no","yes","yes"), region =c("southwest","northwest", "southeast"))

#Predicting the charges
charges = round(predict(lm_model_2, newdata=features, type="response"),2)


#Binding the dataframe and prediction
result <- cbind(features,charges)
rownames(result) <- c("Rezwana","Mohammad", "Jagadeesh")
cat("Health care charges for new data:")
result
