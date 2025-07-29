#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

# Load packages
library(caret)
library(dplyr)
library(glmnet)  
library(MLmetrics)
library(rpart)
library(plumber)

#* @apiTitle Diabetes Model
#* @apiDescription Fit the best model to our diabetes data




# Read the data in like we did for EDA
diabetes_data <- read.csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# Pick response and variables to be investigated and mutate them appropriately
diabetes_data <- diabetes_data[, c("Diabetes_binary", "BMI", "PhysActivity", "HighBP")] %>%
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0, 1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0, 1), labels = c("No", "Yes"))
  )

# Split the data
set.seed(1)
train_index <- createDataPartition(diabetes_data$Diabetes_binary, p = 0.7, list = FALSE)
train_df <- diabetes_data[train_index, ]
test_df  <- diabetes_data[-train_index, ]

# Best model
ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = mnLogLoss
)

set.seed(1)
logmodel3 <- train(
  Diabetes_binary ~ BMI + PhysActivity + HighBP,
  data = train_df,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "logLoss"
)

#* Predict probability of diabetes
#* @param BMI:numeric BMI value (e.g., 25)
#* @param PhysActivity:character "Yes" or "No"
#* @param HighBP:character "Yes" or "No"
#* @get /pred
function(BMI = mean(train_df$BMI),
         PhysActivity = "Yes",
         HighBP = "Yes") {
  
  input_df <- data.frame(
    BMI = as.numeric(BMI),
    PhysActivity = factor(PhysActivity, levels = levels(train_df$PhysActivity)),
    HighBP = factor(HighBP, levels = levels(train_df$HighBP))
  )
  
  prob <- predict(logmodel3, newdata = input_df, type = "prob")[, "Yes"]
  list(prob_diabetes = prob)
}

# Function 1
"http://127.0.0.1:8000/pred"

# Function 2
"http://127.0.0.1:8000/pred?BMI=24.5&PhysActivity=Yes&HighBP=No"

# Function 3
"http://127.0.0.1:8000/pred?BMI=32.0&PhysActivity=No&HighBP=Yes"

#* Author and site info
#* @get /info
function() {
  list(
    name = "Trever Yoder",
    github_site = "https://trever-y.github.io/Project_3/"
  )
}

# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
