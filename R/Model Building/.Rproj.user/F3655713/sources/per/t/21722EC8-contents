library(caret)

# MODEL BUILDING
# step1:Drop unwanted columns
data_model <- data%>%select(-ORDERNUMBER,-ORDERLINENUMBER,-PRODUCTCODE,-CUSTOMERNAME,-CONTACTLASTNAME,-CONTACTFIRSTNAME,-PHONE)

# step2:partition data
set.seed(123) 
trainIndex <- createDataPartition(data_model$SALES, p = 0.8, list = FALSE) 
train_data <- data_model[trainIndex, ] 
test_data  <- data_model[-trainIndex, ] 
train_control <- trainControl( 
  method = "cv", # cross-validation         
  number = 5,   #5-fold CV         
  verboseIter = TRUE 
) 

# step3:Train a model
# Start with Linear Regression 
set.seed(123) 
lm_model <- train( 
  SALES ~ .,  
  data = train_data, 
  method = "lm", 
  preProcess = c("center", "scale", "zv", "nzv"), 
  trControl = train_control 
) 
print(lm_model)

# Evaluation on Test data
pred_lm <- predict(lm_model, newdata = test_data) 
# RMSE and R² 
postResample(pred = pred_lm, obs = test_data$SALES)

