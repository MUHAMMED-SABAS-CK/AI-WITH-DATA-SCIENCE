library(dplyr)

#Load data set
data <- read.csv("sales_data_sample.csv",stringsAsFactors = TRUE)

S#Basic information
str(data)
summary(data)
head(data)

# Check the missing values
sum(is.na(data))
missing_values <- colSums(is.na(data))
missing_values[missing_values > 0]

sapply(data$TERRITORY,class)

# Step 1: Convert factor column to character
data$TERRITORY <- as.character(data$TERRITORY)

# Step 2: Replace missing values with "None"
data$TERRITORY[is.na(data$TERRITORY)] <- "None"

# Step 3: Convert back to factor
data$TERRITORY <- as.factor(data$TERRITORY)

sum(is.na(data))

# check out layers
for (col in names(data)){
  if(is.numeric(data[[col]])){
    boxplot(data[[col]], 
            main = paste("Boxplot of", col), 
            ylab = col, 
            col = "lightblue") 
  } 
}
# Handling out layers
for (col in names(data)) {
  if (is.numeric(data[[col]])) {
    
    Q1 <- quantile(data[[col]],0.25,na.rm = TRUE) 
    Q3 <- quantile(data[[col]],0.75,na.rm = TRUE ) 
    IQR <- Q3 - Q1 
    
    lower_bound <- Q1 - 1.5*IQR 
    upper_bound <- Q3 + 1.5*IQR 
    
    
    data[[col]] <- ifelse(data[[col]]< lower_bound, lower_bound, 
                          ifelse(data[[col]] > upper_bound, upper_bound, 
                                 data[[col]]))
  }
}
for (col in names(data)){
  if(is.numeric(data[[col]])){
    boxplot(data[[col]], 
            main = paste("Boxplot of", col), 
            ylab = col, 
            col = "lightblue") 
  } 
}

