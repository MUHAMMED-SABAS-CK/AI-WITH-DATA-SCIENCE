library(ggplot2)


# Visualization
# Histogram of Sales
ggplot(data, aes(x = SALES)) +
  geom_histogram(binwidth = 10000, fill = "blue") +
  theme_minimal() +
  labs(title = "Distribution of Sales", x = "SALES", y = "QTY")

# Boxplot of Sale by productline
ggplot(data, aes(x = PRODUCTLINE, y = SALES, fill = PRODUCTLINE)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "SaleS Distribution by Productline") +
  theme(axis.text.x = element_text(angle = 90))

# Line plot of sales over the years
ggplot(data, aes(x = YEAR_ID, y = SALES)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "SALES FLOW IN EACH YEAR", x = "Year", y = "SaleS")

# Count the frequency of each category in MSZoning
status_counts <- table(data$STATUS)

# Create a simple pie chart
pie(status_counts, main = "Distribution of Status", col = rainbow(length(status_counts)))

data$DEALSIZE
# Count the number of High and Low priced houses
DEALSIZE_COUNTS <- table(data$DEALSIZE)

# Create a bar chart
barplot(DEALSIZE_COUNTS, 
        main = "LARGE vs MEDIUM vs SMALL  DEALSIZE SALES DISTRIBUTION", 
        col = c("blue", "red","yellow"), 
        xlab = "DEALSIZE", 
        ylab = "COUNT", 
        legend = TRUE)
DEALSIZE_COUNTS

ggplot(data , aes(x = MSRP, y = SALES)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "SaleS vs MSRP Quality for High-Quality products",
       x = "MSRP",
       y = "SALES") +
  theme_minimal()

