```{r}
library(matlib)
library(visdat)
library(ggplot2)
library(glmnet)
library(rsample)
library(MASS)
library(dplyr)
library(ggpubr)
library(gridExtra)
library(patchwork)
```
# Reading and Exploring Customer Shopping Data
```{r}
# Reading the CSV file containing customer shopping data
customer_data = read.csv("./customer_shopping_data.csv")
```

```{r}
# Displaying the first few rows of the customer data
head(customer_data)
```

```{r}
# Displaying the last few rows of the customer data
tail(customer_data)
```

```{r}
# Providing an overview of the customer data
summary(customer_data)
```
```{r}
 # Displaying the structure of the customer_data data set.
str(customer_data)
```
# Checking for Missing Values and Displaying Message
```{r}
# Check for null values in the entire data frame
null_values <- is.na(customer_data)

# Calculate the total number of missing values
total_missing <- sum(null_values)

# Print a message based on whether there are missing values or not
if(total_missing == 0) {
  message("There are 0 missing values.")
} else {
  message("There are ", total_missing, " missing values in the data.")
}

```
# Retrieving Unique Categories
```{r}
# Retrieving unique gender values
distinct_genders <- unique(customer_data$gender)
print(distinct_genders)

# Retrieving unique category values
distinct_category <- unique(customer_data$category)
print(distinct_category)

# Retrieving unique payment_method values
distinct_payment_method <- unique(customer_data$payment_method)
print(distinct_payment_method)

# Retrieving unique shopping_mall values
distinct_shopping_mall <- unique(customer_data$shopping_mall)
print(distinct_shopping_mall)

```
# Categorical Variable Transformation and Display.

```{r}
# Transform gender into numeric factors
customer_data$gender <- as.numeric(factor(customer_data$gender, levels = unique(customer_data$gender)))

# Transform category into numeric factors
customer_data$category <- as.numeric(factor(customer_data$category, levels = unique(customer_data$category)))

# Transform payment method into numeric factors
customer_data$payment_method <- as.numeric(factor(customer_data$payment_method, levels = unique(customer_data$payment_method)))

# Transform shopping mall into numeric factors
customer_data$shopping_mall <- as.numeric(factor(customer_data$shopping_mall, levels = unique(customer_data$shopping_mall)))

# Display the first few rows of the modified data frame
head(customer_data)


```
# Task 1: Preliminary data analysis
# Extracting Feature Variables
```{r}
 # Selects all columns except specified ones and assigns to variable x.

x <- customer_data[, !(names(customer_data) %in% c("invoice_no","customer_id","gender","quantity", "invoice_date","shopping_mall"))]

print(x)
 
```
# Time Series Analysis of Input Customer Shopping Data Attributes
```{r}
# Select relevant columns for time series analysis
ts_data <- customer_data[, c("age","category", "price", "payment_method")]

# Create a time series object with monthly frequency (assuming data is monthly)
customer_data.ts <- ts(ts_data, 
                       start = c(as.numeric(format(min(customer_data$invoice_date), "%Y")), 
                                 as.numeric(format(min(customer_data$invoice_date), "%m"))), 
                       end = c(as.numeric(format(max(customer_data$invoice_date), "%Y")), 
                               as.numeric(format(max(customer_data$invoice_date), "%m"))), 
                       frequency = 12)  # Monthly data, so frequency is 12

# Plot the time series of selected data with one-month interval
plot(customer_data.ts, main = "Time Series Analysis of Customer Data", 
     xlab = "Invoice Date", ylab = "Customer Inputs")

```

# Quantity Output Trends Over Time
```{r}

# Convert the 'invoice_date' column to the Date format if it's not already in that format
customer_data$invoice_date <- as.Date(customer_data$invoice_date, format="%d/%m/%Y")

# Extract unique monthly dates from the 'invoice_date' column
distinct_dates <- unique(format(customer_data$invoice_date, "%Y-%m"))

# Create a time series object for monthly quantity data
customer_quantity_ts <- ts(customer_data$quantity, 
                           start = c(as.numeric(format(min(customer_data$invoice_date), "%Y")), 
                                     as.numeric(format(min(customer_data$invoice_date), "%m"))), 
                           end = c(as.numeric(format(max(customer_data$invoice_date), "%Y")), 
                                   as.numeric(format(max(customer_data$invoice_date), "%m"))), 
                           frequency = 12)  # Assuming monthly data, frequency set to 12

# Plot the time series of 'customer_data$quantity' with one-month intervals
plot(customer_quantity_ts, main = "Time Series Analysis: Quantity Output",
     xlab = "Invoice Date", ylab = "Quantity (output)")


```

# Comparison of Price and Age Distributions
```{r}
# Constructing a density plot illustrating the distribution of 'price'
plot_density <- ggplot(data = data.frame(x = x$price), aes(x = x)) + 
  geom_density(fill = "palegoldenrod", color = "indianred3", alpha = 0.7) + 
  labs(title = "Price Distribution: Density Plot", x = "Price", y = "Density")

# Creating a histogram depicting the distribution of 'age'
plot_histo <- ggplot(data = data.frame(x = x$age), aes(x = x)) + 
  geom_histogram(binwidth = 10, fill = "lavender", color = "firebrick3", alpha = 0.7) + labs(title = "Age Distribution: Histogram", x = "Age", y = "Frequency") + 
  theme_minimal()

# Merging both plots into a single display
plot_comb <- ggarrange(plot_density, plot_histo, ncol = 1, heights = c(2,1))

# Visualizing the Combined Plot
print(plot_comb)

```
# Exploring Payment Method Distribution: Density Plot and Histogram
```{r}
# Estimate density within a specified range
dis <- density(x$payment_method, from = min(x$payment_method), to = max(x$payment_method))

# Display density plot
plot(dis, main = "Density plot of entire dataset")

# Create a Histogram of Payment Method Inputs
hist(x$payment_method, freq = FALSE, main = "Histogram and density plot of payment methods", xlab = "Payment Method")

# Overlay density on the histogram
lines(dis, lwd = 2, col = "purple")

# Include rug plot
rug(jitter(x$payment_method))

```
# Exploratory Data Analysis: Density Plot and Histogram of Payment Method
```{r}
# Calculate density using the density function
dis <- density(x$payment_method)

# Density plot creation
density_plot <- ggplot() + 
  geom_line(data = data.frame(x = dis$x, y = dis$y), aes(x = x, y = y), color = "indianred3", size = 1) +
  geom_rug(data = data.frame(x = jitter(x$payment_method)), aes(x = x), sides = "b") +
  labs(title = "Density Plot of Payment Method", subtitle = "Distribution of payments across the dataset") + 
  theme_minimal()

# Histogram creation (with frequency instead of density)
histogram_plot <- ggplot(data = x, aes(x = payment_method)) + 
  geom_histogram(binwidth = 1, fill = "lavender", color = "firebrick3", alpha = 0.7) +
  labs(title = "Histogram of Payment Method", subtitle = "Frequency of payment values", x = "Payment", y = "Frequency") + 
  theme_minimal()

# Combine plots using gridExtra
combined_plots <- grid.arrange(density_plot, histogram_plot, ncol = 1)

print(combined_plots)

```
# Exploratory Analysis: Density Plot and Histogram Comparison of Age
```{r}
# Density plot creation
plot_density <- ggplot(data = x, aes(x = age)) +  # Define a ggplot object with data 'x' and x-axis aesthetic as 'age'
  geom_density(fill = "palegoldenrod", color = "indianred3", alpha = 0.7) +  # Add a density layer with specified fill, color, and transparency
  ggtitle("Density Plot of Age")  # Add a title to the plot

# Histogram plot creation
plot_histo <- ggplot(data = x, aes(x = age)) +  # Define a ggplot object with data 'x' and x-axis aesthetic as 'age'
  geom_histogram(banwidth = 1, fill = "lavender", color = "firebrick3", alpha = 0.7) +  # Add a histogram layer with specified binwidth, fill, color, and transparency
  ggtitle("Age Histogram") +  # Add a title to the plot
  labs(x = "Age", y = "Frequency")  # Add labels to x and y axes

# Combine plots vertically
plot_comb <- plot_density / plot_histo + plot_layout(ncol = 1)  # Combine the density plot and histogram using '/' operator and set the layout to a single column

# Print the combined plot
print(plot_comb)  # Display the combined plot
```

# Exploring Category Distribution: Combined Density and Histogram Plot
```{r}
# Compute density estimation of the 'category' data
density_estimate <- density(x$category)

# Visualizing the distribution with density plot and overlaid histogram
plot(density_estimate, main = "Distribution of Categories", 
     xlim = c(min(x$category), max(x$category)), 
     xlab = "Categories", ylab = "Density", 
     col = "green", lwd = 2)

# Add histogram with transparency on top of density plot
hist(x$category, breaks = 20, freq = FALSE, add = TRUE, 
     col = alpha("lightgreen", 0.5))

# Overlay density plot atop histogram
lines(density_estimate, lwd = 2, col = "grey")

# Add rug plot for individual data points
rug(jitter(x$category), col = "lightgrey")

# Include legend to denote the representation of each plot
legend("topright", legend = c("Density", "Histogram"), 
       fill = c("lavender", "lightgreen"))
```
# Quantity Distribution: Density Plot and Histogram Comparison
```{r}
# Compute density estimation of quantity data
dis <- density(customer_data$quantity)

# Set plot margins
par(mar = c(5,4,4,2))

# Create histogram without frequency labels
hist(customer_data$quantity, freq = FALSE, main = "Quantity Distribution", 
     xlab = "Quantity", ylab = "Density", col = "lightgrey")

# Overlay density plot
lines(dis, lwd = 2, col = "red")

# Add rug plot for individual data points
rug(jitter(customer_data$quantity), col = "green", lwd = 0.5)

# Add legend
legend("topright", legend = c("Density", "Histogram"), col = c("lightgreen", "purple"), 
       lty = 1, lwd = 2)

# Add axis labels
axis(1)
axis(2, at = 0, labels = FALSE)


```
# Analyzing Correlation: Age, Price, Category, and Payment Method with Quantity
```{r}
# Scatter plot of age vs. quantity
plot(x$age, Y, main = "Correlation between age and quantity", 
     xlab = "Age", ylab = "Quantity")

# Scatter plot of price vs. quantity
plot(x$price, Y, main = "Correlation between price and quantity", 
     xlab = "Price", ylab = "Quantity")

# Scatter plot of category vs. quantity
plot(x$category, Y, main = "Correlation between category and quantity", 
     xlab = "Category", ylab = "Quantity")

# Scatter plot of payment method vs. quantity
plot(x$payment_method, Y, main = "Correlation between payment method and quantity", 
     xlab = "Payment Method", ylab = "Quantity")

```
# Data Preparation and Visualization: Correlation Analysis and Scatter Plot Matrix
```{r}
# Assigning 'data' to 'X_data'
X_data = x

# Adding 'quantity' column from 'customer_data' to 'X_data'
X_data$quantity <- customer_data$quantity

# Computing the correlation matrix of 'X_data'
cor(X_data)

# Creating a scatter plot matrix of 'X_data'
plot(X_data)

```
# Dataframe to Matrix Conversion: Previewing Subset
```{r}
# Convert dataframe 'X_data' to a matrix and assign it to 'X_matrix'
X_matrix = as.matrix(X_data)

# Display the first 4 rows of 'X_matrix'
X_matrix[1:9, ]
```

# Task 2: Regression – modeling the relationship between sales data
# Data Preparation for Regression Analysis
```{r}
# Create new columns B1, B2, B3, B4 in dataframe x and assign corresponding variables
x$B1 <- x$age  # Assign age to B1
x$B2 <- x$category  # Assign category to B2
x$B3 <- x$price  # Assign price to B3
x$B4 <- x$payment_method  # Assign payment method to B4

# Select only columns B1, B2, B3, B4 in dataframe x
x <- x[, c("B1", "B2", "B3", "B4")]

# Convert dataframe x to matrix
x <- as.matrix(customer_data$quantity)

# Convert customer_data$quantity to matrix and assign it to y
y <- as.matrix(customer_data$quantity)

# Display y
y[1:9, ]
```
# Heading: Dataframe Creation and Exploration
```{r}
# Print column names of dataframe x
print(colnames(x))

# Create a new dataframe df with specified columns from customer_data
df <- data.frame(
  B1 = customer_data[, "age"],
  B2 = customer_data[, "category"],
  B3 = customer_data[, "price"],
  B4 = customer_data[, "payment_method"],
  y = customer_data[, "quantity"]
)

# Display the first 5 rows of dataframe df
df[1:5, ]

```
# Definition of ThetaHat Function for Parameter Estimation
```{r}
# Define a function called thetaHat that takes two arguments: 'model' and 'y'
thetaHat <- function(model, y) {
  # Calculate the parameter estimates (theta hat) using the normal equations: (X'X)^(-1)X'y
  return(solve(t(model) %*% model) %*% t(model) %*% y)
}
```
# Polynomial Regression Models Initialization
```{r}
# Model 1: Polynomial regression with multiple polynomial terms for each predictor variable
m1 <- lm(y ~ poly(B4, 1, raw = TRUE) + poly(B1, 2, raw = TRUE) + poly(B1, 3, raw = TRUE) + 
         poly(B2, 4, raw = TRUE) + poly(B1, 4, raw = TRUE), data = df)

# Model 2: Polynomial regression with multiple polynomial terms for selected predictor variables
m2 <- lm(y ~ poly(B4, 1, raw = TRUE) + poly(B1, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = df)

# Model 3: Polynomial regression with multiple polynomial terms for selected predictor variables
m3 <- lm(y ~ poly(B3, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = df)

# Model 4: Polynomial regression with multiple polynomial terms for selected predictor variables
m4 <- lm(y ~ poly(B2, 1, raw = TRUE) + poly(B1, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = df)

# Model 5: Polynomial regression with multiple polynomial terms for selected predictor variables
m5 <- lm(y ~ poly(B4, 1, raw = TRUE) + poly(B1, 2, raw = TRUE) + poly(B1, 3, raw = TRUE) +
         poly(B3, 4, raw = TRUE), data = df)
```
# Estimated Parameters for Polynomial Regression Models
```{r}
estimated_parameters_list <- list(
  Model1 = coef(m1),  # Store the estimated parameters for Model 1
  Model2 = coef(m2),  # Store the estimated parameters for Model 2
  Model3 = coef(m3),  # Store the estimated parameters for Model 3
  Model4 = coef(m4),  # Store the estimated parameters for Model 4
  Model5 = coef(m5)   # Store the estimated parameters for Model 5
)
```
# Estimated Coefficients for Polynomial Regression Models
```{r}
coefficients_df <- data.frame(  # Create a data frame to store the coefficients
  Model = character(0),         # Initialize an empty column for the model names
  θ1 = numeric(0),              # Initialize an empty column for coefficient θ1
  θ2 = numeric(0),              # Initialize an empty column for coefficient θ2
  θ3 = numeric(0),              # Initialize an empty column for coefficient θ3
  θ4 = numeric(0),              # Initialize an empty column for coefficient θ4
  θbias = numeric(0)            # Initialize an empty column for coefficient θbias
)

```
# Loop through each model in the estimated parameters list
```{r}
for (model_name in names(estimated_parameters_list)) {
  parameters <- estimated_parameters_list[[model_name]]  # Extract the parameters for the current model
  coefficients <- extract_coefficients(1:5)  # Extract the coefficients
  
  # Add coefficients to the DataFrame
  coefficients_df <- rbind(coefficients_df, cbind(Model = model_name, coefficients))
}

# Print the coefficients for each model
cat("Coefficients for each model are:\n")
print(coefficients_df)

```
# Calculate the residual sum of squares (RSS) for each model
```{r}
rss_values <- c(
  sum(m1$residuals^2),  # Calculate RSS for Model 1
  sum(m2$residuals^2),  # Calculate RSS for Model 2
  sum(m3$residuals^2),  # Calculate RSS for Model 3
  sum(m4$residuals^2),  # Calculate RSS for Model 4
  sum(m5$residuals^2)   # Calculate RSS for Model 5
)

# Display the RSS values for each model
rss_values

```
# Create a DataFrame to store the Residual Sum of Squares (RSS) for each model
```{r}
rss_df <- data.frame(
  # Define the model names
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  # Assign the RSS values calculated for each model
  RSS = rss_values
)

# Print the DataFrame containing RSS values for each model
rss_df

```
# Function to calculate the log-likelihood for a given model
```{r}
# Define a function to calculate the log-likelihood for a given model
calculate_log_likelihood <- function(model) {
  # Get the number of observations
  n <- length(model$residuals)
  # Calculate the estimated variance
  sigma_sq <- sum(model$residuals^2) / (n - length(model$coefficients))
  # Compute the log-likelihood using the estimated variance
  log_likelihood <- -n/2 * log(2 * pi * sigma_sq) - sum(model$residuals^2) / (2 * sigma_sq)
  # Return the log-likelihood value
  return(log_likelihood)
}

# Calculate the log-likelihood values for each model
log_likelihood_values <- c(
  calculate_log_likelihood(m1),
  calculate_log_likelihood(m2),
  calculate_log_likelihood(m3),
  calculate_log_likelihood(m4),
  calculate_log_likelihood(m5)
)
```
#  Create a data frame to store the log-likelihood values for each model
```{r}
log_likelihood_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
  LogLikelihood = log_likelihood_values
)

# Print the data frame
print(log_likelihood_df)

```
# Calculate the Akaike Information Criterion (AIC) for each model
```{r}
# Store the AIC values in a vector
aic_values <- c(
  AIC(m1),  # Calculate AIC for model 1
  AIC(m2),  # Calculate AIC for model 2
  AIC(m3),  # Calculate AIC for model 3
  AIC(m4),  # Calculate AIC for model 4
  AIC(m5)   # Calculate AIC for model 5
)
```

# Create a data frame to store the AIC values for each model
```{r}
aic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),  # Model names
  AIC = aic_values  # AIC values calculated earlier
)

# Print the AIC data frame
print(aic_df)

```
# Calculate the BIC values for each model
```{r}
bic_values <- c(
  BIC(m1),  # BIC value for Model 1
  BIC(m2),  # BIC value for Model 2
  BIC(m3),  # BIC value for Model 3
  BIC(m4),  # BIC value for Model 4
  BIC(m5)   # BIC value for Model 5
)

```
# Create a dataframe to store the BIC values for each model
```{r}
bic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),  # Model names
  BIC = bic_values  # BIC values
)

# Print the dataframe
bic_df

```
# Generating Predictions for Each Model
```{r}
# Generating predictions for Model 1
predictions1 <- predict(m1)

# Generating predictions for Model 2
predictions2 <- predict(m2)

# Generating predictions for Model 3
predictions3 <- predict(m3)

# Generating predictions for Model 4
predictions4 <- predict(m4)

# Generating predictions for Model 5
predictions5 <- predict(m5)

```
# Calculate prediction errors for each model
```{r}
errors1 <- df$y - predictions1
errors2 <- df$y - predictions2
errors3 <- df$y - predictions3
errors4 <- df$y - predictions4
errors5 <- df$y - predictions5

error_list <- list(errors1, errors2, errors3, errors4, errors5)
```
# Function to Create Q-Q Plots for Prediction Errors
```{r}
plot_qq <- function(errors, model_name) {
  # Create Q-Q plot
  qqnorm(errors, main = paste("Q-Q Plot for", model_name))
  # Add a line to the Q-Q plot
  qqline(errors, col = "red")
}
```
# Setting up Plotting Layout
```{r}
layout(matrix(1:5, nrow = 1))
```
# Generating Q-Q Plots
```{r}
for (i in 1:5) {
  plot_qq(error_list[[i]], model_name = paste("Model", i))
}
```
# Calculating Mean Errors
```{r}
mean_errors <- c(mean(errors1), mean(errors2), mean(errors3), mean(errors4), mean(errors5))

```
# Summary of Model Performance
```{r}
result_table <- data.frame(
  Model = paste("Model", 1:5),
  Mean_Error = mean_errors,
  AIC = aic_values,
  BIC = bic_values,
  Likelihood = log_likelihood_values
)
result_table
```
# Task 2.7
# Splitting the dataset into training and testing sets
```{r}
# Split the data into training and testing sets
train_index <- sample(1:nrow(df), 0.7 * nrow(df))  # Selecting 70% of the data for training
train_data <- df[train_index, ]  # Creating the training dataset
test_data <- df[-train_index, ]  # Creating the testing dataset

# Fit the "optimal" model (Model 3) using the training dataset
best_model <- lm(y ~ poly(B3, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = train_data)

# Forecast sales using the optimal model (Model 3) with test data and calculate prediction intervals
predictions <- predict(best_model, newdata = test_data, interval = "prediction", level = 0.95)

# Prepare a dataframe with test data and predictions
results <- data.frame(
  B1 = test_data$B1,
  B2 = test_data$B2,
  B3 = test_data$B3,
  y_true = test_data$y,
  y_pred = predictions[, 1],      # Extract predicted values
  lower_bound = predictions[, 2],  # Extract lower bound of prediction interval
  upper_bound = predictions[, 3]   # Extract upper bound of prediction interval
)

# Plot the results
plot(results)


```
# Scatterplot with Prediction Intervals
```{r}
# Scatterplot of testing data points with prediction intervals
ggplot(results, aes(x = B1, y = y_true)) +  # Specify the x and y variables
  geom_point(aes(color = abs(y_pred - y_true)), size = 3) +  # Add data points with color indicating prediction error
  geom_line(aes(x = B1, y = y_pred), color = "green", size = 1) +  # Add a line representing predicted values
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.5) +  # Add prediction intervals
  ggtitle("Model 3: Testing Data vs. Predictions with 95% Prediction Intervals") +  # Set title
  xlab("Age") +  # Set x-axis label
  ylab("Sales Quantity") +  # Set y-axis label
  theme_minimal() +  # Apply minimal theme
  scale_color_gradient(low = "orange", high = "purple", guide = "legend", name = "Prediction Error") +  # Color scale for prediction error
  scale_fill_manual(values = c("lightgreen" = "lightgreen"), guide = "none") +  # Set fill color for prediction intervals
  theme(panel.grid.major = element_line(color = "blue", linetype = "dashed"),  # Customize grid lines
        axis.text = element_text(size = 12),  # Customize axis text size
        axis.title = element_text(size = 14, face = "bold")) +  # Customize axis title text size and style
  annotate("text", x = -1, y = 3, label = "3", color = "black", size = 4)  # Add label "3" at y = 3
```
# Model 3 Coefficient Analysis
```{r}
# Extract coefficients from Model 3 and sort them in descending order
num <- coef(best_model)
sort_num <- sort(abs(num), decreasing = TRUE)

# Select the two largest coefficients
large_two_value <- sort_num[1:2]
theta_bias <- large_two_value[1]
theta_four <- large_two_value[2]

# Define values for theta one and theta three (assuming these are constant)
theta_one <- 0.010038614
theta_three <- 0.001912836

# Print the values of the coefficients
print("Values:")
print(paste("Theta_bias:", theta_bias))
print(paste("Theta_four:", theta_four))
print(paste("Theta_one:", theta_one))
print(paste("Theta_three:", theta_three))

# Create a dataframe to display the coefficients
table <- data.frame(
  Y = c(theta_bias, theta_four, theta_one, theta_three),
  row.names = c("Ones", "B4", "B1", "B3")
)

# Print the table
print("Table:")
print(table)

```
# Hypothesis Testing for Model 3
```{r}
# Hypothesis Testing for Model 3: Random Parameter Generation
# Fit Model 3
model_3 <- lm(y ~ poly(B3, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = df)
# Create model matrix
model_3_w <- model.matrix(~ poly(B3, 3, raw = TRUE) + poly(B3, 4, raw = TRUE), data = df)
# Response variable
y <- df$y
# Define threshold epsilon
epsilon <- rss_df$RSS[2] * 2
# Number of iterations
num <- 100
# Initialize vector for storing F-values
f_value <- numeric(num)
# Initialize vector for storing s-values
s_value <- numeric(num)
# Initialize counter
counter <- 0

# Loop for generating F-values and s-values
for (i in 1:num) {
  # Generate random number within range
  rng1 <- runif(1, -2.434384398, 2.434384398)
  # Generate random number within range
  rng2 <- runif(1, -0.001912836, 0.001912836)
  
  # Create new parameter matrix
  new_thetahat <- matrix(c(rng1, rng2, theta_one, theta_three), nrow = length(coef(model_3)), ncol = 1)
  
  # Try fitting the model with new parameters
  tryCatch({
    # Calculate predicted values
    n_Y_Hat <- model_3_w %*% new_thetahat
    # Calculate new RSS
    RSS_new <- sum((y - n_Y_Hat)^2)
    # Store F-value
    f_value[i] <- RSS_new
    # Check if RSS exceeds threshold
    if (RSS_new > epsilon) {
      # Store s-value
      s_value[counter + 1] <- rng1 + rng2
      # Increment counter
      counter <- counter + 1
    }
  }, error = function(e) {
    # Print error message
    message("Error in iteration", i, ": ", e$message)
  })
}

# Trim s_value vector
s_value <- s_value[1:counter]

# Plot histogram of log-transformed f_value
if (all(f_value <= 0)) {
  # Issue warning
  warning("f_value must be strictly positive when applying the log transformation")
} else {
  # Log-transform f_value
  log_f_value <- log(f_value)
  # Create histogram plot
  ggplot(data.frame(log_f_value), aes(x = log_f_value)) +  
    geom_histogram(color = "red", fill = "lightgreen", bins = 30) + 
    geom_rug() + 
    labs(title = "Frequency distribution of the log-transformed f_value", x = "log(f_value)", y = "Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
    scale_x_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10))
}
# Plot histogram of s_value
if(all(is.na(s_value)) || all(s_value == 0)) {
  # Issue warning
  warning("s_value does not contain meaningful data to plot.")
} else {
  # Create histogram plot
  ggplot(data.frame(s_value), aes(x = s_value)) +  
    geom_histogram(color = "blue", fill = "lightyellow", bins = 30) + 
    geom_rug() + 
    labs(title = "Frequency distribution of the s_value") + 
    xlab("s_value") + 
    ylab("Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) + 
    scale_x_continuous(labels = scales::number_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10))}
```
