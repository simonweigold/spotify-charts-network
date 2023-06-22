# Principal Component Analysis --------------------------------------------
pca <- prcomp(metrics %>% select(streams, degree, betweenness, closeness, eigenvector),
              scale = TRUE)
fviz_eig(pca)
fviz_pca_var(pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


# Principal components
principal_components <- pca$x

# Eigenvalues
eigenvalues <- pca$sdev^2

# Proportion of variance explained
variance_explained <- pca$sdev^2 / sum(pca$sdev^2)


# Scree plot
plot(cumsum(variance_explained), xlab = "Number of Components", ylab = "Cumulative Proportion of Variance Explained", type = "b")



# Diagnostics -------------------------------------------------------------
regression <- lm(streams ~ degree + betweenness + closeness + eigenvector,
                 data = metrics_minmax, na.action = na.exclude)

#1. Linearity
avPlots(regression)

#2. No multicollinearity
metrics_minmax %>%
  dplyr::select(c(streams, degree, betweenness, closeness, eigenvector)) %>% 
  stats::cor(use = "na.or.complete") %>% 
  corrplot::corrplot.mixed(upper = "circle",
                           lower = "number",
                           tl.pos = "lt",
                           tl.col = "black",
                           lower.col = "black",
                           number.cex = 1)

CT <- cbind(metrics_minmax$degree, metrics_minmax$betweenness, metrics_minmax$closeness, metrics_minmax$eigenvector)
rcorr(CT)

vif(regression)

#3. Homoskedasticity of the residuals
plot(regression, 1)

bptest(regression)

#4. Normality of the residuals
hist(regression$residuals, breaks= 30, freq = F,
     main = "Distribution of residuals",
     xlab = "Residuals")
curve(dnorm(x, mean(regression$residuals), sd(regression$residuals)), 
      col = "red",
      add = T)
plot(regression, 2)

#5. No influential points
plot(regression, 4)


# penalized linear models -----------------------------------------------

# Separate the independent variables and the dependent variable
plm_x <- as.matrix(metrics_minmax[, c("degree", "betweenness", "closeness", "eigenvector")])
plm_y <- metrics_minmax$streams

# Perform penalized linear regression using glmnet
penalized_regression <- glmnet(plm_x, plm_y)

# Set up lambda values for tuning
lambda_seq <- 10^seq(-2, 2, by = 0.5)

# Perform cross-validation to tune the lambda parameter
cv_result <- cv.glmnet(plm_x, plm_y, lambda = lambda_seq)

# Find the optimal lambda value
optimal_lambda <- cv_result$lambda.min

# Fit the penalized regression model with the optimal lambda
penalized_regression_optimal <- glmnet(plm_x, plm_y, lambda = optimal_lambda)

# Predict the response variable using the optimal model
plm_y_pred <- predict(penalized_regression_optimal, newx = plm_x)

# Calculate the mean squared error (MSE)
mse <- mean((plm_y - plm_y_pred)^2)

# Calculate deviance explained
null_deviance <- sum((plm_y - mean(plm_y))^2)
deviance_explained <- 1 - (sum((plm_y - plm_y_pred)^2) / null_deviance)

# Print the coefficients and R-squared of the optimal model
print(coef(penalized_regression_optimal))
print(paste("MSE:", mse))
print(paste("Deviance Explained:", deviance_explained))

plot(penalized_regression, xvar = "lambda", label = TRUE)


# Plot predicted vs. original values
# Predict the values using the optimal model
predicted_values <- predict(penalized_regression, newx = plm_x, s = optimal_lambda)

# Plot the predicted values and the original values
plot(plm_y, type = "o", col = "blue", ylim = range(c(plm_y, predicted_values)), 
     xlab = "Observation", ylab = "Value", main = "Predicted vs Original Values")
lines(predicted_values, type = "o", col = "red")
legend("topright", legend = c("Original", "Predicted"), 
       col = c("blue", "red"), pch = c(1, 1))


# Plot
ggplot2::ggplot(data = as.data.frame(cbind(plm_y, plm_y_pred)),
                mapping = aes(x = plm_y, y = s0, col = s0)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = F, col = "red")
#predicted_values <- predict(penalized_regression_optimal, newx = plm_x)
#plot(metrics_minmax$streams, predicted_values)
#abline(metrics_minmax$streams, predicted_values)

# Calculate R-squared [= deviance_explained!]
observed_values <- plm_y
mean_observed <- mean(observed_values)
total_sum_of_squares <- sum((observed_values - mean_observed) ^ 2)
residual_sum_of_squares <- sum((observed_values - predicted_values) ^ 2)
rsquared <- 1 - (residual_sum_of_squares / total_sum_of_squares)

