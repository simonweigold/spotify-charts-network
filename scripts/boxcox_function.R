# Function to perform Box-Cox transformation on a specific column of a dataframe
boxcox_transform <- function(data, column_name) {
  library(MASS)  # Required for boxcox() function
  
  # Check if the column name is valid
  #if (!(column_name %in% colnames(data))) {
    #stop(paste("Invalid column name:", column_name))
  #}
  
  # Find the optimal lambda value
  lambda <- boxcox(data$column_name ~ 1)$x[which.max(boxcox(data$column_name ~ 1)$y)]
  
  # Apply the Box-Cox transformation using the optimal lambda and replace the original column with the transformed values
  data$column_name <- ((data$column_name^lambda) - 1)/lambda
  
  # Return the updated dataframe
  return(data)
}
