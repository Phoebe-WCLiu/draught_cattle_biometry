# The following example uses the Bp/GLpe and Bd/GLpe from the first phalanges as input variables 
# and predicts the probability using logistic regression.

# Load Required Packages
library(ggplot2)
library(RColorBrewer)
library(dplyr)


# Logistic Regression Prediction Function
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names match the columns in the actual dataset
logistic_prediction <- function(Bp_Glpe, Bd_Glpe) {
  intercept <- -37.58
  coef_Bp_Glpe <- -13.56   
  coef_Bd_Glpe <- 86.26
  
  odds <- exp(intercept + coef_Bp_Glpe * Bp_Glpe + coef_Bd_Glpe * Bd_Glpe)
  probability <- odds / (1 + odds)
  
  return(probability)
}



# Data Cleaning
# Remove rows with missing values and infinite values in the dataset
# Note: Replace 'test' with the actual name of your dataset
cleaned_Data <- test[complete.cases(test[, c("Bp_Glpe", "Bd_Glpe")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$Bp_Glpe), ]

# Model Prediction
predictions <- logistic_prediction(cleaned_Data$Bp_Glpe, cleaned_Data$Bd_Glpe)
cleaned_Data$Predicted_Probability <- predictions




#Create elements for plotting
x_values <- seq(min(cleaned_Data$Bp_Glpe), max(cleaned_Data$Bp_Glpe), length.out = 100)
y_values <- seq(min(cleaned_Data$Bd_Glpe), max(cleaned_Data$Bd_Glpe), length.out = 100)
grid <- expand.grid(Bp_Glpe = x_values, Bd_Glpe = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_Bp_Glpe * grid$Bp_Glpe + coef_Bd_Glpe * grid$Bd_Glpe)
points_df <- data.frame(Bp_Glpe = cleaned_Data$Bp_Glpe,
                        Bd_Glpe = cleaned_Data$Bd_Glpe,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)


#Plotting with ggplot2
ggplot() +
  geom_point(data = points_df, aes(x = Bp_Glpe, y = Bd_Glpe, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(x = Bp_Glpe, y = Bd_Glpe, z = Predicted_Probability), breaks = 0.5, color = "black", linetype = 4) +
  labs(x = "Bp/GLpe", y = "Bd/GLpe", color = "Probability") +
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) 
  ggtitle("First phalanges")