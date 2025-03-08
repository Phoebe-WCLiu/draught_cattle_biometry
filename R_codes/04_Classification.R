# The following R scripts are used for predicting new archaeological data based on 
# the models developed in the study -------------------

# ------------------- Metacarpal -------------------
# -------------------Logistic Regression Prediction Function -------------------
# Set up the logistic regression by defining the coefficients
# ------------------------------------------------------------------------------  
# Logistic regression model  
df <- Metacarpal[!is.na(Metacarpal$MC_e1) & !is.na(Metacarpal$MC_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MC_LG_model <- glm(Type ~ MC_e1 + MC_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))
# ------------------------------------------------------------------------------
intercept <- coef(MC_LG_model)[1]
coef_e_1 <- coef(MC_LG_model)[2]
coef_f_4 <- coef(MC_LG_model)[3]

logistic_prediction <- function(e_1, f_4) {
  intercept <- coef(MC_LG_model)[1]
  coef_e_1 <- coef(MC_LG_model)[2]
  coef_f_4 <- coef(MC_LG_model)[3]
  odds <- exp(intercept + coef_e_1 * e_1 + coef_f_4 * f_4)
  probability <- odds / (1 + odds)
  
  return(probability)
}


# ---------------Import your archaeological dataset for prediction -------------
# Note: Replace 'your_file.csv' with the actual file path of your dataset
# ------------------------------------------------------------------------------
Archaeological_data <- read.csv("your_file.csv")

# ------------Calculate required shape ratios based on measurements------------
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names (e,D1,f,D4) match the columns in the actual dataset
# ------------------------------------------------------------------------------
# Calculate e_1 ratio
Archaeological_data$e_1 <- Archaeological_data$e/Archaeological_data$D1
# Calculate f_4 ratio
Archaeological_data$f_4 <- Archaeological_data$f/Archaeological_data$D4

# --------------------------------Data Cleaning--------------------------------
# Remove rows with missing values and infinite values in the dataset
# ------------------------------------------------------------------------------
cleaned_Data <- Archaeological_data[complete.cases(Archaeological_data[, c("e_1", "f_4")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$e_1), ]

# ------------------------------Model Prediction-------------------------------
predictions <- logistic_prediction(cleaned_Data$e_1, cleaned_Data$f_4)
cleaned_Data$Predicted_Probability <- predictions

# -----------------------Create elements for visualisation----------------------
# To extend the decision boundary further, you may increase the multiplication factor
# For example: 
#x_values <- seq(min(cleaned_Data$e_1), max(cleaned_Data$e_1)*2.5, length.out = 100)
#y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4)*2.5, length.out = 100)
# ------------------------------------------------------------------------------
x_values <- seq(min(cleaned_Data$e_1), max(cleaned_Data$e_1), length.out = 100)
y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4), length.out = 100)
grid <- expand.grid(e_1 = x_values, f_4 = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_e_1 * grid$e_1 + coef_f_4 * grid$f_4)
points_df <- data.frame(e_1 = cleaned_Data$e_1,
                        f_4 = cleaned_Data$f_4,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)

# --------------------------------Visualisation--------------------------------
ggplot() +
  geom_point(data = points_df, aes(x = e_1, y = f_4, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(x = e_1, y = f_4, z = Predicted_Probability), breaks = 0.5, color = "black", linetype = 4) +
  labs(x = "e/1", y = "f/4", color = "Probability") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) +
  ggtitle("Metacarpal")

# ------------------- Metatarsal -------------------
# -------------------Logistic Regression Prediction Function -------------------
# Set up the logistic regression by defining the coefficients
# ------------------------------------------------------------------------------  
# Logistic regression model  
df <- Metatarsal[!is.na(Metatarsal$MT_e1) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MT_LG_model1 <- glm(Type ~ MT_e1 + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

intercept <- coef(MT_LG_model1)[1]
coef_e_1 <- coef(MT_LG_model1)[2]
coef_f_4 <- coef(MT_LG_model1)[3]

logistic_prediction <- function(e_1, f_4) {
  intercept <- coef(MT_LG_model1)[1]
  coef_e_1 <- coef(MT_LG_model1)[2]
  coef_f_4 <- coef(MT_LG_model1)[3]
  odds <- exp(intercept + coef_e_1 * e_1 + coef_f_4 * f_4)
  probability <- odds / (1 + odds)
  
  return(probability)
}



# ---------------Import your archaeological dataset for prediction -------------
# Note: Replace 'your_file.csv' with the actual file path of your dataset. 
# ------------------------------------------------------------------------------
Archaeological_data <- read.csv("your_file.csv")

# ------------Calculate required shape ratios based on measurements------------
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names (e,D1,f and D4) match the columns in the actual dataset
# ------------------------------------------------------------------------------
# Calculate e_1 ratio
Archaeological_data$e_1 <- Archaeological_data$e/Archaeological_data$D1
# Calculate f_4 ratio
Archaeological_data$f_4 <- Archaeological_data$f/Archaeological_data$D4

# --------------------------------Data Cleaning--------------------------------
# Remove rows with missing values and infinite values in the dataset
# ------------------------------------------------------------------------------
cleaned_Data <- Archaeological_data[complete.cases(Archaeological_data[, c("e_1", "f_4")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$e_1), ]

# ------------------------------Model Prediction-------------------------------
predictions <- logistic_prediction(cleaned_Data$e_1, cleaned_Data$f_4)
cleaned_Data$Predicted_Probability <- predictions

# -----------------------Create elements for visualisation----------------------
# Note: To extend the decision boundary further, you may increase the multiplication factor
# For example: 
#x_values <- seq(min(cleaned_Data$e_1), max(cleaned_Data$e_1), length.out = 100)
#y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4), length.out = 100)
# ------------------------------------------------------------------------------
x_values <- seq(min(cleaned_Data$e_1), max(cleaned_Data$e_1), length.out = 100)
y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4), length.out = 100)
grid <- expand.grid(e_1 = x_values, f_4 = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_e_1 * grid$e_1 + coef_f_4 * grid$f_4)
points_df <- data.frame(e_1 = cleaned_Data$e_1,
                        f_4 = cleaned_Data$f_4,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)


# --------------------------------Visualisation--------------------------------
ggplot() +
  geom_point(data = points_df, aes(x = e_1, y = f_4, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(x = e_1, y = f_4, z = Predicted_Probability), breaks = 0.5, color = "black", linetype = 4) +
  labs(x = "e/1", y = "f/4", color = "Probability") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) +
  ggtitle("Metatarsal")

# ------------------- Metatarsal(Bd/GL) -------------------
# -------------------Logistic Regression Prediction Function -------------------
# Set up the logistic regression by defining the coefficients
# ------------------------------------------------------------------------------  
# Logistic regression model  
df <- Metatarsal[!is.na(Metatarsal$MT_Bd_GL) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MT_LG_model2 <- glm(Type ~ MT_Bd_GL + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))



logistic_prediction <- function(Bd_GL, f_4) {
  intercept <- coef(MT_LG_model2)[1]
  coef_Bd_GL <- coef(MT_LG_model2)[2]
  coef_f_4 <- coef(MT_LG_model2)[3]
  odds <- exp(intercept + coef_Bd_GL * Bd_GL + coef_f_4 * f_4)
  probability <- odds / (1 + odds)
  
  return(probability)
}

# ---------------Import your archaeological dataset for prediction -------------
# Note: Replace 'your_file.csv' with the actual file path of your dataset
# ------------------------------------------------------------------------------
Archaeological_data <- read.csv("your_file.csv")

# ------------Calculate required shape ratios based on measurements------------
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names (Bd,GL,f and D4) match the columns in the actual dataset
# ------------------------------------------------------------------------------
# Calculate Bd_GL ratio
Archaeological_data$Bd_GL <- Archaeological_data$Bd/Archaeological_data$GL
# Calculate f_4 ratio
Archaeological_data$f_4 <- Archaeological_data$f/Archaeological_data$D4

# --------------------------------Data Cleaning--------------------------------
# Remove rows with missing values and infinite values in the dataset
# ------------------------------------------------------------------------------
cleaned_Data <- Archaeological_data[complete.cases(Archaeological_data[, c("Bd_GL", "f_4")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$Bd_GL), ]

# ------------------------------Model Prediction-------------------------------
predictions <- logistic_prediction(cleaned_Data$Bd_GL, cleaned_Data$f_4)
cleaned_Data$Predicted_Probability <- predictions

# -----------------------Create elements for visualisation----------------------
# Note:To extend the decision boundary further, you may increase the multiplication factor
# For example: 
#x_values <- seq(min(cleaned_Data$Bd_GL), max(cleaned_Data$Bd_GL)*1.5, length.out = 100)
#y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4)*1.5, length.out = 100)
# ------------------------------------------------------------------------------
x_values <- seq(min(cleaned_Data$Bd_GL), max(cleaned_Data$Bd_GL), length.out = 100)
y_values <- seq(min(cleaned_Data$f_4), max(cleaned_Data$f_4), length.out = 100)
grid <- expand.grid(Bd_GL = x_values, f_4 = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_Bd_GL * grid$Bd_GL + coef_f_4 * grid$f_4)
points_df <- data.frame(Bd_GL = cleaned_Data$Bd_GL,
                        f_4 = cleaned_Data$f_4,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)

# --------------------------------Visualisation--------------------------------
ggplot() +
  geom_point(data = points_df, aes(x = Bd_GL, y = f_4, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(x = Bd_GL, y = f_4, z = Predicted_Probability), breaks = 0.5, color = "black", linetype = 4) +
  labs(x = "Bd/GL", y = "f/4", color = "Probability") +
  theme_dark()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) +
  ggtitle("Metatarsal")

# ------------------- First phalanx -------------------
# -------------------Logistic Regression Prediction Function -------------------
# ------------------------------------------------------------------------------  
# Logistic regression model  
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe), ]
df$Type <- as.numeric(df$Type == "Draught")
PH1_model <- glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))
# ------------------------------------------------------------------------------
intercept <- coef(PH1_model)[1]
coef_Bp_Dp <- coef(PH1_model)[2]
coef_Bd_Glpe <- coef(PH1_model)[3]


logistic_prediction <- function(Bp_Dp, Bd_Glpe) {
  intercept <- coef(PH1_model)[1]
  coef_Bp_Dp <- coef(PH1_model)[2]
  coef_Bd_Glpe <- coef(PH1_model)[3]
  odds <- exp(intercept + coef_Bp_Dp * Bp_Dp + coef_Bd_Glpe * Bd_Glpe)
  probability <- odds / (1 + odds)
  
  return(probability)
}

# ---------------Import your archaeological dataset for prediction -------------
# Note: Replace 'your_file.csv' with the actual file path of your dataset
# ------------------------------------------------------------------------------
Archaeological_data <- read.csv("your_file.csv") 

# ------------Calculate required shape ratios based on measurements------------
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names (Bp,Dp,Bd,GLpe) match the columns in the actual dataset
# Dp measurement is derived from von den Driesch's (1976: 99) description of equids
# ------------------------------------------------------------------------------
# Calculate Bp_Dp ratio
Archaeological_data$Bp_Dp <- Archaeological_data$Bp/Archaeological_data$Dp
# Calculate Bd_Glpe ratio
Archaeological_data$Bd_Glpe <- Archaeological_data$Bd/Archaeological_data$GLpe 

# --------------------------------Data Cleaning--------------------------------
# Remove rows with missing values and infinite values in the dataset
# ------------------------------------------------------------------------------
cleaned_Data <- Archaeological_data[complete.cases(Archaeological_data[, c("Bp_Dp", "Bd_Glpe")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$Bp_Dp), ]

# ------------------------------Model Prediction-------------------------------
predictions <- logistic_prediction(cleaned_Data$Bp_Dp, cleaned_Data$Bd_Glpe)
cleaned_Data$Predicted_Probability <- predictions

# -----------------------Create elements for visualisation----------------------
# Note:To extend the decision boundary further, you may increase the multiplication factor
# For example:
#x_values <- seq(min(cleaned_Data$Bp_Dp), max(cleaned_Data$Bp_Dp)*1.1, length.out = 100)
#y_values <- seq(min(cleaned_Data$Bd_Glpe), max(cleaned_Data$Bd_Glpe)*1.1, length.out = 100)
# ------------------------------------------------------------------------------
x_values <- seq(min(cleaned_Data$Bp_Dp), max(cleaned_Data$Bp_Dp), length.out = 100)
y_values <- seq(min(cleaned_Data$Bd_Glpe), max(cleaned_Data$Bd_Glpe), length.out = 100)
grid <- expand.grid(Bp_Dp = x_values, Bd_Glpe = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_Bp_Dp * grid$Bp_Dp + coef_Bd_Glpe * grid$Bd_Glpe)
points_df <- data.frame(Bp_Dp = cleaned_Data$Bp_Dp,
                        Bd_Glpe = cleaned_Data$Bd_Glpe,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)

# --------------------------------Visualisation--------------------------------
ggplot() +
  geom_point(data = points_df, aes(x = Bp_Dp, y = Bd_Glpe, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(x = Bp_Dp, y = Bd_Glpe, z = Predicted_Probability), 
               breaks = 0.5, color = "black", linetype = 4) +
  labs(x = "Bp/Dp", y = "Bd/GLpe", color = "Probability") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) +
  ggtitle("First phalanx")

# ------------------- Second phalanx -------------------
# -------------------Logistic Regression Prediction Function -------------------
# Set up the logistic regression by defining the coefficients
# ------------------------------------------------------------------------------ 
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe), ]
df$Type <- as.numeric(df$Type == "Draught")
PH2_model <-glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

# Logistic regression model 
intercept <- coef(PH2_model)[1]
coef_Bd_Glpe <- coef(PH2_model)[2]
coef_Bp_Glpe <- coef(PH2_model)[3]

logistic_prediction <- function(Bd_Glpe, Bp_Glpe) {
  intercept <- coef(PH2_model)[1]
  coef_Bd_Glpe <- coef(PH2_model)[2]
  coef_Bp_Glpe <- coef(PH2_model)[3]
  odds <- exp(intercept + coef_Bd_Glpe * Bd_Glpe + coef_Bp_Glpe * Bp_Glpe)
  probability <- odds / (1 + odds)
  return(probability)
}

# ---------------Import your archaeological dataset for prediction -------------
# Note: Replace 'your_file.csv' with the actual file path of your dataset
# ------------------------------------------------------------------------------
Archaeological_data <- read.csv("your_file.csv")

# ------------Calculate required shape ratios based on measurements------------
# Note: The variables used in the prediction depend on the naming conventions used in your own dataset
# Ensure that these variable names (Bp,Bd,GLpe) match the columns in the actual dataset
# Note that the GLpe in the second phalanx is equivalent to the GL measurement in von den Driesch (1976: 98)
# ------------------------------------------------------------------------------
# Calculate Bd_Glpe ratio
Archaeological_data$Bd_Glpe <- Archaeological_data$Bd/Archaeological_data$GLpe 
# Calculate Bp_Glpe ratio
Archaeological_data$Bp_Glpe <- Archaeological_data$Bp/Archaeological_data$GLpe 

# --------------------------------Data Cleaning--------------------------------
# Remove rows with missing values and infinite values in the dataset
# ------------------------------------------------------------------------------
cleaned_Data <- Archaeological_data[complete.cases(Archaeological_data[, c("Bd_Glpe", "Bp_Glpe")]), ]
cleaned_Data <- cleaned_Data[is.finite(cleaned_Data$Bd_Glpe), ]

# ------------------------------Model Prediction-------------------------------
predictions <- logistic_prediction(cleaned_Data$Bd_Glpe, cleaned_Data$Bp_Glpe)
cleaned_Data$Predicted_Probability <- predictions

# -----------------------Create elements for visualisation----------------------
# Note:To extend the decision boundary further, you may increase the multiplication factor
# For example:
#x_values <- seq(min(cleaned_Data$Bp_Glpe)*-0.02, max(cleaned_Data$Bp_Glpe)*1.2, length.out = 100)
#y_values <- seq(min(cleaned_Data$Bd_Glpe)*-0.02, max(cleaned_Data$Bd_Glpe)*1.2, length.out = 100)
# ------------------------------------------------------------------------------
x_values <- seq(min(cleaned_Data$Bp_Glpe), max(cleaned_Data$Bp_Glpe), length.out = 100)
y_values <- seq(min(cleaned_Data$Bd_Glpe), max(cleaned_Data$Bd_Glpe), length.out = 100)
grid <- expand.grid(Bd_Glpe = x_values, Bp_Glpe = y_values)
grid$Predicted_Probability <- plogis(intercept + coef_Bd_Glpe * grid$Bd_Glpe + coef_Bp_Glpe * grid$Bp_Glpe)
points_df <- data.frame(Bd_Glpe = cleaned_Data$Bd_Glpe,
                        Bp_Glpe = cleaned_Data$Bp_Glpe,
                        Predicted_Probability = cleaned_Data$Predicted_Probability)

# --------------------------------Visualisation--------------------------------
ggplot() +
  geom_point(data = points_df, aes(y = Bd_Glpe, x = Bp_Glpe, color = Predicted_Probability)) +
  scale_color_distiller(palette = "RdBu", direction = 1,breaks=c(0,0.5,1),labels=c("0",0.5,"1"),
                        limits=c(0,1)) +
  geom_contour(data = grid, aes(y = Bd_Glpe, x = Bp_Glpe, z = Predicted_Probability), 
               breaks = 0.5, color = "black", linetype = 4) +
  labs(y = "Bd/GLpe", x = "Bp/GLpe", color = "Probability") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(
          size = 13,
          hjust = 0.5,
          face = "bold",
          margin = margin(b = 5)
        )
  ) +
  ggtitle("Second phalanx")
