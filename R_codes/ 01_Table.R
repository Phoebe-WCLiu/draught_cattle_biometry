#Table 1. Results of Mann-Whitney U test carried out on each ratio of measurements between type of cattle (draught and non-draught).
Metacarpal <- read.csv("Metacarpal.csv")
Metatarsal <- read.csv("Metatarsal.csv")
First_phalanx <- read.csv("First_phalanx.csv")
Second_phalanx <- read.csv("Second_phalanx.csv")

wilcox.test(MC_e1 ~ Type, data = Metacarpal, exact = FALSE)
wilcox.test(MC_f4 ~ Type, data = Metacarpal, exact = FALSE)
wilcox.test(MT_e1 ~ Type, data = Metatarsal, exact = FALSE)
wilcox.test(MT_f4 ~ Type, data = Metatarsal, exact = FALSE)
wilcox.test(MT_Bd_GL ~ Type, data = Metatarsal, exact = FALSE)
wilcox.test(PH1_Bp_Dp ~ Type, data = First_phalanx, exact = FALSE)
wilcox.test(PH1_Bd_Glpe ~ Type, data = First_phalanx, exact = FALSE)
wilcox.test(PH2_Bp_GLpe ~ Type, data = Second_phalanx, exact = FALSE)
wilcox.test(PH2_Bd_GLpe ~ Type, data = Second_phalanx, exact = FALSE)

#Table S4.
#Anterior first phalanx 
side_A <- subset(First_phalanx, Side == "A")
wilcox.test(PH1_Bp_Dp ~ Type, data = side_A, exact = FALSE)
wilcox.test(PH1_Bd_Glpe ~ Type, data = side_A, exact = FALSE)
#Posterior first phalanx 
side_P <- subset(First_phalanx, Side == "P")
wilcox.test(PH1_Bp_Dp ~ Type, data = side_P, exact = FALSE)
wilcox.test(PH1_Bd_Glpe ~ Type, data = side_P, exact = FALSE)
#Anterior second phalanx 
side_A <- subset(Second_phalanx, Side == "A")
wilcox.test(PH2_Bp_GLpe ~ Type, data = side_A, exact = FALSE)
wilcox.test(PH2_Bd_GLpe ~ Type, data = side_A, exact = FALSE)
#Posterior second phalanx 
side_P <- subset(Second_phalanx, Side == "P")
wilcox.test(PH2_Bp_GLpe ~ Type, data = side_P, exact = FALSE)
wilcox.test(PH2_Bd_GLpe ~ Type, data = side_P, exact = FALSE)

#Table 2.Spearman's rank correlation analysis between age and shape ratios.
spearman_MC <- cor.test(Metacarpal$MC_e1, Metacarpal$Age, method = "spearman")
spearman_MC
spearman_MC <- cor.test(Metacarpal$MC_f4, Metacarpal$Age, method = "spearman")
spearman_MC
spearman_MT <- cor.test(Metatarsal$MT_e1, Metatarsal$Age, method = "spearman")
spearman_MT
spearman_MT <- cor.test(Metatarsal$MT_f4, Metatarsal$Age, method = "spearman")
spearman_MT
spearman_MT <- cor.test(Metatarsal$MT_Bd_GL, Metatarsal$Age, method = "spearman")
spearman_MT
spearman_PH1 <- cor.test(First_phalanx$PH1_Bp_Dp, First_phalanx$Age, method = "spearman")
spearman_PH1
spearman_PH1 <- cor.test(First_phalanx$PH1_Bd_Glpe, First_phalanx$Age, method = "spearman")
spearman_PH1
spearman_PH2 <- cor.test(Second_phalanx$PH2_Bp_GLpe, Second_phalanx$Age, method = "spearman")
spearman_PH2
spearman_PH2 <- cor.test(Second_phalanx$PH2_Bd_GLpe, Second_phalanx$Age, method = "spearman")
spearman_PH2

#Table 3.Multiple linear regression 
MT_lr <- lm(MT_e1 ~ Age + Type, data = Metatarsal)
summary(MT_lr )
MT_lr <- lm(MT_f4 ~ Age + Type, data = Metatarsal)
summary(MT_lr )
PH1_lr <- lm(PH1_Bd_Glpe ~ Age + Type, data = First_phalanx)
summary(PH1_lr )
PH2_lr <- lm(PH2_Bp_GLpe ~ Age + Type, data = Second_phalanx)
summary(PH2_lr )
PH2_lr <- lm(PH2_Bd_GLpe ~ Age + Type, data = Second_phalanx)
summary(PH2_lr )

#Table S5.Spearman's rank correlation analysis between age and shape ratios.
#Anterior first phalanx 
side_A <- subset(First_phalanx, Side == "A")
spearman_PH1 <- cor.test(side_A$PH1_Bp_Dp, side_A$Age, method = "spearman")
spearman_PH1
spearman_PH1 <- cor.test(side_A$PH1_Bd_Glpe, side_A$Age, method = "spearman")
spearman_PH1
#Posterior first phalanx 
side_P <- subset(First_phalanx, Side == "P")
spearman_PH1 <- cor.test(side_P$PH1_Bp_Dp, side_P$Age, method = "spearman")
spearman_PH1
spearman_PH1 <- cor.test(side_P$PH1_Bd_Glpe, side_P$Age, method = "spearman")
spearman_PH1
#Anterior second phalanx 
side_A <- subset(Second_phalanx, Side == "A")
spearman_PH2 <- cor.test(side_A$PH2_Bp_GLpe, side_A$Age, method = "spearman")
spearman_PH2
spearman_PH2 <- cor.test(side_A$PH2_Bd_GLpe, side_A$Age, method = "spearman")
spearman_PH2
#Posterior second phalanx 
side_P <- subset(Second_phalanx, Side == "P")
spearman_PH2 <- cor.test(side_P$PH2_Bp_GLpe, side_P$Age, method = "spearman")
spearman_PH2
spearman_PH2 <- cor.test(side_P$PH2_Bd_GLpe, side_P$Age, method = "spearman")
spearman_PH2

#Table S6.Multiple linear regression 
#Anterior first phalanx 
side_A <- subset(First_phalanx, Side == "A")
PH1A_lr <- lm(PH1_Bd_Glpe ~ Age + Type, data = side_A)
summary(PH1A_lr)
#Posterior first phalanx 
side_P <- subset(First_phalanx, Side == "P")
PH1P_lr <- lm(PH1_Bd_Glpe ~ Age + Type, data = side_P)
summary(PH1P_lr)
#Posterior second phalanx 
side_P <- subset(Second_phalanx, Side == "P")
PH2P_lr <- lm(PH2_Bd_GLpe ~ Age + Type, data = side_P)
summary(PH2P_lr)

#Table 4.Brier score
#Metacarpal
df <- Metacarpal[!is.na(Metacarpal$MC_e1) & !is.na(Metacarpal$MC_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ MC_e1 + MC_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Metatarsal
df <- Metatarsal[!is.na(Metatarsal$MT_Bd_GL) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ MT_Bd_GL + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Metatarsal
df <- Metatarsal[!is.na(Metatarsal$MT_e1) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ MT_e1 + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#First phalanx
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Second phalanx
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore

#Table S7.Brier score
#Anterior first phalanx 
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe) & First_phalanx$Side == "A", ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Posterior first phalanx 
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe) & First_phalanx$Side == "P", ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Anterior second phalanx 
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe) & Second_phalanx$Side == "A", ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore
#Posterior second phalanx 
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe) & Second_phalanx$Side == "P", ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
pred.prob <- predict(model,type='response')
brierScore <- mean((pred.prob- df$Type)^2)
brierScore

#Table 5. Model performance metrics.
#Metacarpal
df <- Metacarpal[!is.na(Metacarpal$MC_e1) & !is.na(Metacarpal$MC_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ MC_e1 + MC_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
predicted_probs <- predict(model, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Draught", "Non-draught")
confusion_matrix <- table(Actual = df$Type, Predicted = predicted_classes)
# True Positive
TP <- confusion_matrix["1", "Draught"]
# True Negative (TN)
TN <- confusion_matrix["0", "Non-draught"]
# False Positive (FP)
FP <- confusion_matrix["0", "Draught"]
#  False Negative (FN)
FN <- confusion_matrix["1", "Non-draught"]
# Sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
# Specificity (True Negative Rate)
specificity <- TN / (TN + FP)
#  Balanced Accuracy
balanced_accuracy <- (sensitivity + specificity) / 2
#  F_score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F_score <- 2 * (precision * recall) / (precision + recall)

#Metatarsal
df <- Metatarsal[!is.na(Metatarsal$MT_e1) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ MT_e1 + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
predicted_probs <- predict(model, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Draught", "Non-draught")
confusion_matrix <- table(Actual = df$Type, Predicted = predicted_classes)
# True Positive
TP <- confusion_matrix["1", "Draught"]
# True Negative (TN)
TN <- confusion_matrix["0", "Non-draught"]
# False Positive (FP)
FP <- confusion_matrix["0", "Draught"]
#  False Negative (FN)
FN <- confusion_matrix["1", "Non-draught"]
# Sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
# Specificity (True Negative Rate)
specificity <- TN / (TN + FP)
#  Balanced Accuracy
balanced_accuracy <- (sensitivity + specificity) / 2
#  F_score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)

#First phalanx
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
predicted_probs <- predict(model, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Draught", "Non-draught")
confusion_matrix <- table(Actual = df$Type, Predicted = predicted_classes)
# True Positive
TP <- confusion_matrix["1", "Draught"]
# True Negative (TN)
TN <- confusion_matrix["0", "Non-draught"]
# False Positive (FP)
FP <- confusion_matrix["0", "Draught"]
#  False Negative (FN)
FN <- confusion_matrix["1", "Non-draught"]
# Sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
# Specificity (True Negative Rate)
specificity <- TN / (TN + FP)
#  Balanced Accuracy
balanced_accuracy <- (sensitivity + specificity) / 2
#  F_score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F_score <- 2 * (precision * recall) / (precision + recall)

#Second phalanx
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe), ]
df$Type <- as.numeric(df$Type == "Draught")
model <- suppressWarnings(glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100)))
predicted_probs <- predict(model, newdata = df, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Draught", "Non-draught")
confusion_matrix <- table(Actual = df$Type, Predicted = predicted_classes)
# True Positive
TP <- confusion_matrix["1", "Draught"]
# True Negative (TN)
TN <- confusion_matrix["0", "Non-draught"]
# False Positive (FP)
FP <- confusion_matrix["0", "Draught"]
#  False Negative (FN)
FN <- confusion_matrix["1", "Non-draught"]
# Sensitivity (True Positive Rate)
sensitivity <- TP / (TP + FN)
# Specificity (True Negative Rate)
specificity <- TN / (TN + FP)
#  Balanced Accuracy
balanced_accuracy <- (sensitivity + specificity) / 2
#  F_score
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
F_score <- 2 * (precision * recall) / (precision + recall)
