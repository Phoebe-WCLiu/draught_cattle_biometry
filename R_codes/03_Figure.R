library(ggplot2)   
library(ggstar)    
library(dplyr) 
library(ggthemes) 

Metacarpal <- read.csv("Metacarpal.csv")
Metatarsal <- read.csv("Metatarsal.csv")
First_phalanx <- read.csv("First_phalanx.csv")
Second_phalanx <- read.csv("Second_phalanx.csv")

# abline function for decision boundary 
abline_fun <- function(model) {
  coef <- coef(model)
  function(x) {
    -(coef[1] + coef[2]*x) / coef[3]
  }
}

# -----------------------Figure. 4 -----------------------
df <- Metacarpal[!is.na(Metacarpal$MC_e1) & !is.na(Metacarpal$MC_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MC_LG_model <- glm(Type ~ MC_e1 + MC_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

df$ID[df$ID == "110"] <- "Yak"
df$ID[df$ID == "1924.4.28.1"] <- "Chartley"
df$ID[df$ID == "1980.2665"] <- "Chartley"

MC_Fig4<-ggplot(data = df, aes(x = MC_e1, y = MC_f4, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  geom_text(data = subset(df, ID %in% c("Yak", "Chartley", "Chartley", "Chartley")), aes(label = ID),
            hjust = 0.5, vjust = -0.8, show.legend = FALSE,color="black") +   
  geom_ribbon(stat = 'function', fun = abline_fun(MC_LG_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(MC_LG_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -MC_LG_model$coefficients[1] / MC_LG_model$coefficients[3], 
                  slope = -MC_LG_model$coefficients[2] / MC_LG_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "e/1", y = "f/4") +
  scale_shape_manual(values = c(21:25))+ 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Metacarpal") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) + 
  theme(legend.title = element_text(size = 13),
        plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 

ggsave("MC_Fig4.png", MC_Fig4, width = 8, height = 6, dpi = 300)

# -----------------------Figure. 6A -----------------------
df <- Metatarsal[!is.na(Metatarsal$MT_e1) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MT_LG_model1 <- glm(Type ~ MT_e1 + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

MT_Fig6A<-ggplot(data = df, aes(x = MT_e1, y = MT_f4, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  coord_cartesian(ylim = c(0.55, 0.8)) +
  geom_ribbon(stat = 'function', fun = abline_fun(MT_LG_model1),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(MT_LG_model1),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -MT_LG_model1$coefficients[1] / MT_LG_model1$coefficients[3], 
                  slope = -MT_LG_model1$coefficients[2] / MT_LG_model1$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "e/1", y = "f/4") +
  scale_shape_manual(values = c(21:25))+ 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0")) +
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Metatarsal") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) + 
  theme(legend.title = element_text(size = 13),
        plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 

ggsave("MT_Fig6A.png", MT_Fig6A, width = 8, height = 6, dpi = 300)


# -----------------------Figure. 6B -----------------------
df <- Metatarsal[!is.na(Metatarsal$MT_Bd_GL) & !is.na(Metatarsal$MT_f4), ]
df$Type <- as.numeric(df$Type == "Draught")
MT_LG_model2 <- glm(Type ~ MT_Bd_GL + MT_f4, data = df, family = binomial(link = "logit"), control = list(maxit = 100))


MT_Fig6B<-ggplot(data = df, aes(x = MT_Bd_GL, y = MT_f4, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  coord_cartesian(ylim = c(0.55, 0.8)) +
  geom_ribbon(stat = 'function', fun = abline_fun(MT_LG_model2),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(MT_LG_model2),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -MT_LG_model2$coefficients[1] / MT_LG_model2$coefficients[3], 
                  slope = -MT_LG_model2$coefficients[2] / MT_LG_model2$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bd/GL", y = "f/4") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0")) +
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  scale_shape_manual(values = c(21:25))+ 
  theme_base() +
  ggtitle("Metatarsal") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) + 
  theme(legend.title = element_text(size = 13),
        plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 

ggsave("MT_Fig6B.png", MT_Fig6B, width = 8, height = 6, dpi = 300)

# -----------------------Figure. 7 -----------------------
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) & !is.na(First_phalanx$PH1_Bd_Glpe), ]
df$Type <- as.numeric(df$Type == "Draught")
PH1_model <- glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

  PH1_Fig7<-ggplot(data = df, aes(x = PH1_Bp_Dp, y = PH1_Bd_Glpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  geom_ribbon(stat = 'function', fun = abline_fun(PH1_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH1_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH1_model$coefficients[1] / PH1_model$coefficients[3], 
                  slope = -PH1_model$coefficients[2] / PH1_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/Dp", y = "Bd/GLpe") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("First phalanx") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.spacing = unit(0.5, "cm"),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(
    shape = guide_legend(title = "", order = 0),  
    fill = guide_legend(order = 1), 
    linetype = guide_legend(title = "", order = 2)
  )
  
  ggsave("PH1_Fig7.png", PH1_Fig7, width = 8, height = 6, dpi = 300)
  

# -----------------------Figure. 8 -----------------------
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) & !is.na(Second_phalanx$PH2_Bd_GLpe), ]
df$Type <- as.numeric(df$Type == "Draught")
PH2_model <-glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

  PH2_Fig8<-ggplot(data = df, aes(x = PH2_Bp_GLpe, y = PH2_Bd_GLpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  coord_cartesian(ylim = c(0.38, 0.89)) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH2_model$coefficients[1] / PH2_model$coefficients[3], 
                  slope = -PH2_model$coefficients[2] / PH2_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/GLpe", y = "Bd/GLpe") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Second phalanx") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) +  
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 
  
  ggsave("PH2_Fig8.png", PH2_Fig8, width = 8, height = 6, dpi = 300)
  

# -----------------------Figure. S4A -----------------------
df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) &  !is.na(First_phalanx$PH1_Bd_Glpe) & First_phalanx$Side == "A", ]
df$Type <- as.numeric(df$Type == "Draught")
PH1A_model <-glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))


 PH1A_FigS4A<-ggplot(data = df, aes(x = PH1_Bp_Dp, y = PH1_Bd_Glpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  geom_ribbon(stat = 'function', fun = abline_fun(PH1A_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH1A_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH1A_model$coefficients[1] / PH1A_model$coefficients[3], 
                  slope = -PH1A_model$coefficients[2] / PH1A_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/Dp", y = "Bd/GLpe") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Anterior first phalanx") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.spacing = unit(0.5, "cm"),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(
    shape = guide_legend(title = "", order = 0),  
    fill = guide_legend(order = 1), 
    linetype = guide_legend(title = "", order = 2)
  )
 
 ggsave("PH1A_FigS4A.png", PH1A_FigS4A, width = 8, height = 6, dpi = 300)
 
# -----------------------Figure. S4B -----------------------

df <- First_phalanx[!is.na(First_phalanx$PH1_Bp_Dp) &  !is.na(First_phalanx$PH1_Bd_Glpe) & First_phalanx$Side == "P", ]
df$Type <- as.numeric(df$Type == "Draught")
PH1P_model <- glm(Type ~ PH1_Bp_Dp + PH1_Bd_Glpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

  PH1P_FigS4B<-ggplot(data = df, aes(x = PH1_Bp_Dp, y = PH1_Bd_Glpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  geom_ribbon(stat = 'function', fun = abline_fun(PH1P_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH1P_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH1P_model$coefficients[1] / PH1P_model$coefficients[3], 
                  slope = -PH1P_model$coefficients[2] / PH1P_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/Dp", y = "Bd/GLpe") +
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  coord_cartesian(ylim = c(0.35, 0.55)) +
  theme_base() +
  ggtitle("Posterior first phalanx") +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.spacing = unit(0.5, "cm"),
    legend.key.size = unit(1, 'cm'),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)) +
  guides(
    shape = guide_legend(title = "", order = 0),  
    fill = guide_legend(order = 1), 
    linetype = guide_legend(title = "", order = 2)
  )

  ggsave("PH1P_FigS4B.png", PH1P_FigS4B, width = 8, height = 6, dpi = 300)
  
# -----------------------Figure. S5A -----------------------

df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) &  !is.na(Second_phalanx$PH2_Bd_GLpe) & Second_phalanx$Side == "A", ]
df$Type <- as.numeric(df$Type == "Draught")
PH2A_model <- glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))
  PH2A_FigS5A<-ggplot(data = df, aes(x = PH2_Bp_GLpe, y = PH2_Bd_GLpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  coord_cartesian(ylim = c(0.38, 0.89)) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2A_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2A_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH2A_model$coefficients[1] / PH2A_model$coefficients[3], 
                  slope = -PH2A_model$coefficients[2] / PH2A_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/GLpe", y = "Bd/GLpe") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Anterior second phalanx") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) +  
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 

  ggsave("PH2A_FigS5A.png", PH2A_FigS5A, width = 8, height = 6, dpi = 300)
  
# -----------------------Figure. S5B -----------------------
df <- Second_phalanx[!is.na(Second_phalanx$PH2_Bp_GLpe) &  !is.na(Second_phalanx$PH2_Bd_GLpe) & Second_phalanx$Side == "P", ]
df$Type <- as.numeric(df$Type == "Draught")
PH2P_model <- glm(Type ~ PH2_Bp_GLpe + PH2_Bd_GLpe, data = df, family = binomial(link = "logit"), control = list(maxit = 100))

  PH2P_FigS5B<-ggplot(data = df, aes(x = PH2_Bp_GLpe, y = PH2_Bd_GLpe, shape = Sex, fill = as.factor(Type))) +
  geom_star(aes(starshape=Sex), size=3.5) +
  scale_shape_manual(values = c(21:25))+ 
  coord_cartesian(ylim = c(0.38, 0.89)) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2P_model),
              mapping = aes(ymin = -Inf, ymax = after_stat(y)),
              fill = "#FC4E07", alpha = 0.07) +
  geom_ribbon(stat = 'function', fun = abline_fun(PH2P_model),
              mapping = aes(ymin = after_stat(y), ymax = Inf),
              fill = "#00AFBB", alpha = 0.07) +
  geom_abline(aes(intercept = -PH2P_model$coefficients[1] / PH2P_model$coefficients[3], 
                  slope = -PH2P_model$coefficients[2] / PH2P_model$coefficients[3],
                  linetype = "Decision Boundary"), color = "black", size = 0.7) +
  labs(x = "Bp/GLpe", y = "Bd/GLpe") + 
  scale_fill_manual(values = c("#00AFBB", "#FC4E07"),  
                    name = "Type",  
                    labels = c("Draught cattle", "Non-draught cattle"),  
                    breaks = c("1", "0"))+
  scale_linetype_manual(values = c("twodash", 10, 10), 
                        name = "",
                        labels = c("Decision Boundary")) +
  theme_base() +
  ggtitle("Posterior second phalanx") +
  theme(legend.position = "right", 
        legend.key.width = unit(0.5, "cm"),
        legend.title = element_text(size = 13),
        legend.key.height = unit(0.5, "cm"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.spacing = unit(0.5, "cm")) +  
  guides(shape = guide_legend(title = "", order = 0),  
         colour = guide_legend(title = "", order = 2),  
         linetype = guide_legend(title = "", order = 3),  
         fill = guide_legend(order = 1)) 
  
  ggsave("PH2P_FigS5B.png", PH2P_FigS5B, width = 8, height = 6, dpi = 300)
  

  