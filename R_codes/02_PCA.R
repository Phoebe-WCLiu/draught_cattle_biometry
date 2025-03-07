library(ggplot2)
library(dplyr)
library(tibble)
library(ggthemes)
library(factoextra)
Metacarpal_PCA <- read.csv("Metacarpal_PCA.csv")
Metatarsal_PCA <- read.csv("Metatarsal_PCA.csv")
First_phalanx_PCA <- read.csv("First_phalanx_PCA.csv")
Second_phalanx_PCA <- read.csv("Second_phalanx_PCA.csv")
# -----------------------Figure. 3A -----------------------
Metacarpal_PCA_nomissing <- na.omit(Metacarpal_PCA)
Metacarpal_PCA$PCA_Type <- factor(Metacarpal_PCA$PCA_Type)
Metacarpal_PCA_pca_data <- Metacarpal_PCA_nomissing[, c("MC_a2", "MC_b5", "MC_a3", "MC_b6", "MC_ae2", "MC_e1", 
                                                  "MC_a8", "MC_b10", "MC_c8", "MC_d10", "MC_SDDD",
                                                  "MC_c7", "MC_d9", "MC_ae3", "MC_ae7", "MC_c2", "MC_d5", "MC_f4", 
                                                  "MC_a1", "MC_b4", "MC_a7", "MC_b9","MC_BpGL","MC_BatfGL","MC_BdGL")]
Metacarpal_PCA_pca <- prcomp(Metacarpal_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(Metacarpal_PCA_pca$x) %>% bind_cols(Metacarpal_PCA_nomissing)
explained_variance <- Metacarpal_PCA_pca$sdev^2 / sum(Metacarpal_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))

MC_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Metacarpal") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. S1A-B -----------------------
res.pca <- PCA(Metacarpal_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
MC_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
             fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('MC_a2' = 'a/2',
                              'MC_b10'='b/10',
                              'MC_a3' = 'a/3',
                              'MC_a8' = 'a/6',
                              'MC_a1' = 'a/1'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))

MC_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                         fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('MC_ae7' = '(a-e)/7',
                              'MC_ae2'='(a-e)/2',
                              'MC_ae3' = '(a-e)/3',
                              'MC_BpGL'='Bp/GL',
                              'MC_SDDD'='SD/DD')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. 3B -----------------------
Metatarsal_PCA_nomissing <- na.omit(Metatarsal_PCA)
Metatarsal_PCA$PCA_Type <- factor(Metatarsal_PCA$PCA_Type)
Metatarsal_PCA_pca_data <- Metatarsal_PCA_nomissing[, c("MT_a2", "MT_b5", "MT_a3", "MT_b6", "MT_ae2", "MT_e1", 
                                                        "MT_a8", "MT_b10", "MT_c8", "MT_d10", 
                                                        "MT_c7", "MT_d9", "MT_ae3", "MT_ae7", "MT_c2", "MT_d5", "MT_f4", 
                                                        "MT_a1", "MT_b4", "MT_a7", "MT_b9","MT_BpGL","MT_BatfGL","MT_BdGL")]
Metatarsal_PCA_pca <- prcomp(Metatarsal_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(Metatarsal_PCA_pca$x) %>% bind_cols(Metatarsal_PCA_nomissing)
explained_variance <- Metatarsal_PCA_pca$sdev^2 / sum(Metatarsal_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))


MT_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Metatarsal") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. S1C-D -----------------------
res.pca <- PCA(Metatarsal_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
MT_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                         fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('MT_a3' = 'a/3',
                              'MT_a1'='a/1',
                              'MT_a2' = 'a/2',
                              'MT_b4' = 'b/4',
                              'MT_b6' = 'b/6'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))

MT_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                         fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('MT_ae7' = '(a-e)/7',
                              'MT_ae2'='(a-e)/2',
                              'MT_ae3' = '(a-e)/3',
                              'MT_BpGL'='Bp/GL',
                              'MT_BatfGL'='BatF/GL')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. 3C -----------------------
First_phalanx_PCA_nomissing <- na.omit(First_phalanx_PCA)
First_phalanx_PCA$PCA_Type <- factor(First_phalanx_PCA$PCA_Type)
First_phalanx_PCA_pca_data <- First_phalanx_PCA_nomissing[, c("PH1_Bp_Dp","PH1_Bd_Glpe","PH1_Bp_Glpe","PH1_BP_BD","PH1_Dp_Glpe")]
First_phalanx_PCA_pca <- prcomp(First_phalanx_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(First_phalanx_PCA_pca$x) %>% bind_cols(First_phalanx_PCA_nomissing)
explained_variance <- First_phalanx_PCA_pca$sdev^2 / sum(First_phalanx_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
PH1_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("First phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S1E-F -----------------------
res.pca <- PCA(First_phalanx_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH1_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                          fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))
PH1_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                          fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S2A -----------------------
First_phalanx_PCA_nomissing <- subset(na.omit(First_phalanx_PCA), Side == "A")
First_phalanx_PCA$PCA_Type <- factor(First_phalanx_PCA$PCA_Type)
First_phalanx_A_PCA_pca_data <- First_phalanx_PCA_nomissing[First_phalanx_PCA_nomissing$Side == "A", 
                                                            c("PH1_Bp_Dp", "PH1_Bd_Glpe", "PH1_Bp_Glpe", 
                                                              "PH1_BP_BD", "PH1_Dp_Glpe")]
First_phalanx_A_PCA_pca <- prcomp(First_phalanx_A_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(First_phalanx_A_PCA_pca$x) %>% bind_cols(First_phalanx_PCA_nomissing)
explained_variance <- First_phalanx_A_PCA_pca$sdev^2 / sum(First_phalanx_A_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
PH1A_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Anterior first phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. S3A-B -----------------------

res.pca <- PCA(First_phalanx_A_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH1A_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))

PH1A_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S2B -----------------------
First_phalanx_PCA_nomissing <- subset(na.omit(First_phalanx_PCA), Side == "P")
First_phalanx_PCA$PCA_Type <- factor(First_phalanx_PCA$PCA_Type)
First_phalanx_P_PCA_pca_data <- First_phalanx_PCA_nomissing[First_phalanx_PCA_nomissing$Side == "P", 
                                                            c("PH1_Bp_Dp", "PH1_Bd_Glpe", "PH1_Bp_Glpe", 
                                                              "PH1_BP_BD", "PH1_Dp_Glpe")]
First_phalanx_P_PCA_pca <- prcomp(First_phalanx_P_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(First_phalanx_P_PCA_pca$x) %>% bind_cols(First_phalanx_PCA_nomissing)
explained_variance <- First_phalanx_P_PCA_pca$sdev^2 / sum(First_phalanx_P_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
PH1P_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Posterior first phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. S3C-D -----------------------

res.pca <- PCA(First_phalanx_P_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH1P_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))

PH1P_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH1_Bd_Glpe' = 'Bd/GLpe',
                              'PH1_Dp_Glpe'='Dp/GLpe',
                              'PH1_Bp_Glpe' = 'Bp/GLpe',
                              'PH1_BP_BD' = 'Bp/Bd',
                              'PH1_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. 3D -----------------------
Second_phalanx_PCA <- na.omit(Second_phalanx_PCA)
Second_phalanx_PCA$PCA_Type <- factor(Second_phalanx_PCA$PCA_Type)
Second_phalanx_PCA_pca_data <- Second_phalanx_PCA[, c("PH2_Bp_GLpe","PH2_Bd_GLpe","PH2_BP_BD","PH2_Dp_Glpe")]
Second_phalanx_PCA_pca <- prcomp(Second_phalanx_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(Second_phalanx_PCA_pca$x) %>% bind_cols(Second_phalanx_PCA)
explained_variance <- Second_phalanx_PCA_pca$sdev^2 / sum(Second_phalanx_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
PH2_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Second phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S1G-H -----------------------
res.pca <- PCA(Second_phalanx_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH2_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                          fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))
PH2_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                          fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S2C -----------------------
Second_phalanx_PCA_nomissing <- subset(na.omit(Second_phalanx_PCA), Side == "A")
Second_phalanx_PCA$PCA_Type <- factor(Second_phalanx_PCA$PCA_Type)
Second_phalanx_PCA_pca_data <- Second_phalanx_PCA_nomissing[, c("PH2_Bp_GLpe","PH2_Bd_GLpe","PH2_BP_BD","PH2_Dp_Glpe")]
Second_phalanx_PCA_pca <- prcomp(Second_phalanx_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(Second_phalanx_PCA_pca$x) %>% bind_cols(Second_phalanx_PCA_nomissing)
explained_variance <- Second_phalanx_PCA_pca$sdev^2 / sum(Second_phalanx_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
PH2A_PCA<-ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Anterior second phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S1E-F -----------------------
res.pca <- PCA(Second_phalanx_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH2A_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))
PH2A_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )

# -----------------------Figure. S2D -----------------------
Second_phalanx_PCA_nomissing <- subset(na.omit(Second_phalanx_PCA), Side == "P")
Second_phalanx_PCA$PCA_Type <- factor(Second_phalanx_PCA$PCA_Type)
Second_phalanx_PCA_pca_data <- Second_phalanx_PCA_nomissing[, c("PH2_Bp_GLpe","PH2_Bd_GLpe","PH2_BP_BD","PH2_Dp_Glpe")]
Second_phalanx_PCA_pca <- prcomp(Second_phalanx_PCA_pca_data, scale. = TRUE)
pca_points <- as_tibble(Second_phalanx_PCA_pca$x) %>% bind_cols(Second_phalanx_PCA_nomissing)
explained_variance <- Second_phalanx_PCA_pca$sdev^2 / sum(Second_phalanx_PCA_pca$sdev^2)
percent_variance_PC1 <- round(explained_variance[1] * 100, 1)
percent_variance_PC2 <- round(explained_variance[2] * 100, 1)
pca_hull <- 
  pca_points %>% 
  group_by(PCA_Type) %>% 
  slice(chull(PC1, PC2))
  ggplot(pca_points, aes(x = PC1, y = PC2,fill = as.factor(PCA_Type))) +
  geom_point(size = 3.5, color = "black",shape=21) +
  geom_polygon(data = pca_hull,
               aes(fill = PCA_Type,
                   colour = PCA_Type),
               alpha = 0.5,
               show.legend = FALSE)+
  scale_color_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#984ea3", "#a65628", "#e41a1c","#ff7f00")) +
  labs( x = paste0("PC1 (", percent_variance_PC1, "%)"),
        y = paste0("PC2 (", percent_variance_PC2, "%)")) +
  ggtitle("Posterior second phalanx") +
  theme_base() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
# -----------------------Figure. S1G-H -----------------------
res.pca <- PCA(Second_phalanx_PCA_pca_data, graph = FALSE)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 40)
PH2P_PCA_PC1<-fviz_contrib(res.pca, choice = "var", axes = 1, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp'))+
  theme_base() +
  ggtitle("Contribution of variables to PC1") + 
  theme(
    axis.title.x = element_blank(),         
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0))
PH2P_PCA_PC2<-fviz_contrib(res.pca, choice = "var", axes = 2, top = 5,
                           fill = "lightgray", color = "black") +
  scale_x_discrete(labels = c('PH2_Bd_GLpe' = 'Bd/GLpe',
                              'PH2_Bp_GLpe'='Bp/GLpe',
                              'PH2_BP_BD' = 'Bp/Bd',
                              'PH2_Dp_Glpe' = 'Dp/GLpe',
                              'PH2_Bp_Dp' = 'Bp/Dp')) +
  theme_base() +
  ggtitle("Contribution of variables to PC2") + 
  theme(
    axis.title.x = element_blank(),   
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 13),
    plot.caption = element_text(size = 9, color = "black", face = "italic", hjust = 0)
  )
