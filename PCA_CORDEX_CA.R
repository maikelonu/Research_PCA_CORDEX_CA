# ////////////////////////////////////////////////////////////////////////////////////////////////////////////
# INSTITUTO TECNOLOGICO DE COSTA RICA
# Construction Engineering School
# MSc.Eng. Maikel Mendez Morales
# https://www.tec.ac.cr
# Email: maikel.mendez@gmail.com; mamendez@itcr.ac.cr
# https://orcid.org/0000-0003-1919-141X
# https://www.scopus.com/authid/detail.uri?authorId=51665581300
# https://scholar.google.com/citations?user=JnmSVFYAAAAJ&hl=en
# https://www.youtube.com/c/maikelmendez
# https://github.com/maikelonu
# Skype: maikel.mendez
# ////////////////////////////////////////////////////////////////////////////////////////////////////////////

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT TITLE:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT FIGURES:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# INFO: This script is intended for 
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT TITLE:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# INPUT FILES:
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# OUTPUT FILES:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------------
# MANUSCRIPT FIGURES:
# To be defined
#-------------------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------------------
# References
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
# https://www.r-bloggers.com/2021/05/principal-component-analysis-pca-in-r/
# http://www.sthda.com/english/wiki/wiki.php?id_contents=7851
# https://www.kaggle.com/code/agailloty/comprehensive-pca-with-r-using-factominer
# http://factominer.free.fr/factomethods/principal-components-analysis.html
# https://rpubs.com/gokul108/pca1
#-------------------------------------------------------------------------------------------------------------

# Workspace is cleared
gc(); rm(list = ls())

# Working directory is defined
setwd("~/Dropbox/Academics/IDF_CC_tool_CANADA/R_scripts/PCA_MEM_AIJS")

# Scientific notation is disabled
options(scipen=999)

# Start time is recorded
start.time <- Sys.time()

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: CRAN libraries are loaded
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
require(corrplot)
require(DescTools)
require(devtools)
require(dplyr)
require(factoextra)
require(FactoMineR)
require(ggfittext)
require(ggplot2)
require(ggpubr)
require(investr)
require(matrixStats)
require(openxlsx)
require(PerformanceAnalytics)
require(tidyr)
require(tidyverse)

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: Input Data for PCA
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# Weather station ETCCDI indices are loaded
df.metrics.bc <- read.delim("export.txt", header = TRUE, sep = "\t")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# BLOCK: PCA Analysis
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////

# A subset data.frame is created
subset01 <- df.metrics.bc

# If needed, a subset is modified to include relevant ETCCDI ONLY
# Otherwise, code line is commented
# subset01 <- subset01[, 1:12]

# Rownames are isolated
subset02 <- subset01[, -c(1)]

# data.frame rownames are included as variables
row.names(subset02) <- subset01$rcm_id

# A subset data.frame is created
subset03 <- subset02

# A correlation matrix is created
cor.mat <- round(cor(subset03), 2)

# A corrplot {corrplot} is created
corrplot(cor.mat,
         type="upper", 
         order="original", 
         tl.col="black", tl.srt=45,
         method="pie")

# A normalized matrix object is created for visualization purposes
subset03.normalized <- scale(subset03)

# Matrix object is requested to visually check for outliers
View(subset03.normalized)

# ----------------------------------------------------------------------------------------------------------------------------------
# SUBBLOCK: FactoMineR PCA
# ----------------------------------------------------------------------------------------------------------------------------------

# The goal of principal component analysis is to transform the initial variables 
# into a new set of variables which explain the variation in the data.

# PCA {FactoMineR} Principal Component Analysis function is requested
result.factor.NS <- PCA(X = subset03,
                        ncp = 5,
                        graph = TRUE,
                        col.w = NULL) # or NULL or w.vector

# ***-ANALYSIS-***
# This plots shows the structural relationship between the variables and the components.
# The projection of a variable vector onto the component axis allows to directly
# read the correlation between the variable and the component.
# The axis that represents PC 1 and PC 2 is the Pearson coefficient of correlation
# which goes from - 1 to 1.
# First PC-1 Read the correlation from left to right. The idea behind this plot 
# is to show in which direction the variables correlate.
# Second PC-2 Read the correlation from the bottom to the up.

# eigenvalue {factoextra} is called to extract and visualize the eigenvalues/variances of dimensions
fviz_eig(result.factor.NS,
         addlabels = TRUE,
         hjust = -0.3,
         barfill = "gray",
         barcolor = "black",
         linecolor = "#FC4E07",
         ncp = 5) +
  theme_bw(base_size = 18.0)

# A PCA plot is requested
plot(result.factor.NS, axes=c(1, 2), choix="var", col.var="blue",new.plot=TRUE)

# fviz_pca {factoextra} function is requested to visualize PCA
fviz_pca_var(result.factor.NS, repel = TRUE, labelsize = 6,
             col.var = "black", col.circle = "black",) +
  theme_bw(base_size = 22.0) +
  theme(axis.text.x = element_text(vjust = 0.5,angle = 90.0),
        panel.grid.major = element_line(colour = '#cccccc'))

# fviz_pca {factoextra} function is requested to visualize PCA
fviz_pca_var(result.factor.NS, col.var="contrib", # "contrib" or "cos"
             axes = c(1, 2),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE,
             labelsize = 4,
             ggtheme = theme_minimal())

# fviz_pca {factoextra} function is requested to visualize PCA
fviz_pca_var(result.factor.NS, col.var="contrib", # "contrib" or "cos"
             axes = c(1, 3),
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE,
             labelsize = 4,
             ggtheme = theme_minimal())

# An object summary is requested
summary(result.factor.NS)

# Eigenvalue percentage of variance cumulative is requested
result.factor.NS$eig

# ***-ANALYSIS-***
# An eigenvalue > 1 indicates that PCs account for more variance than accounted 
# by one of the original variables in standardized data.
# Hence, the components with eigenvalue > 1 are retained. 

# A list of matrices containing all the results for the active variables is requested
result.factor.NS$var

# A list of matrices containing the correlations is requested
result.factor.NS$var$cor

# Correlation matrix is isolated
df.corr <- as.data.frame(result.factor.NS$var$cor)

# Irrelevant PC components are deleted
df.corr <- df.corr[ ,1:3]

# PC components are renamed
names(df.corr) <- c("PC1","PC2","PC3")

# Names are added as variables
df.corr <- tibble::rownames_to_column(df.corr, var = "indicator")

# melt {reshape2}	is used to convert an object into a molten data frame
df.corr <- reshape2::melt(df.corr)

# Variable variable is rounded
df.corr$value <- round(df.corr$value,2 )

# A list of matrices containing the contributions is requested
result.factor.NS$var$contrib

# Correlation matrix is isolated
df.contri <- as.data.frame(result.factor.NS$var$contrib)

# Irrelevant PC components are deleted
df.contri <- df.contri[ ,1:3]

# PC components are renamed
names(df.contri) <- c("PC1","PC2","PC3")

# Names are added as variables
df.contri <- tibble::rownames_to_column(df.contri, var = "indicator")

# melt {reshape2}	is used to convert an object into a molten data frame
df.contri <- reshape2::melt(df.contri)

# Variable variable is rounded
df.contri$value <- round(df.contri$value, 2)

# A correlation heatmap for PCs is created
ggplot() +
  geom_tile(aes(y = indicator,x = variable,fill = value),data=df.corr,show.legend=TRUE) + 
  scale_fill_gradient2(midpoint = 0, mid="#ffffbf", low="#d73027", high="#4575b4", limits = c(-1,1)) +
  geom_text(aes(x = variable,y = indicator,label = value, fontface = 4), data=df.corr, 
            colour = '#252525',size = 5,hjust = 0.50,vjust = 0.50,parse = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.5,angle = 90.0),
        text=element_text(size=18,  family="serif"), legend.position="right") +
  scale_x_discrete(position = 'top') +
  ggtitle("Correlation - Heatmap") +
  xlab("Principal Component") +
  ylab("ETCCDI Index")

# A contribution heatmap for PCs is created
ggplot() +
  geom_tile(aes(y = indicator,x = variable,fill = value),data=df.contri,show.legend=TRUE) + 
  scale_fill_gradient2(midpoint = 7.5, mid="#ffffbf", low="#d73027", high="#4575b4", limits = c(0,15)) +
  geom_text(aes(x = variable,y = indicator,label = value, fontface = 4), data=df.contri, 
            colour = '#252525',size = 5,hjust = 0.50,vjust = 0.50,parse = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.5,angle = 90.0),
        text=element_text(size=18,  family="serif"), legend.position="right") +
  scale_x_discrete(position = 'top') +
  ggtitle("Contribution - Heatmap") +
  xlab("Principal Component") +
  ylab("ETCCDI Index")

# ***-ANALYSIS-***
# This data.frame gives the contribution of each variable in the building of the dimensions.
# The sum of all the contributions must be 100. Variables that are not correlated with any 
# PC or correlated with the last dimensions are variables with low contribution and might be 
# removed to simplify the overall analysis. The larger the value of the contribution, the more 
# the variable contributes to the component.

# A list of matrices containing all the results for cos2 is requested
result.factor.NS$var$cos2

# ***-ANALYSIS-***
# The quality of representation of the variables on factor map is called cos2
# (square cosine, squared coordinates). A high cos2 indicates a good representation 
# of the variable on the principal component. In this case the variable is positioned 
# close to the circumference of the correlation circle. A low cos2 indicates that 
# the variable is not perfectly represented by the component.

# a list of matrices containing all the results for the active individuals is requested
result.factor.NS$ind

# dimdesc {FactoMineR} is used to identify the most significantly associated variables
# with a given principal component. The output will be sorted by p-values.
desc.var <- dimdesc(result.factor.NS, axes = c(1,2), proba = 0.05)
desc.var$Dim.1
desc.var$Dim.2

# ----------------------------------------------------------------------------------------------------------------------------------
# SUBBLOCK: FactoMineR Hierarchical Clustering
# ----------------------------------------------------------------------------------------------------------------------------------

# HCPC{FactoMineR} Hierarchical Clustering on Principle Components function is requested
cluster01 <-  HCPC(res = result.factor.NS,
                   nb.clust = 4, # -1 Auto
                   metric = "euclidean",
                   method = "ward",
                   graph = FALSE,
                   cluster.CA = "rows" , # "rows" or "columns"
                   iter.max = 10)

# fviz_dend {factoextra} is used to visualize dendrogram generated by hierarchical clustering
fviz_dend(cluster01, 
          cex = 0.85,                     # Label size
          palette = c("#0073C2", "#EFC000", "#868686", "#CD534C"),
          rect = TRUE, rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 2.5,     # Augment the room for labels
          show_labels = TRUE,
          color_labels_by_k = TRUE,
          type = "rectangle")      

# Object cluster01 is plotted in 3D
plot(cluster01, choice = "3D.map", ind.names = TRUE)

# fviz_cluster {factoextra} is used to visualize individuals on the principal component map
fviz_cluster(cluster01,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE,  # Show cluster centers
             palette = c("#0073C2","#CD534C", "#868686", "#EFC000"),         # Color palette see. ggpubr::ggpar
             main = "Factor map",
             ellipse.type = "convex",
             axes = c(1, 2),
             ggtheme = theme_bw())

# fviz_cluster {factoextra} is used to visualize individuals on the principal component map
fviz_cluster(cluster01,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE,  # Show cluster centers
             palette = c("#0073C2","#CD534C", "#868686", "#EFC000"),         # Color palette see. ggpubr::ggpar
             main = "Factor map",
             ellipse.type = "convex",
             axes = c(1, 3),
             ggtheme = theme_bw())

# An object summary is requested
summary(cluster01)

# ***-ANALYSIS-***
# data.clust: The original data with a supplementary column called class containing the partition.
# desc.var: The variables describing clusters
# desc.ind: The more typical individuals of each cluster
# desc.axes: The axes describing clusters

# BEWARE !!! there is missing info here as NULLs are returned !!!
cluster01$desc.var      # WHY ????
cluster01$desc.axes     # WHY ????
cluster01$desc.ind$para # WHY ????

# fviz_cluster {factoextra} is used to visualize individuals on the principal component map
g.cluster01 <- fviz_cluster(cluster01)

# Alternatively, a ggplot2 object is created to visualize individuals on the principal component map
g.cluster01 +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10.0,min.n = 10.0)) +
  theme_bw(base_size = 18.0) +
  theme(axis.text.x = element_text(vjust = 0.5,angle = 90.0),
        panel.grid.major = element_line(colour = '#cccccc'))

# fviz_pca {factoextra} is called to visualize Principal Component Analysis using biplot
fviz_pca_biplot(result.factor.NS, repel = TRUE, labelsize = 6, col.var = "black")

# fviz_pca {factoextra} is called to visualize Principal Component Analysis using biplot
fviz_pca_biplot(result.factor.NS,  geom = "text")

# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
# END OF CODE
# /////////////////////////////////////////////////////////////////////////////////////////////////////////////
