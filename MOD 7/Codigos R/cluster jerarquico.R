

version

# Instala el paquete
install.packages("installr")

# Actualiza R
library(installr)
updateR()

#####cluster jerárquico  ############################
# Load the data
data("USArrests")
head(USArrests)

# Standardize the data
df <- scale(USArrests)

# Show the first 6 rows
head(df, nrow = 6)

#Compute the dissimilarity matrix
# df = the standardized data
res.dist <- dist(df, method = "euclidean")
head(res.dist)

as.matrix(res.dist)[1:6, 1:6]

#elaboración del árbol jerárquico

res.hc <- hclust(d = res.dist, method = "ward.D2")
print(res.hc)
plot(res.hc)

#d: a dissimilarity structure as produced by the dist() function.
#method: The agglomeration (linkage) method to be used for computing 
#distance between clusters. Allowed values is one of "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median" or "centroid".

#se utiliza la función fviz_dend para visualizar el dendograma
library("factoextra")
fviz_dend(res.hc, cex = 0.5)

# cut the dendogram in differente groups
# cortar el árbol en 4 grupos
grp <- cutree(res.hc, k = 4)
print(grp)
head(grp, n = 4)

# Número de observaciones en cada cluster
table(grp)

# los nombres de las observaciones en el cluster 1
rownames(df)[grp == 4]
fviz_dend(res.hc, k=4, #corta en 4 grupos)
          cex=0.5, #tamaño letra
          k_colors=c("#2E9FDF","blue","#E7B800","#FC4E07"),
          color_labels_by_k=TRUE, #COLOR D ELOS GRUPOS
          rect=TRUE) #agregar rectangulo


##TAMBIEN ES POSIBLE VISUALIZAR EL DENDOGRAMA COMO SCATTER PLOT
fviz_cluster(list(data=df,cluster=grp),
             palette=c("#2E9FDF","blue","#E7B800","#FC4E07"),
             ellipse.type = "convex",
             repel=TRUE, #EVITAR OVERLAPPING
             show.clust.cent = FALSE, ggtheme = theme_minimal() )


##########ANÁLISIS DE CLUSTERS JERÁRQUICOS USANDO LA LIBRERIA CLUSTER
###########################################################


library("cluster")
# Agglomerative Nesting (Hierarchical Clustering)
res.agnes <- agnes(x = USArrests, # data matrix
                   stand = TRUE, # Standardize the data
                   metric = "euclidean", # metric for distance matrix
                   method = "ward" # Linkage method
)

# DIvisive ANAlysis Clustering
res.diana <- diana(x = USArrests, # data matrix
                   stand = TRUE, # standardize the data
                   metric = "euclidean" # metric for distance matrix
)

# Compute agnes()
fviz_dend(res.agnes, cex = 0.6, k = 4)

# Compute diana()
library(cluster)
res.diana <- diana(USArrests, stand = TRUE)


# Plot the dendrogram
library(factoextra)
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
)


         ########  COMPARACIÓN DE DENDOGRAMAS   ###########
df <- scale(USArrests)
# SELECCIONAR ALEATORIAMENTE 10 OBSERVACIONES DE LAS 50 DEL DATASET
set.seed(123)
ss <- sample(1:50, 10)
df <- df[ss,]

#install.packages("dendextend")
library(dendextend)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Create a list to hold dendrograms
dend_list <- dendlist(dend1, dend2)


# Align and plot two dendrograms side by side
dendlist(dend1, dend2) %>%
  untangle(method = "step1side") %>% # Find the best alignment layout
  tanglegram()                       # Draw the two dendrograms

#Correlation matrix between a list of dendrogams
#The function cor.dendlist() is used to compute "Baker" or 
#"Cophenetic" correlation matrix between a list of trees. The value can range
#between -1 to 1. With near 0 values meaning that the two trees are 
#not statistically similar.

# Cophenetic correlation matrix
cor.dendlist(dend_list, method = "cophenetic")
# Baker correlation matrix
cor.dendlist(dend_list, method = "baker")

#The correlation between two trees can be also computed as follow:
# Cophenetic correlation coefficient
cor_cophenetic(dend1, dend2)
# Baker correlation coefficient
cor_bakers_gamma(dend1, dend2)


#############################   HEATMAPS  ########################

df <- scale(mtcars)
head(df)

heatmap(df, scale = "row") # rojo valores más altos, amarillos más bajos
#scale: a character indicating if the values should be centered and scaled 
#in either the row direction or the column direction, or none. Allowed values are in c("row", "column", "none"). Default is "row".


#se pueden cambiar los colores
col<- colorRampPalette(c("red", "white", "blue"))(256)

library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)


# Use RColorBrewer color palette names
library("RColorBrewer")
col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
heatmap(df, scale = "none", col =  col, 
        RowSideColors = rep(c("blue", "pink"), each = 16),
        ColSideColors = c(rep("purple", 5), rep("orange", 6)))

#The function heatmap.2() [in gplots package] provides many extensions to the
#standard R heatmap() function presented in the previous section.
install.packages("gplots")
library("gplots")
heatmap.2(df, scale = "none", col = bluered(100), 
          trace = "none", density.info = "none")

#First, install the pheatmap package: install.packages("pheatmap"); 
#then type this:
install.packages("pheatmap")
library("pheatmap")
pheatmap(df, cutree_rows = 4)

install.packages("d3heatmap")
library("d3heatmap")
d3heatmap(scale(mtcars), colors = "RdYlBu",
          k_row = 4, # Number of groups in rows
          k_col = 2 # Number of groups in columns
)

head(iris)

df<-iris[,-5]


