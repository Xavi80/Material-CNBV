
#####   ANÁLISIS DE COMPONENTES PRINCIPALES   #################


install.packages("factoextra")
install.packages("ggplot2")
library(ggplot2)
if(!require(devtools)) install.packages("devtools")

library("factoextra")
data(decathlon2)
head(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])

res.pca <- prcomp(decathlon2.active, scale = TRUE)
#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.
fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables. Positive correlated variables point to the same side of the
#plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)


##########Access to the PCA results

library(factoextra)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 


#####################################   OTRA FORMA DE HACERLO ##############
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

data(decathlon2)
head(decathlon2)

#PARTIMOS EL DATASET Y SOLO TOMAMOS LOS PRIMEROS 23 RENGLONES Y 10 COLUMNAS
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)

#SE ESTANDARIZAN LOS DATOS
PCA(decathlon2.active, scale.unit = TRUE, ncp = 5, graph = TRUE)

library("FactoMineR")
res.pca <- PCA(decathlon2.active, graph = FALSE)
print(res.pca)


#visualización e interpretación
library("factoextra")
eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))


var <- get_pca_var(res.pca)
var

# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)

#calidad de representación
head(var$cos2, 4)

#You can visualize the cos2 of variables on all the dimensions using the corrplot package:
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)



# Color by cos2 values: quality on the factor map
#mientras más una variable se acerque al circulo de la correlación, es mejor
#ya que indica que es más importante dentro de los componentes principales
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# contributions
#Que tanto 
head(var$contrib, 4)

#para ver que variables contribuyen con mayor correlación hacia cada componente principal.
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


#total de la contribución al componente principal es:
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)


#The most important (or, contributing) variables can be highlighted on the correlation plot as follow:
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)

############  analisis de los individuos  ###################
ind <- get_pca_ind(res.pca)
ind
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

#cambiar tamaño
fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#cambiar el tamaño y el color
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


# crear la contribución en barras (pc1)
fviz_cos2(res.pca, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)


#######  predicción usando PCA    ####################
# Data for the supplementary individuals
ind.sup <- decathlon2[24:27, 1:10]
ind.sup[, 1:6]


#Predict the coordinates of new individuals data. Use the R base function predict():
ind.sup.coord <- predict(res.pca, newdata = ind.sup)
head(ind.sup.coord)

#Graph of individuals including the supplementary individuals:
# Plot of active individuals
p <- fviz_pca_ind(res.pca, repel = TRUE)

print(p)
