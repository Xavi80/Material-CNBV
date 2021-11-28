#Programa para calcular distancias

##Preparación de los datos
# Subset of the data
set.seed(123)
ss <- sample(1:50, 15)   # Take 15 random rows
df <- USArrests[ss, ]    # Subset the 15 rows
df.scaled <- scale(df)   # Standardize the variables

head(df)

#Hacer el computo de la Distancia euclideana
dist.eucl <- dist(df.scaled, method = "euclidean")
print(dist.eucl)


# Poner en formato de Matriz
# seleccionar los primeras 5 columnas y renglones, y redondear valores
round(as.matrix(dist.eucl)[1:5, 1:5], 1)


# DISTANCIAS EN BASE A CORRELACIÓN
install.packages("factoextra")
install.packages("ggplot2")
library(ggplot)
library("factoextra")
dist.cor <- get_dist(df.scaled, method = "pearson")

# muestra un subconjunto
round(as.matrix(dist.cor)[1:3, 1:3], 1)


#elaboración de computo ante datos mixtos
install.packages("cluster")
library(cluster)
# Load data
data(flower)
head(flower, 3)

# Data structure
str(flower)

# Distance matrix
dd <- daisy(flower)
round(as.matrix(dd)[1:3, 1:3], 2)

##VISUALIZAR LAS MATRICES DE DISTANCIAS
library(factoextra)
fviz_dist(dist.eucl)

###### implementación de clusters KMEANS METHOD######################

##DATA PREPARATION
# Load data
data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)
# View the firt 3 rows
head(my_data, n = 10)

## DETERMINA EL NÚMERO ÓPTIMO DE CLUSTERS
library("factoextra")
install.packages("ggsignif")
install.packages("broom")
library(broom)
library(ggsignif)

# Elbow method
fviz_nbclust(my_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


# Silhouette method
fviz_nbclust(my_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
set.seed(123)
fviz_nbclust(my_data, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


set.seed(123)
km.res <- kmeans(my_data, 4, nstart = 25)
print(km.res)



# Visualize
library("factoextra")
fviz_cluster(km.res, data = my_data, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_test())


#### PAM CLUSTERING  ###################
# Compute PAM

#número óptimo de clusters
fviz_nbclust(my_data, pam,method = "silhouette")

library(cluster)
pam.res <- pam(my_data,2)
# Visualize
fviz_cluster(pam.res)
print(pam.res)
pam.res$medoids
pam.res$cluster

#se agregan las clasificaciones de los puntos a los datos originales
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)
# Cluster medoids: New Mexico, Nebraska
pam.res$medoids
#Cluster numbers
head(pam.res$clustering)

#visualización de los clusters con PAM
fviz_cluster(pam.res,palette=c("#00AFBB", "#FC4E07"),
             ellipse.type = "t", repel=TRUE, ggtheme =theme_classic() )
fviz_cluster(pam.res)


##########   CLARA ALGORITMO   #########################


set.seed(1234)
# Generate 500 objects, divided into 2 clusters.
df <- rbind(cbind(rnorm(200,0,8), rnorm(200,0,8)),
            cbind(rnorm(300,50,8), rnorm(300,50,8)))
head(df)

# Specify column and row names
colnames(df) <- c("x", "y")
rownames(df) <- paste0("S", 1:nrow(df))

# Previewing the data
head(df, nrow = 6)
##estimar el número óptimo de clusters
fviz_nbclust(df, clara,method = "silhouette")

#se genera el algoritmo
clara.res<-clara(df, 2, metric = "euclidean", stand = FALSE, 
      samples = 50, pamLike = FALSE)

print(clara.res)

clara.res$medoids
head(clara.res$clustering)


# de la misma forma se agrega a la base original el número del cluster asignado
dd <- cbind(df, cluster = clara.res$cluster)
head(dd, n = 4)


# visualizar los clusters mediante PAM
fviz_cluster(clara.res, palette=c("red","blue"),ellipse.type = "t",geom="point",
             pointsize = 1, ggtheme = theme_gray())


###practica utilice al algoritmo kmeans para visualizar los grupos formados
#en el dataset de iris
#recuerda seleccionar aquellas variables numericas solamente
df<-iris
head(iris)
df[,1:4]
head(df[,1:4])


summary(iris)
str(iris)
# se comparan los clusters estimados con los datos originales
table(irisCluster$cluster, df$Species)

fviz_nbclust(df[,1:4], kmeans,
             method = "silhouette")

fviz_nbclust(df[,1:4], kmeans,
             method = "gap_stat")

fviz_nbclust(df[,1:4], kmeans,
             method = "wss")

#se genera el algoritmo de K MEDIAS

set.seed(123)
km.res <- kmeans(df[,1:4], 4, nstart = 25)
print(km.res)

# Visualize
fviz_cluster(km.res, data = df[, -5],
             palette = c("#2E9FDF", "brown", "green", "red"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw() )


# Visualize
library("factoextra")
fviz_cluster(km.res, data = df[, -5], 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_test())


### pam datos iris
pam.res <- pam(df[, -5],4)
print(pam.res)
pam.res$medoids
fviz_cluster(pam.res, data = df[, -5], 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_test())



############ MODELOS VANAZADOS DE CLUSTERS  ##########################

df <- scale(USArrests)

# Compute hierarchical k-means clustering
library(factoextra)
res.hk <-hkmeans(df, 4)
# Elements returned by hkmeans()
names(res.hk)

# Print the results
res.hk

# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco", 
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

# Visualize the hkmeans final clusters
fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())




#############  fuzzy clustering (Clustering difuso)  #########

library(cluster)
df <- scale(USArrests)     # Standardize the data
res.fanny <- fanny(df, 2)  # Compute fuzzy clustering with k = 2
head(res.fanny$membership, 3) # Membership coefficients

res.fanny$coeff # Dunn's partition coefficient

head(res.fanny$clustering) # Observation groups

library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

#mientras más alto mejor.
fviz_silhouette(res.fanny, palette = "jco",
                ggtheme = theme_minimal())


#c-means
set.seed(123)

# Load the data
data("USArrests")
# Subset of USArrests
ss <- sample(1:50, 20)
df <- scale(USArrests[ss,])

# Compute fuzzy clustering
library(e1071)
cm <- cmeans(df, 4)
cm

# Membership coefficient
head(cm$membership)


# Visualize using corrplot
install.packages("corrplot")
library(corrplot)
corrplot(cm$membership, is.corr = FALSE)


# Observation groups/clusters
cm$cluster

library(factoextra)
fviz_cluster(list(data = df, cluster=cm$cluster), 
             ellipse.type = "norm",
             ellipse.level = 0.68,
             palette = "jco",
             ggtheme = theme_minimal())



#####################################################################

##############################DBSCAN CLUSTERING##################


install.packages("fpc")
install.packages("dbscan")
install.packages("factoextra")

library(fcp)
library(dbscan)
library(factoextra)


# Load the data 
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Compute DBSCAN using fpc package
library(fpc)
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)

# Plot DBSCAN results
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

#resultados
print(db)
