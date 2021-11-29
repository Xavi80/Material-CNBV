
##########  REGRESIÓN LOGISTICA ###############################
#version
#install.packages("installr")
#library(installr)
#updateR() 
#se instalan las librerias y se cargan los paquetes
install.packages("Rtools")
install.packages("tidiverse")
install.packages("broom")
library(tidyverse)
library(broom)
theme_set(theme_classic())

#construir un modelo de regresión logistica
# Load the data
install.packages("mlbench")
library(mlbench)
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)


#elaboramos un análisis d elas variables del dataset
str(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
pairs(PimaIndiansDiabetes2[,-1])
# se le agrega color
pairs(PimaIndiansDiabetes2[,-1],col=PimaIndiansDiabetes2$diabetes)

##Using ggplot
install.packages("ggplot2")
install.packages("GGally")
library(ggplot2)
library(GGally)

ggpairs(data=PimaIndiansDiabetes2, columns=1:8, 
        mapping=aes(color=PimaIndiansDiabetes2$diabetes,alpha=0.5) )

#se agregan líneas de regresión
ggpairs(data=PimaIndiansDiabetes2, columns=1:8, 
        mapping=aes(color=PimaIndiansDiabetes2$diabetes,alpha=0.5),
        lower = list(continuous = "smooth") )

ggcorr(PimaIndiansDiabetes2[,-1], palette = "RdBu", label = TRUE)

# Fit the logistic regression model
model <- glm(diabetes ~., data = PimaIndiansDiabetes2, 
             family = binomial)

print(model)
# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")

head(probabilities)

predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)


####Logistic regression diagnostics
##Linearity assumption
# Select only numeric predictors
mydata <- PimaIndiansDiabetes2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
install.packages("tidyr")
install.packages("tidyverse")
library(tidyverse)
library(tidyr)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#create scatterplots
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#valores de influencia (outliers)
plot(model, which = 4, id.n = 3)


# Extract model results
model.data <- augment(model) %>% 
  mutate(index = 1:n()) 

#The data for the top 3 largest values, according to the Cook's distance,
#can be displayed as follow:
model.data %>% top_n(3, .cooksd)

#Plot the standardized residuals:
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = diabetes), alpha = .5) +
  theme_bw()

#Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.std.resid) > 3)



###################Stepwise Logistic Regression Essentials in R

###################################################################
install.packages("caret")
install.packages("lattice")
library(tidyverse)
library(caret)
library(lattice)


# Load the data and remove NAs
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
# Inspect the data
sample_n(PimaIndiansDiabetes2, 3)

# se dividen los datos en datos de Entrenamiento y prueba
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]

#Computing stepwise logistique regression
library(MASS)
# Fit the model
model <- glm(diabetes ~., data = train.data, family = binomial) %>%
  stepAIC(trace = FALSE)
# Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Model accuracy
mean(predicted.classes==test.data$diabetes)

#Full logistic regression model
full.model <- glm(diabetes ~., data = train.data, family = binomial)
coef(full.model)

#Perform stepwise variable selection
#Select the most contributive variables:
library(MASS)
step.model <- full.model %>% stepAIC(trace = FALSE)
coef(step.model)

#Compare the full and the stepwise models
# Make predictions
probabilities <- full.model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Prediction accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)

###Prediction accuracy of the stepwise logistic regression model:
# Make predictions
probabilities <- predict(step.model, test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
# Prediction accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)
error <- mean(observed.classes != predicted.classes)
error

###confusion matrix
# Confusion matrix, number of cases
table(observed.classes, predicted.classes)

# Confusion matrix, proportion of cases
table(observed.classes, predicted.classes) %>% 
  prop.table() %>% round(digits = 3)


#Precision, Recall and Specificity
confusionMatrix(as.factor(predicted.classes), observed.classes)

### ROC CURVE
install.packages("pROC")
library(pROC)
# Compute roc
res.roc <- roc(observed.classes, probabilities)
plot.roc(res.roc, print.auc = TRUE)


#If we want a classifier model with a specificity of at least 60%, 
#then the sensitivity is about 0.88%. The corresponding probability threshold 
#can be extract as follow:

# Extract some interesting results
roc.data <- data_frame(
  thresholds = res.roc$thresholds,
  sensitivity = res.roc$sensitivities,
  specificity = res.roc$specificities
)
# Get the probality threshold for specificity = 0.6
roc.data %>% filter(specificity >= 0.6)

#The best threshold with the highest sum sensitivity + specificity can be
#printed as follow. There might be more than one threshold.
plot.roc(res.roc, print.auc = TRUE, print.thres = "best")
