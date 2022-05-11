#########Projet final de STAT203#############################
#setwd("~/ENSTA 2A/STA203")
setwd("~/Documents/STA203/STA203")

library("openxlsx")
rm(list=objects()) ; graphics.off()
data = read.xlsx("Raisin.xlsx",colNames=TRUE)

#PARTIE 1 

#question 1

#Analyse uni-variee : boxplot
p = ncol(data)
boxplot(data[,c(1,5)]) 
boxplot(data[,-p])
boxplot(data[,c(2,3)]) #axis length
boxplot(data[,c(4,6)])#excentricite extent
boxplot(data[,c(7)]) #perimetre

#Analyse bi-vari?e

#matrice de correlation
round(cor(data[-p]),2)
library(corrplot)
corrplot(cor(data[,-p]), method="circle")

pairs(data[,-p],col=ifelse(data$Class=="Kecimen", "black", "red"))

# pour regarder de plus pr?t les r?partitions
ggplot(data,aes(x=data$Area,y=data$Perimeter,color=as.factor(data$Class))) + geom_point()


#question 2 : ACP

library("FactoMineR")

data$Class = as.factor(data$Class)
res=PCA(data,quali.sup = p, graph = TRUE)
plt1 = plot(res,axes=c(1,2),choix="ind",label="quali", habillage=p) +
  theme(legend.position = "none") 
plt2 = plot(res,axes=c(2,3),choix="ind",label="quali", habillage=p) 
plt4 = plot(res,axes=c(6,5),choix="ind",label="quali", habillage=p) 
plt4
plt3 = plot(res,choix="var")
cowplot::plot_grid(plt1, plt4, nrow=1, ncol=2)

barplot(res$eig[,1])

res$var$cos2[,1:2]
# > res$var$cos2[,1:2]
#                  Dim.1       Dim.2
# Area            0.97109063 0.019587803
# MajorAxisLength 0.94935877 0.027110793
# MinorAxisLength 0.73266049 0.204270120
# Eccentricity    0.19907684 0.542191863
# ConvexArea      0.98262357 0.011155576
# Extent          0.01535408 0.647175333
# Perimeter       0.98212422 0.001696952


#Question 3.1 

# K-means
n = nrow(data)
set.seed(1)
centres = data[sample(1:n,2),-p]
centres = data[1:2,-p]
res.kmeans = kmeans (data[-p],centres)

#erreur de classification
table(pred=res.kmeans$cluster,vrai=data$Class)

#normalisation des variables

data.scale= scale(data[-p])
centres.scale = data.scale[sample(1:n,2),-p]
centres.scale = data.scale[1:2,-p]
res.kmeans.scale = kmeans(data.scale,centres.scale)

#erreur de classification avec var normalisees
table(pred=res.kmeans.scale$cluster,vrai=data$Class)

#question3.2- Classification hierarchique

table_distances=dist(data[,-p])
mon_arbre=hclust(table_distances)
#mon_arbre=hclust(table_distances,method="ward.D")
mon_arbre
#Cluster method   : ward.D 
#Distance         : euclidean 
#Number of objects: 900 

plot(mon_arbre, cex=0.5)

#normalisation des variables

table_distances2=dist(data.scale)
mon_arbre2=hclust(table_distances2)
mon_arbre2
plot(mon_arbre2, cex=0.5)

barplot(rev(mon_arbre2$height)[1:15],main="diagramme des hauteurs")
abline(h=12)       # 2 classes

cutree(mon_arbre2,h=12);
plot(as.factor(cutree(mon_arbre2,h=12)))

#erreur de classification
table(pred=as.factor(cutree(mon_arbre2,h=12)),vrai=data$Class)


#Question 4 

######################PARTIE 2 

#question2

set.seed(1)
n = nrow(data)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
data_train = cbind(Class = data$Class,as.data.frame(res$ind$coord))[train,]
data_test = cbind(Class = data$Class,as.data.frame(res$ind$coord))[-train,]

#mod?le complet


res.glm.complet = glm(Class~.,
              family=binomial,data=data_train)
summary(res.glm.complet)

probabilities = res.glm.complet %>% predict(data_test, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
predicted.classes
mean(predicted.classes == data_test$Class) #0.865406


#mod?le deux composantes principales

comp1 = res$ind$coord[train,1]
comp2 = res$ind$coord[train,2]
new = as.data.frame(cbind(Class=data_train$Class,dim1 = comp1,dim2 = comp2))
res.glm.prin = glm(Class~(Dim.1+Dim.2) ,family=binomial,data=data_train )

summary(res.glm.prin)

probabilities = res.glm.prin %>% predict(data_test, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
predicted.classes
mean(predicted.classes == data_test$Class) #0.8709677

library(tidyverse)
library(caret)
data_train %>%
  mutate(prob = ifelse(Class == "Kecimen", 1, 0)) %>%
  ggplot(aes(Dim.1+Dim.2, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Dim1 + Dim2",
    y = "Probability of being Kecimen"
  )


# mod?le par AIC
glm = glm(Class~.,family=binomial,data=data_train)
selection=MASS::stepAIC(glm,direction="both")
glm.AIC = glm(Class~Dim.1+Dim.2 + Dim.3 + Dim.5,family=binomial,data=data_train)

probabilities = glm.AIC %>% predict(data_test, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
predicted.classes
mean(predicted.classes == data_test$Class) #0.862069


# mod?le obtenu par r?gression p?nalis?e lasso

library(glmnet)

x = as.matrix(data_train[,-1])
y = data_train$Class
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
lasso.fit = glmnet(x,y,family = "binomial",alpha=1,,lambda=cv.lasso$lambda.min)

x.test <- model.matrix(Class ~., data_test)[,-1] #on retire l'intercept!
probabilities = lasso.fit %>% predict(newx=x.test)
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
predicted.classes
mean(predicted.classes == data_test$Class) #0.8464961


#question 3 : SVM

#SVM lin?aire

library(e1071)
co= cbind(0.001, 0.01, 0.1, 1, 5, 10, 100)
obj = tune.svm(Class~Dim.1+Dim.2, data=data_train , kernel ='linear', scale =FALSE,tunecontrol=tune.control(cross=2), cost = co) #trouver meilleur cout
res.svm = svm(Class~Dim.1+Dim.2, data=data_train , kernel ='linear', scale =FALSE,cost= 1)
summary(res.svm)
plot(res.svm, data_train,Dim.2~Dim.1 )

table(pred=predict(res.svm),vrai=data_train$Class)
# pred      Besni Kecimen
# Besni     250      37
# Kecimen    43     270


#SVM noyau polynomial 


obj = tune.svm(Class~Dim.1+Dim.2, data=data_train , kernel ='linear', scale =FALSE,tunecontrol=tune.control(cross=2), cost = co ,coef0 = (0:10), degree = (1:10))
res.svm = svm(Class~Dim.1+Dim.2, data=data_train , kernel ='polynomial',scale = FALSE,cost= 1,coef0=0, degree=1)
summary(res.svm)
plot(res.svm, data_train,Dim.2~Dim.1 )

table(pred=predict(res.svm),vrai=data_train$Class)

# vrai
# pred      Besni Kecimen
# Besni     250      38
# Kecimen    43     269


y_gridL = predict(res.svm, newdata = data_test)

table(pred = y_gridL,vrai = data_test$Class)
