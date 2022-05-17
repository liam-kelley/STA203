#########Projet final de STAT203#############################
#setwd("~/ENSTA 2A/STA203")
setwd("~/Documents/STA203/STA203")

library("openxlsx")
rm(list=objects()) ; graphics.off()
data = read.xlsx("Raisin.xlsx",colNames=TRUE)

#################################################################### PARTIE 1 

#-------------------------------------------question 1

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


#---------------------------------question 2 : ACP

library("FactoMineR")

data$Class = as.factor(data$Class)
res.PCA=PCA(data,quali.sup = p, graph = TRUE)
plt1=plot(res.PCA,axes=c(1,2),choix="ind",label="quali", habillage=p) +
  theme(legend.position = "none") 
plt2=plot(res.PCA,axes=c(2,3),choix="ind",label="quali", habillage=p) 
plt4=plot(res.PCA,axes=c(6,5),choix="ind",label="quali", habillage=p) 
plt4
plt3 = plot(res.PCA,choix="var")
cowplot::plot_grid(plt1, plt3, nrow=1, ncol=2)

barplot(res.PCA$eig[,1])

res.PCA$var$cos2[,1:2]
# > res$var$cos2[,1:2]
#                  Dim.1       Dim.2
# Area            0.97109063 0.019587803
# MajorAxisLength 0.94935877 0.027110793
# MinorAxisLength 0.73266049 0.204270120
# Eccentricity    0.19907684 0.542191863
# ConvexArea      0.98262357 0.011155576
# Extent          0.01535408 0.647175333
# Perimeter       0.98212422 0.001696952


#----------------------------------Question 3.1 - k-means

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

#----------------------------------question3.2- Classification hierarchique

d.data = dist(data[,-p])
cah.ward = hclust(d.data, method="ward.D2")
cah.ward
#Cluster method   : ward.D2
#Distance         : euclidean 
#Number of objects: 900 
plot(cah.ward, cex=0.5)

#avec variables normalisées
d.data2 = dist(data.scale)
cah.ward2 = hclust(d.data2, method="ward.D2")
cah.ward2
plot(cah.ward2, cex=0.5)

# dendrogram with highlighting of the groups
rect.hclust(cah.ward2,k=2)
# partition in 4 groups
groupes.cah <- cutree(cah.ward2,k=2)
# assignment of the instances to clusters
#print(sort(groupes.cah))
plot(as.factor(groupes.cah))

#erreur de classification
table(pred=as.factor(groupes.cah),vrai=data$Class)


#-------------------------------------------------Question 4

#for (i in 1:7) {
#  res.pca2 = PCA(data.scale, ncp = i, graph=FALSE)
#  res.hcpc <- HCPC(res.pca2, graph = FALSE)
#}

res.pca2 = PCA(data.scale, ncp = 1, graph=FALSE)
summary(res.pca2)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc1 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc1,vrai=data$Class) #erreur de classification = 214

res.pca2 = PCA(data.scale, ncp = 2, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc2 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc2,vrai=data$Class) #erreur de classification = 214

table(un=groupes.hcpc1,deux=groupes.hcpc2)

res.pca2 = PCA(data.scale, ncp = 3, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc3 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc3,vrai=data$Class) #erreur de classification = 209

res.pca2 = PCA(data.scale, ncp = 4, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc4 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc4,vrai=data$Class) #erreur de classification = 209

res.pca2 = PCA(data.scale, ncp = 5, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc5 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc5,vrai=data$Class) #erreur de classification = 209

res.pca2 = PCA(data.scale, ncp = 6, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc6 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc6,vrai=data$Class) #erreur de classification = 209

res.pca2 = PCA(data.scale, ncp = 7, graph=FALSE)
res.hcpc = HCPC(res.pca2, nb.clust=2, graph = TRUE)
groupes.hcpc7 = res.hcpc$data.clust$clust
table(pred=groupes.hcpc7,vrai=data$Class) #erreur de classification = 209

#for pretty graphs

#library("factoextra")
#fviz_dend(res.hcpc, 
#          cex = 0.7,                     # Taille du text
#          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
#          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
#          rect_border = "jco",           # Couleur du rectangle
#          labels_track_height = 0.8      # Augment l'espace pour le texte
#)

#fviz_cluster(res.hcpc,
#             repel = TRUE,            # Evite le chevauchement des textes
#             show.clust.cent = TRUE, # Montre le centre des clusters
#             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
#             ggtheme = theme_minimal(),
#             main = "Factor map"
#)

########################################################### PARTIE 2 

#-------------------------------------------------question2

set.seed(1)
n = nrow(data)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))

res=res.PCA

data_train = cbind(Class = data$Class,as.data.frame(res$ind$coord))[train,]
data_test = cbind(Class = data$Class,as.data.frame(res$ind$coord))[!train,]

#modele complet

res.glm.complet = glm(Class~.,
                      family=binomial,data=data_train)
summary(res.glm.complet)

#modele deux composantes principales

comp1 = res$ind$coord[train,1]
comp2 = res$ind$coord[train,2]
new = as.data.frame(cbind(Class=data_train$Class,dim1 = comp1,dim2 = comp2))
res.glm.prin = glm(Class~(Dim.1+Dim.2) ,family=binomial,data=data_train )

summary(res.glm.prin)

library(magrittr)

probabilities = res.glm.prin %>% predict(data_test, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
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


# modele par AIC
glm = glm(Class~.,family=binomial,data=data_train)
selection=MASS::stepAIC(glm,direction="both")
glm.AIC = glm(Class~Dim.1+Dim.2 + Dim.3 + Dim.5,family=binomial,data=data_train)

probabilities = glm.AIC %>% predict(data_test, type = "response")
predicted.classes = ifelse(probabilities > 0.5, "Kecimen", "Besni")
mean(predicted.classes == data_test$Class) #0.862069


# modele obtenu par regression penalisee lasso

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


#-----------------------------------------------------question 3 : SVM

#SVM lineaire

library(e1071)
co= cbind(0.001, 0.01, 0.1, 1, 5, 10, 100)
obj = tune.svm(Class~Dim.1+Dim.2, data=data_train , kernel ='linear', scale =FALSE,tunecontrol=tune.control(cross=2), cost = co) #trouver meilleur cout
res.svm = svm(Class~Dim.1+Dim.2, data=data_train , kernel ='linear', scale =FALSE,cost= 1)
summary(res.svm)
plot(res.svm, data_train,Dim.2~Dim.1 )


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


test_pred = predict(res.svm, newdata = data_test)

table(pred = test_pred,vrai = data_test$Class)


#--------------------------------------question 4 : courbe ROC

#courbe roc pour modele complet test et appentissage superpose
library(ROCR)
proba.complet.test = res.glm.complet %>% predict(data_test, type = "response")
pred = prediction(proba.complet.test,data_test$Class)
ROC = performance(pred,"sens","fpr")
plot(ROC, xlab="1-specif", main="courbes ROC")               
lines(c(0,1),c(0,1),lty=2) 

proba.complet.train = res.glm.complet %>% predict(data_train, type = "response")
pred.train = prediction(proba.complet.train,data_train$Class)
ROC_train = performance(pred.train,"sens","fpr")
plot(ROC_train, xlab="1-specif", main="courbes ROC",col =2,add=TRUE)


legend("bottomright",legend= c(paste("echantillon d'apprentissage"),
                               paste("echantillon de test")
), col=1:2, lty=1)


#Courbe des 4 modele de la question 2 sur echantillon test

proba.complet.test = res.glm.complet %>% predict(data_test, type = "response")
pred_complet = prediction(proba.complet.test,data_test$Class)
class(pred_complet) 
mode(pred_complet) 
ROC_complet = performance(pred,"sens","fpr")
plot(ROC_complet, xlab="1-specif", main="courbes ROC",legend = "complet")

proba.princ.test = res.glm.prin %>% predict(data_test, type = "response")
pred_princ = prediction(proba.princ.test,data_test$Class)
class(pred_princ) 
mode(pred_princ) 
ROC_princ = performance(pred,"sens","fpr")
plot(ROC_princ, xlab="1-specif", main="courbes ROC",col=2,add=TRUE,legend="2 CP")

x.test <- model.matrix(Class ~., data_test)[,-1] #on retire l'intercept!
proba.lasso.test = lasso.fit %>% predict(newx=x.test)
pred_lasso = prediction(proba.lasso.test,data_test$Class)
class(pred_lasso) 
mode(pred_lasso) 
ROC_lasso = performance(pred_lasso,"sens","fpr")
plot(ROC_lasso, xlab="1-specif", main="courbes ROC",col=3,add=TRUE,legend="Lasso")

proba.aic.test = glm.AIC %>% predict(data_test, type = "response")
pred_AIC = prediction(proba.aic.test,data_test$Class)
class(pred_AIC) 
mode(pred_AIC) 
ROC_AIC = performance(pred_AIC,"sens","fpr")
plot(ROC_AIC, xlab="1-specif", main="courbes ROC",col=4,add=TRUE,legend="AIC")



perf1= performance(pred_complet, "auc")
(AUC1=round(unlist(perf1@y.values),4) ) 
perf2 = performance(pred_princ, "auc")
(AUC2=round(unlist(perf2@y.values),4) )
perf3 = performance(pred_lasso, "auc")
(AUC3=round(unlist(perf3@y.values),4) )
perf4 = performance(pred_AIC, "auc")
(AUC4=round(unlist(perf4@y.values),4) )

legend("bottomright",legend= c(paste("AUC complet: ",AUC1),
                               paste("AUC 2 CP: ",AUC2),
                               paste("AUC lasso: ",AUC3),
                               paste("AUC AIC: ",AUC4)
), col=1:4, lty=1)

#--------------------------------------------------QUESTION 5

#on cherche le meilleur seuil

pred_AIC %>%
  performance(measure = "tpr", x.measure = "fpr") -> result

plotdata <- data.frame(x = result@x.values[[1]],
                       y = result@y.values[[1]], 
                       p = result@alpha.values[[1]])


dist_vec <- plotdata$x^2 + (1 - plotdata$y)^2
opt_pos <- which.min(dist_vec)

seuil = plotdata[opt_pos, ]$p  #0.0505

###############en apprentissage##########################
#modele complet 
predicted.classes.complet.app = ifelse(proba.complet.train >seuil, "Kecimen", "Besni")
acc = mean(predicted.classes.complet.app == data_train$Class) #0.8616667
1- acc #0.1383333


#modele 2 premiere composantes principales 
proba.princ.train = res.glm.prin %>% predict(data_train, type = "response")
predicted.classes.princ.train = ifelse(proba.princ.train >seuil, "Kecimen", "Besni")
acc = mean(predicted.classes.princ.train == data_train$Class) #0.8616667
1- acc #0.1333333

#modele AIC
proba.aic.train = glm.AIC %>% predict(data_train, type = "response")
predicted.classes.aic.app = ifelse(proba.aic.train >seuil, "Kecimen", "Besni")
1-mean(predicted.classes.aic.app == data_train$Class) #0.8583333 0.1416667

#modele lasso
x.train <- model.matrix(Class ~., data_train)[,-1] #on retire l'intercept!
proba.lasso.train = lasso.fit %>% predict(newx=x.train)
predicted.classes.lasso.train = ifelse(proba.lasso.train > seuil, "Kecimen", "Besni")
1-mean(predicted.classes.lasso.train == data_train$Class) #0.155

#modele SVM
pred_train = predict(res.svm,data_train)
1-mean(pred_train==data_train$Class) #0.135


######en test###############################

#modele complet 
predicted.classes.complet.test = ifelse(proba.complet.test >seuil, "Kecimen", "Besni")
acc = mean(predicted.classes.complet.test == data_test$Class) #0.8616667
1- acc #0.134594


#modele 2 premiere composantes principales 
proba.princ.test = res.glm.prin %>% predict(data_test, type = "response")
predicted.classes.princ.test = ifelse(proba.princ.test >seuil, "Kecimen", "Besni")
acc = mean(predicted.classes.princ.test == data_test$Class) #0.8616667
1- acc #0.1290323

#modele AIC
proba.aic.test = glm.AIC %>% predict(data_test, type = "response")
predicted.classes.aic.test = ifelse(proba.aic.test >seuil, "Kecimen", "Besni")
1-mean(predicted.classes.aic.test == data_test$Class) #0.137931

#modele lasso
x.test <- model.matrix(Class ~., data_test)[,-1] #on retire l'intercept!
proba.lasso.test = lasso.fit %>% predict(newx=x.test)
predicted.classes.lasso.test = ifelse(proba.lasso.test > seuil, "Kecimen", "Besni")
1-mean(predicted.classes.lasso.test == data_test$Class) #0.1535039

#modele SVM
pred_test = predict(res.svm,data_test)
1-mean(pred_test==data_test$Class) #0.1290323

#################################################################### PARTIE III

#-----------------------------------------------------------question 1

library(ade4)
data_train2 = as.data.frame(data.scale)[train,] # need own data.train to apply pca to
data_test2 = as.data.frame(data.scale)[!train,]
rm(res.pca3)
res.pca3=dudi.pca(data_train2, scannf = FALSE, nf = 2)
summary(res.pca3)
res.pca3plusproj=suprow(res.pca3, data_test2)
full=rbind(res.pca3$li,res.pca3plusproj$lisup)
#plot(res.pca3$li)
#plot(res.pca3plusproj$lisup)
plot(full,col=as.factor(cbind(rep("train", 600),rep("projected tests",300))))

#En utilisant que PCA
res.pca4=PCA(as.data.frame(rbind(data_train2,data_test2)), ncp = 2, ind.sup = seq(601, 900, 1), graph = TRUE)

#---------------- intro au reste

# on ne travaille que sur les 2 premieres composantes principales 

comp1 = res$ind$coord[train,1]
comp2 = res$ind$coord[train,2]
data3 = as.data.frame(cbind(Class=data_train$Class,dim1 = comp1,dim2 = comp2))

data3_B = data3[data3$Class=="1",]
data3_K = data3[data3$Class=="2",]

comp1_test = res$ind$coord[!train,1]
comp2_test = res$ind$coord[!train,2]
data3_test = as.data.frame(cbind(Class=data_test$Class,dim1 = comp1_test,dim2 = comp2_test))

#-----------------------------------------------------------question 2
##a)

# Analyse discriminante lin?aire (LDA) : Utilise des combinaisons lin?aires
# de pr?dicteurs pour pr?dire la classe d'une observation 
# donn?e. Elle suppose que les variables pr?dicteurs (p) sont
# normalement distribu?es et que les classes ont des variances
# identiques (pour une analyse univari?e, p = 1) ou des matrices
# de covariance identiques (pour une analyse multivari?e, p > 1).


#b) 

# calcul de mu_kecimen et mu_besni

muk = colMeans(data3_K[,-1])
mub = colMeans(data3_B[,-1])

#calcul de la matrice de cov

sigma_K = t(as.matrix(data3_K[,-1] - muk))%*%as.matrix(data3_K[,-1]-muk)
sigma_B = t(as.matrix(data3_B[,-1]-mub))%*%as.matrix(data3_B[,-1]-mub)

sigma = cov(as.matrix(data3_K[,-1]))+ cov(as.matrix(data3_B[,-1]))
invSigma = solve(sigma)
coef  = invSigma%*%(muk-mub)

#              [,1]
# dim1 -0.0012498297
# dim2 -0.0002739991

#calcul de l'intercept 

inter = log(nrow(data3_K)/nrow(data3_B))-0.5*(-t(mub) %*% invSigma %*% mub+t(muk) %*% invSigma %*% muk)
#0.04579246

##c) V?rification de la formule th?orique

eig = eigen(sigma)
lambda = diag(eig$values)
U = -eig$vectors
new_coef = U%*%solve(lambda)%*%t(U)%*%(muk-mub)
coef-new_coef
# [,1]
# dim1 -2.220446e-16
# dim2  2.775558e-17

new_intercept = log(nrow(data3_K)/nrow(data3_B))-0.5*(-t(mub) %*% U%*%solve(lambda)%*%t(U) %*% mub+t(muk) %*% U%*%solve(lambda)%*%t(U) %*% muk)
new_intercept - inter # 0


####d) plot dans le premier plan principal avec droite d?cision


plt1 + geom_abline(intercept = -inter/coef[2], slope = -coef[1]/coef[2])



####question 3

#prediction avec question pr?c?dente sur ?chantillon test

predict = ifelse(as.matrix(data3_test[,-1])%*%coef > -0.05820362, "2", "1") #2 : kecimen 1:Besni
mean(as.factor(predict)==data3_test$Class) #0.85



#LDA avec fonction
library(MASS)

model <- lda(Class~., data = data3)
predictions <- model %>% predict(data3_test[,-1])
mean(predictions$class!=data3_test$Class)
#0.1433333

#courbe ROC associ? ? LDA

library(ROCR)

pred <- prediction(predictions$posterior[,2], data3_test$Class) 
perf <- performance(pred,"tpr","fpr")
AUC_LDA = performance(pred,"auc")
(AUC5 = round(unlist(AUC_LDA@y.values),4) )
plot(perf, xlab="1-specif", main="courbes ROC",col=5,add=TRUE,legend="AIC")
legend("bottomright",legend= c(paste("AUC complet: ",AUC1),
                               paste("AUC 2 CP: ",AUC2),
                               paste("AUC lasso: ",AUC3),
                               paste("AUC AIC: ",AUC4),
                               paste("AUC LDA :", AUC5)
), col=1:5, lty=1)

