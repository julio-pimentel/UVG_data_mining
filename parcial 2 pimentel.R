library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)

#cargar datos
setwd("C:/Users/Juliopimentel/Dropbox/UVG/2014/8vo semestre/Mineria de Datos/DataM")
credito = read.csv("credito.csv")
str(credito)
#cambiar factores
credito2=credito
credito2$A1 <- as.factor(credito$A1)
credito2$A8 <- as.factor(credito$A8)
credito2$A9 <- as.factor(credito$A9)
credito2$A11 <- as.factor(credito$A11)

str(credito2)
#particion de datos
set.seed(123)
split = sample.split(credito2$Class, SplitRatio = 0.70)
credito2Train = subset(credito2, split == TRUE)
credito2Test = subset(credito2, split == FALSE)

prom1=mean(credito2Train$A2)
prom1

prom2=mean(credito2Test$A2)
prom2
#crear arbol
arbol <- rpart(Class ~ .,data=credito2Train,method="class",control=rpart.control(minbucket=5))
prp(arbol)
#crear matriz de confusion
predictClass <- predict(arbol, newdata=credito2Test,type="class")
confumat <- table(credito2Test$Class,predictClass)
confumat
sum(diag(confumat))/sum(confumat)
#baseline
tab <- table(credito$Class)
max(tab)/sum(tab)
#random forest
str(credito2)

set.seed(123456789)
credito2RF <- randomForest(Class ~ .,data=credito2Train)
credito2Pred2 <- predict(credito2RF, newdata=credito2Test)
t3 <- table(credito2Test$Class, credito2Pred2)
sum(diag(t3))/sum(t3)

#cluster jerarjico
credito3=credito[,-15]
credito3 <- scale(credito3,center=FALSE)
d <- dist(credito3,method="euclidean")
h1 <- hclust(d,method="ward")
plot(h1,labels=FALSE)
rect.hclust(h1,2,border="red")
#clasificacion cluster
grupos <- cutree(h1,2)
t3=table(credito$Class,grupos)
sum(diag(t3))/sum(t3)
#cluster k medias
set.seed(123)
cluserk <- kmeans(credito3, 2)
t4=table(cluserk$cluster,credito$Class)
t4
t5=table(cluserk$cluster,grupos)
t5
