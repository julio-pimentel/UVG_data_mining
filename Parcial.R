#Julio Pimentel

setwd("C:/Users/Juliopimentel/Dropbox/UVG/2014/8vo semestre/Mineria de Datos/DataM")
boston = read.csv("boston.csv")
str(boston)
summary(boston)
boston
a=tapply(boston$MEDV, boston$TOWN, mean)

b=sort(a)
b

boxplot(boston$MEDV ~ boston$CHAS)


library(caTools)

set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.75)

bostonTrain = subset(boston, split == TRUE)
bostonTest = subset(boston, split == FALSE)

modelo1 = lm(MEDV ~ CRIM+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+PTRATIO , data=bostonTrain)
summary(modelo1)


modelo2 = lm(MEDV ~ CRIM+INDUS+CHAS+NOX+RM+AGE+DIS+PTRATIO , data=bostonTrain)
summary(modelo2)

modelo2T = lm(MEDV ~ CRIM+INDUS+CHAS+NOX+RM+AGE+DIS+PTRATIO , data=bostonTest)
SSE = sum(modelo2T$residuals^2)
SSE

set.seed(123)
split = sample.split(boston$CHAS, SplitRatio = 0.75)

bostonTrain2 = subset(boston, split == TRUE)
bostonTest2 = subset(boston, split == FALSE)

modglm1 = glm(CHAS ~ LAT+LON+DIS+RAD, data=bostonTrain2, family=binomial)
summary(modglm1)

modglm1T = glm(CHAS ~ LAT+LON+DIS+RAD, data=bostonTest2, family=binomial)
predictTest = predict(modglm1T, type="response")
predictTest
summary(predictTest)
tapply(predictTest, bostonTest2$CHAS, mean)
confumat <- table(bostonTest2$CHAS, predictTest > 0.14)
confumat
preci <- sum(diag(confumat))/sum(confumat)
preci