# Modelo de Regresion Logistica
setwd("C:/Users/Juliopimentel/Dropbox/UVG/2014/8vo semestre/Mineria de Datos/DataM")
carros = read.csv("cars1.csv")

# Examinar la estructura
str(carros)

carros$US = as.numeric(carros$brand == "US.")
table(carros$US)


# Instalar paquete para hacer el split de train/test
library(caTools)

# Hacer un split aleatorio de los datos, garantizando reproducibilidad
set.seed(123)
split = sample.split(carros$US, SplitRatio = 0.75)
split

# Crear sets de datos para entrenamiento y validacion
carrosTrain = subset(carros, split == TRUE)
carrosTest = subset(carros, split == FALSE)

#otra forma de hacerlo
#magTrain2 = magazine[split==TRUE,]

# Modelo de regresion logistica

#FOrmula logistica:
#1/(1+exp(-(b0+b1*x1+b2*x2...)))

modglm1 = glm(US ~ mpg + cylinders + cubicinches + hp + weightlbs + time.to.60, data=carrosTrain, family=binomial)
summary(modglm1)


modglm2 = glm(US ~ mpg + cylinders + cubicinches + hp + weightlbs , data=carrosTrain, family=binomial)
summary(modglm2)

modglm3 = glm(US ~ mpg + cylinders + cubicinches + weightlbs , data=carrosTrain, family=binomial)
summary(modglm3)

modglm4 = glm(US ~  cylinders + cubicinches + weightlbs , data=carrosTrain, family=binomial)
summary(modglm4)


#k=b0+b1*x1+b2*x2..
#P(=1)= 1/(e+exp(k))

#AIC = entre mas pequeño es mejor el modelo

# Hacer predicciones con el modelo utilizando los datos de entrenamiento
#se utiliza "response" para utilizar la funcion logistica

predictTrain = predict(modglm4, type="response")
predictTrain
# Analizar los resultados de las predicciones
summary(predictTrain)
#utilice preditrain para agrumar con magTrain$buy y apliquele la media
tapply(predictTrain, carrosTrain$US, mean)

#  Sensitividad =  PV/(PV+NF)
#  Especificidad =  NV/(NV+PF)

# Matriz de Confusion para un umbral de 0.5
confumat <- table(carrosTrain$US, predictTrain > 0.50)

confumat

#sirve para dejar un intervalo de "0" o "1" donde sea aceptable
#por ejemplo si es 0.6 - > es 1
#si predice que es 0, entonces la cantidad de "False" son los que le pego el modelo
#los "True" son los que no le pego

#cuando predice que es 1, y es "True" significa que le pego al modelo


# Sensitividad, especificidad y precison del modelo

#En este caso NF = 18
#Capacidad de predecir correctamente los positivos
sensi <- confumat[2,2]/sum(confumat[2,])
#Capacidad de predecir correctamente los negativos
especi <- confumat[1,1]/sum(confumat[1,])

preci <- sum(diag(confumat))/sum(confumat)
sensi
especi
preci
  
# Matriz de Confusion para un umbral de 0.7
confumat2 <- table(carrosTrain$US, predictTrain > 0.60)
confumat2

sensi2 <- confumat2[2,2]/sum(confumat2[2,])
especi2 <- confumat2[1,1]/sum(confumat2[1,])

preci2 <- sum(diag(confumat2))/sum(confumat2)
sensi2
especi2
preci2

confumat3 <- table(carrosTrain$US, predictTrain > 0.5)
confumat3

sensi3 <- confumat2[2,2]/sum(confumat2[2,])
especi3 <- confumat2[1,1]/sum(confumat2[1,])

preci3 <- sum(diag(confumat2))/sum(confumat2)
sensi3
especi3
preci3


predictTest <- predict(modglm4, type="response", newdata=carrosTest)

summary(predictTest)
tapply(predictTest, carrosTest$US, mean)

# Matriz de Confusion para un umbral de 0.5
confumatT <- table(carrosTest$US, predictTest > 0.5)
confumatT
# Sensitividad, especificidad y precison del modelo
sensiT <- confumatT[2,2]/sum(confumatT[2,])
especiT <- confumatT[1,1]/sum(confumatT[1,])

preciT <- sum(diag(confumatT))/sum(confumatT)
sensiT
especiT
preciT
preci3