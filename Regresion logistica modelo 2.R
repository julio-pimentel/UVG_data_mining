# Modelo de Regresion Logistica
setwd("C:/Users/Juliopimentel/Dropbox/UVG/2014/8vo semestre/Mineria de Datos/DataM")
# Leer los datos
magazine = read.csv("magazine.csv")

# Examinar la estructura
str(magazine)

# Tabular la variable respuesta
table(magazine$Buy)


# Instalar paquete para hacer el split de train/test
library(caTools)

# Hacer un split aleatorio de los datos, garantizando reproducibilidad
set.seed(88)
split = sample.split(magazine$Buy, SplitRatio = 0.75)
split

# Crear sets de datos para entrenamiento y validacion
magTrain = subset(magazine, split == TRUE)
magTest = subset(magazine, split == FALSE)

#otra forma de hacerlo
#magTrain2 = magazine[split==TRUE,]

# Modelo de regresion logistica
magmod1 = glm(Buy ~ Professional + College + DualIncome + Income + English, data=magTrain, family=binomial)
summary(magmod1)

magmod = glm(Buy ~ DualIncome + Income + English, data=magTrain, family=binomial)
summary(magmod)


#k=b0+b1*x1+b2*x2..
#P(=1)= 1/(e+exp(k))

#AIC = entre mas pequeño es mejor el modelo

# Hacer predicciones con el modelo utilizando los datos de entrenamiento
#se utiliza "response" para utilizar la funcion logistica

predictTrain = predict(magmod, type="response")
predictTrain
# Analizar los resultados de las predicciones
summary(predictTrain)
#utilice preditrain para agrumar con magTrain$buy y apliquele la media
tapply(predictTrain, magTrain$Buy, mean)

#  Sensitividad =  PV/(PV+NF)
#  Especificidad =  NV/(NV+PF)

# Matriz de Confusion para un umbral de 0.5
confumat <- table(magTrain$Buy, predictTrain > 0.55)

confumat

#sirve para dejar un intervalo de "0" o "1" donde sea aceptable
#por ejemplo si es 0.6 - > es 1
#si predice que es 0, entonces la cantidad de "False" son los que le pego el modelo
#los "True" son los que no le pego

#cuando predice que es 1, y es "True" significa que le pego al modelo


#crea un vector de True or False, depende si cumple la condicion
a=predictTrain >0.5
a



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
confumat2 <- table(magTrain$Buy, predictTrain > 0.7)
confumat2

# Sensitividad, especificidad y precison del modelo
sensi2 <- confumat2[2,2]/sum(confumat2[2,])
especi2 <- confumat2[1,1]/sum(confumat2[1,])

preci2 <- sum(diag(confumat2))/sum(confumat2)
sensi2
especi2
preci2

# Matriz de Confusion para un umbral de 0.4
confumat3 <- table(magTrain$Buy, predictTrain > 0.4)
confumat3

# Sensitividad, especificidad y precison del modelo
sensi3 <- confumat3[2,2]/sum(confumat3[2,])
especi3 <- confumat3[1,1]/sum(confumat3[1,])

preci3 <- sum(diag(confumat3))/sum(confumat3)
sensi3
especi3
preci3

# Hacer Predicciones fuera de muestra

predictTest <- predict(magmod, type="response", newdata=magTest)

summary(predictTest)
tapply(predictTest, magTest$Buy, mean)

# Matriz de Confusion para un umbral de 0.5
confumat <- table(magTest$Buy, predictTest > 0.4)
confumat
# Sensitividad, especificidad y precison del modelo
sensi <- confumat[2,2]/sum(confumat[2,])
especi <- confumat[1,1]/sum(confumat[1,])

preci <- sum(diag(confumat))/sum(confumat)
sensi
especi
preci