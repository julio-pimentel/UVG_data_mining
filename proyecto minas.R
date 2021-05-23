#Julio Pimentel
#Carlos Garcia
#Maria Andre Sampuel
###Base de datos de "Division of Mineral Resources Mined Lands" en Nueva York
###link: http://www.dec.ny.gov/lands/5374.html#Mine_Status_Codes
###link de descarga: ftp://ftp.dec.state.ny.us/dmn/zip/mlDOS.zip




#Se establece el lugar del directorio
setwd("C:/Users/Juliopimentel/Dropbox/UVG/2014/8vo semestre/Mineria de Datos/Proyecto 2")
#se lee la base de datos
proyecto = read.csv("gismines.csv")
#se revisa las propiedades de las variables
str(proyecto)

#se crea una lista de las variables que hay que convertir a numericas
lista=c(28,29)

#se recorre esa lista para cambiar la variable a numerica
for (i in lista) 
{
  proyecto[,i] <- as.numeric(proyecto[,i])
}

proyecto[,22]=as.factor(proyecto[,22])

#se crea una lista de las posibles variables que sean de utilidad para predecir
#se depuran informacion como: ID, direccion, nombres ...
lista=c(13,17:23,28,29,32,34)
proyecto2=proyecto[,lista]
str(proyecto2)

#Status: es el estatus de la mina -activo -reclamado -expirado -pendiente -pendiente -transferencia -no permitida -codigo temporal por mina
#AcresControlled: Total de acres controlado por la mina
#AcresLifeOfMine: acres con vida de la mina
#AcresAffected: acres afectadas
#AcresReclaimed: acres reclamados desde 1975
#AcresBB_range: rango de acres afectadas completas
#commodity: tipo de mineral que esta siendo extraido
#Latitude: latitud en grados decimales NAD83 Datum
#Longitude: longitud en grados decimales NAD83 Datum
#Undgnd: si la mina es subterranea
#MapFlag: C  Consolidated Materials Surface Mine -CR  Reclaimed Consolidated Materials Surface Mine -UC  Unconsolidated Materials Surface Mine -UCR  Reclaimed Unconsolidated Materials Surface Mine - UNG  Underground Mine -UNGR  Reclaimed Underground Mine





#se cargan las librerias necesarias para hacer un arbol
library(caTools)
library(rpart)
library(rpart.plot)

#se hace una particion de los datos
set.seed(123)
split = sample.split(proyecto2$Undgnd, SplitRatio=0.7)
proyectoTrain <- proyecto2[split==TRUE,]
proyectoTest <- proyecto2[split==FALSE,]

#se crear el primer arbol, intentando de predecir que cuando una mina es subterranea
arbol1 <- rpart(Undgnd ~ .,data=proyectoTrain,method="class",control=rpart.control(minbucket=2))
prp(arbol1)
#se puede observar que solo depende del codigo GIS en el mapa que representa el tipo de la mina

#se crea un modelo que prediga con los valores del arbol si la mina es subtearranea
pred = predict(arbol1,newdata=proyectoTest,type="class")
# calculamos la nueva matriz de confusion y la precision del nuevo modelo
confumat <- table(proyectoTest$Undgnd, pred)
confumat
sum(diag(confumat))/sum(confumat)
#se tiene una precision del 100%
#esto es debido a que en mapflag tiene entre su clasificaciones las minas subterraneas









#se crea el segundo arbol, intentando de predecir el estatuts de la mina
arbol2 <- rpart(Status ~ .,data=proyectoTrain,method="class",control=rpart.control(minbucket=30))
prp(arbol2)

#se crea un modelo que prediga con los valores del arbol que tipo de estatus tiene la mina
pred2 = predict(arbol2,newdata=proyectoTest,type="class")
# calculamos la matriz de confusion y la precision del nuevo modelo
confumat2 <- table(proyectoTest$Status, pred2)
sum(diag(confumat2))/sum(confumat2)
#se tiene una precision del 93%
#Se puede concluir que el modelo es preciso para poder predecir el estatus de la mina
#predice si la mina esta: activa, no permitida, reclamada








#se crea el segundo arbol, intentando de predecir el que tipo de mineral estan en las minas
arbol3 <- rpart(commodity ~ .,data=proyectoTrain,method="class",control=rpart.control(minbucket=20))
prp(arbol3)

#se crea un modelo que prediga con los valores del arbol que tipo de mineral se encuentra en la mina
pred3 = predict(arbol3,newdata=proyectoTest,type="class")
# calculamos la matriz de confusion y la precision del nuevo modelo
confumat3 <- table(proyectoTest$commodity, pred3)
sum(diag(confumat3))/sum(confumat3)
#se tiene una precision del 88%
#Se puede concluir que el modelo es preciso para poder predecir el tipo de mineral en la mina
#se probo con diferentes "minbucket" y da el mismo arbol




arbol4 <- rpart(AcresBB_range ~ .,data=proyectoTrain,method="class",control=rpart.control(minbucket=20))
prp(arbol4)

#se crea un modelo que prediga con los valores del arbol que tipo de mineral se encuentra en la mina
pred4 = predict(arbol4,newdata=proyectoTest,type="class")
# calculamos la matriz de confusion y la precision del nuevo modelo
confumat4 <- table(proyectoTest$AcresBB_range, pred4)
sum(diag(confumat4))/sum(confumat4)
#se tiene una precision del 100%
#La clasificaicon 




arbol5 <- rpart(Mapflag ~ .,data=proyectoTrain,method="class",control=rpart.control(minbucket=10))
prp(arbol5)

#se crea un modelo que prediga con los valores del arbol que tipo de mineral se encuentra en la mina
pred5 = predict(arbol5,newdata=proyectoTest,type="class")
# calculamos la matriz de confusion y la precision del nuevo modelo
confumat5 <- table(proyectoTest$Mapflag, pred5)
sum(diag(confumat5))/sum(confumat5)
#se tiene una precision del 99.5%
#existe relacion directa del lugar donde esta la mina con el minerla que se extrae




#CLUSTER JERARJICO



d <- dist(proyecto2,method="euclidean")
h1 <- hclust(d,method="ward")
plot(h1,labels=FALSE)
rect.hclust(h1,3,border="red")
grupos <- cutree(h1,3)

# ahora podemos ver si esta agrupación clasificó correctamente las especies

table(proyecto$Mapflag,grupos)
table(proyecto$Undgnd,grupos)
table(proyecto$Status,grupos)
table(proyecto$commodity,grupos)
