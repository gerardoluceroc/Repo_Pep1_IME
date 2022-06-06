#Ejercicio Practico 10
#Grupo 3

#Integrantes: 
#- Gerardo Lucero
#- Luis Gonzalez
#- Cristobal Arias



#Se importan los paquetes a utilizar
library(dplyr)
library(magrittr)
library(ggpubr)
library(ggplot2)



#Se selecciona la carpeta de trabajo 
setwd("C://Users//gerar//Desktop//Inferencia y Modelos Estadisticos//Ejercicio 10")


#Se obtienen los datos 
datos <- read.csv2("EP10 Datos.csv")

################################# PREGUNTA 1 ###########################################
########################################################################################


#¿Existe diferencia en la puntuación obtenida por los envases diseñados por LaKajita según las evaluaciones
#realizadas por niños y jóvenes?


#Viendo el enunciado, todo parece indicar que para responder a esta pregunta
#es pertinente utilizar una prueba t de student para muestras independientes

#Para esto, primero que todo se revisarán los datos de LaKajita y las evaluaciones hechas solos por jovenes y niños

#se obtiene una tabla con los datos de la empresa LaKajita
datosLK <- datos %>% filter(Diseno == "LaKajita")

#Se filtran los datos de LaKajita con las evalauciones hechas solo por jovenes y niños
datosLK <- datosLK %>% filter(Edad == "Joven" | Edad == "Nino")


#Se obtiene una tabla de frecuencias con las 7 categorias de calificaciones disponibles
freqLK <- as.data.frame(table(datosLK[["Puntaje"]]))

#se cambia el nombre de las columnas del data frame obtenido con las frecuencias
colnames(freqLK) <- c("Puntaje Asignado", "Frecuencia")



#Se realiza el gráfico de barras con las frecuencias
graficoBarras <- ggbarplot(freqLK, x = "Puntaje Asignado", y = "Frecuencia",
                                     label = TRUE, lab.pos = "out", lab.col = "black",
                                     fill = "Puntaje Asignado", palette = "jco",
                                     title = "Puntaje Dado a LaKajita",
                                     subtitle = "")
#Se muestra el grafico de barras
print(graficoBarras)

#Viendo el grafico, se puede ver que la muestra obtenida se distribuye aproximadamente normal

#Tambien, viendo el enunciado del problema, se puede ver que las observaciones son independientes
#ya que se encuestan a diferentes personas al azar, por lo que esta condición también se cumple.

#Ahora,si bien se cumplen algunas condiciones para utiizar la prueba t de student, viendo
#las características del problema, se ve que no es posible utilizar esta prueba, 
#ya que el puntaje asignado a los envases siguen una escala Likert, y no todas las escalas Likert
#pueden asegurar que son de igual intervalo, por ejemplo 
#si dos participantes califican un envase con puntaje 3 y 5, mientras que dos
#participantes califican el mismo envase con notas 4 y 6, no se puede asegurar que en ambos hay la misma diferencia de intervalos.

#Con esto cuenta, se concluye que no se puede utilizar la prueba t de student para resolver el problema.

#Como alternativa entonces, se utiliza la prueba no parametrica de Wilcoxon-Manmn-Whitney

#Para esto se deben cumplir 2 condiciones:

#1. Como ya se dijo, las observaciones son independientes ya que son respuestas dadas por diferentes personas elegidas al azar
#2. La escala es almenos ordinal, ya que hay valores mas grandes que otros, por lo que se puede hablar de "mayor que", "menor que" o "igual que".


#Ahora se proceden a realizar los respectivos dataframe para realizar la prueba

#Se obtiene el data frame con los datos de LaKajita y las evaluciones hechas por jovenes
datosLKJovenes <- datosLK %>% filter(Edad == "Joven")

#Se obtiene el data frame con los datos de LaKajita y las evaluciones hechas por niños
datosLKNinos <- datosLK %>% filter(Edad == "Nino")

#Se guardan los puntajes asignados por jovenes y niños a los envases de LaKajita en su vector respectivo
puntajeJovenes <- unlist(datosLKJovenes["Puntaje"])
puntajeNinos <- unlist(datosLKNinos["Puntaje"])

#Una vez obtenido los datos a utilizar, se procede a realizar la prueba de hipotesis

#H0: No Existe diferencia en la puntuación obtenida por los envases diseñados por LaKajita según las evaluaciones
#    realizadas por niños y jóvenes

#HA:  Existe diferencia en la puntuación obtenida por los envases diseñados por LaKajita según las evaluaciones
#     realizadas por niños y jóvenes

#Se utiliza un nivel de significancia de 0.05
alfa <- 0.05

#Se procede a realizar la prueba de Wilcoxon-Manmn-Whitney
prueba <- wilcox.test(puntajeJovenes, puntajeNinos, alternative = "two.sided", conf.level = 1-alfa)


print(prueba)

#Finalmente se tiene un valor p = 0.5135 > 0.05

#Como el valor p es mayor al nivel de significancia, y al ser una prueba bilateral, se falla al rechazar H0, y se concluye
#con un 95% de confianza que no existe una diferencia significativa en la puntuacion
#obtenida por los envases diseñados por LaKajita según las evaluaciones realizadas por niños y jovenes.

#########################################################################################################################
#########################################################################################################################































################################# PREGUNTA 2 ############################################################################
#########################################################################################################################

#¿Existen diferencias entre las puntuaciones obtenidas para los diferentes envases de chocolate? De ser así,
#¿cuál(es) envase(s) se diferencia(n) de los demás?

#se obtiene una tabla con los datos del producto chocolate
datosChocolate <- datos %>% filter(Producto == "Chocolate")

#Se ordenan los datosde acuerdo a la empresa que realizo el diseño
datosChocolate <- datosChocolate %>% arrange(Diseno)

#Se obtiene una tabla de frecuencias con las 7 categorias de calificaciones disponibles
freqChocolate <- as.data.frame(table(datosChocolate[["Puntaje"]]))

#se cambia el nombre de las columnas del data frame obtenido con las frecuencias
colnames(freqChocolate) <- c("Puntaje Asignado", "Frecuencia")



#Se realiza el gráfico de barras con las frecuencias
graficoBarrasChocolate <- ggbarplot(freqChocolate, x = "Puntaje Asignado", y = "Frecuencia",
                           label = TRUE, lab.pos = "out", lab.col = "black",
                           fill = "Puntaje Asignado", palette = "jco",
                           title = "Puntaje Dado al producto Chocolate",
                           subtitle = "")
#Se muestra el grafico de barras
print(graficoBarrasChocolate)

#Como se puede ver en el grafico, la distribucion de los puntajes al producto Chocolate
#No se distribuye completamente normal

#Ahora, para resolver la pregunta 2, se puede ver que no se puede realizar la prueba t de student ya que existen mas de 2 variables.
#Otra razón para no utilizar la prueba t de student es que, al igual que la pregunta 1,
#En el enunciado general del problema el puntaje asignado a los envases siguen una escala Likert, y no todas las escalas Likert
#pueden asegurar que son de igual intervalo, por ejemplo 
#si dos participantes califican un envase con puntaje 3 y 5, mientras que dos
#participantes califican el mismo envase con notas 4 y 6, no se puede asegurar que en ambos hay la misma diferencia de intervalos.

#Por lo que resulta conveniente utilizar la prueba no paramétrica de Kruskal-Wallis

#Para usar esta prueba, se deben cumplir las siguientes condiciones:
# 1. Las observaciones son independientes entre sí.
# 2. La variable independiente debe tener a lo menos dos niveles.
# 3. La escala de la variable dependiente debe ser, a lo menos, ordinal.

#En cuanto a la primera condicion, se puede ver que las observaciones son independientes ya que son producto de seleccionar diferentes personas al azar.

#En cuanto a la segunda condición, esta también se cumple ya que la variable independiente tiene 4 niveles pertenecientes a los nombres de las empresas que hicieron el diseño.

#Finalmente, la tercera condición tambien se cumple ya que los valores que maneja la variable dependiente
#tiene valores mas grandes que otros, por lo que se puede hablar de "mayor que", "menor que" o "igual que".

#Con estas tres condiciones cumpliendose, se puede realizar la prueba de Kruskal-Wallis

#Pero antes, se realizan las manipulaciones correspondientes a los datos de modo de poder realizar la prueba

#Se obtiene el data frame con los datos del producto chocolate diseñado por la empresa LaKajita
datosChocolateLK <- datosChocolate %>% filter(Diseno == "LaKajita")

#Se obtiene el data frame con los datos del producto chocolate diseñado por la empresa PackPro
datosChocolatePP <- datosChocolate %>% filter(Diseno == "PackPro")

#Se obtiene el data frame con los datos del producto chocolate diseñado por la empresa KoolDesign
datosChocolateKD <- datosChocolate %>% filter(Diseno == "KoolDesign")

#Se obtiene el data frame con los datos del producto chocolate diseñado por la empresa DisenoColor
datosChocolateDC <- datosChocolate %>% filter(Diseno == "DisenoColor")

#Se guardan los puntajes asignados al producto chocolate para las distintas empresas que realizaron un diseño
puntajeLK <- unlist(datosChocolateLK["Puntaje"])
puntajePP <- unlist(datosChocolatePP["Puntaje"])
puntajeKD <- unlist(datosChocolateKD["Puntaje"])
puntajeDC <- unlist(datosChocolateDC["Puntaje"])


puntajes <- c(puntajeLK,puntajePP,puntajeKD,puntajeDC)

tablaEmpresas <- c(rep("puntajeLK", length(puntajeLK)),
           rep("puntajePP", length(puntajePP)),
           rep("puntajeKD", length(puntajeKD)),
           rep("puntajeDC", length(puntajeDC)))

tablaEmpresas <- factor(tablaEmpresas)

datosChocolatePuntajes <- data.frame(puntajes,tablaEmpresas)

#Finalmente, se realiza la prueba de hipotesis de Kruskal-Wallis

#H0: No existen diferencias entre las puntuaciones obtenidas para los diferentes envases de chocolate

#HA:  existen diferencias entre las puntuaciones obtenidas para los diferentes envases de chocolate


#Se establece un nivel de significancia de 0.05
alfaP2 <- 0.05

#Se realiza la prueba de Kruskal-Wallis
pruebaP2 <- kruskal.test(puntajes ~ tablaEmpresas, data = datosChocolatePuntajes)

print(pruebaP2)

#Finalmente se tiene un valor p = 0.6735 > 0.05

#Como el valor p es mayor al nivel de significancia, y al ser una prueba bilateral, se falla al rechazar H0, y se concluye
#con un 95% de confianza que no existe una diferencia significativa en la puntuacion
#obtenida para los diferentes envases de chocolate


#########################################################################################################################
#########################################################################################################################




