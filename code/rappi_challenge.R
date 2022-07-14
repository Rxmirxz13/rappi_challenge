## ---------------------------
##
## Script name: rappi_challenge
##
## Purpose of script: 
##
## Author: Emiliano Ramírez
##
## Date Created: 12 July 2022
##
## Copyright (c) Emiliano Ramírez, 2022
## Email: emiliano.ramirez.l.14@gmail.com
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

## set working directory for Mac and PC

setwd("D:/Emiliano/Rappi")

## ---------------------------

## load up the packages we will need: 

library(tidyverse)
library(ggplot2)
library(dplyr)
library(readxl)
library(skimr)
library(scales)
library(lubridate)
library(jsonlite)
library(pastecs)
library(corrplot)
library(NbClust)
library(factoextra) 


options(scipen=9)

## ---------------------------
## ---------------------------


df <- read.csv("raw/ds_challenge_data.csv")


#vemos que existe una columna en formato json por lo que la formatearemos

#creamos función para deparse de collumna en formato json
ParseJSONColumn <- function(x)  {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>%
    fromJSON(flatten = T) %>%
    as_tibble()
}

#seleccionamos columna que tiene formato json
dispositivo <- df %>% select(dispositivo)

dispositivo_unj <- dispositivo  %>%
  map_dfc(.f = ParseJSONColumn)


#eliminamos la variable model porque es constante en toda la base
df <- bind_cols(df, dispositivo_unj) %>% select(-c(dispositivo, model)) 

#notemos que tenemos varias variables que deben ser declaradas como categóricas
#para su correcto tratamiento.

#Estas variables son: genero, estableciemiento, ciudad, tipo tarjeta (tipo_tc),
#status_txn, is_prime, fraude, device_Score y os.

#damos un poco de limipieza a algunas variables antes de su procesamiento
df <- df %>% mutate(genero=ifelse(genero=="--", NA, genero),
                    establecimiento=ifelse(establecimiento=="N/A",NA,
                                           ifelse(establecimiento=="",NA,establecimiento)),
                    ciudad=ifelse(ciudad=="", NA, ifelse(ciudad=="N/A",NA,ciudad)),
                    os=ifelse(os==".", NA, os))

# en genero: 1 mujer 0 hombre
# factores: establecimiento, ciudad, 
# 1 tarjeta fisica  0 virtual
df <- df %>% mutate(d_genero=ifelse(genero=='F',1,ifelse(genero=='M',0,NA)),
                    establecimiento=as.factor(establecimiento),
                    ciudad=as.factor(ciudad),
                    d_tarj=ifelse(tipo_tc=="Física", 1, 0),
                    status_txn=as.factor(status_txn),
                    d_is_prime=ifelse(is_prime=="True", 1, 0),
                    d_fraude=ifelse(fraude=="True",1,0),
                    os=as.factor(os),
                    fecha=as.Date(dmy(fecha)))

#desechamos variables que ya no necesitamos 
#df <- df %>% select(-c(genero, tipo_tc, status_txn, is_prime, fraude))

#obtengamos un panorama de la base resultante
df %>% skim()

#obtengamos algunos estadísticos descriptivos de algunas variables 

summary(df$d_genero)
# 44.2 % de la muestra es del sexo femenino y existen 2730 missings

stat.desc(df$linea_tc)
#la mediana es casi igual a la media de la distribución de la var de linea de 
#crédito por lo que es factible suponer que la distribución es simétrica, 
#la proporción de la desviación estandar con respecto a la media es del 35%, por
# lo que las colas no se separan mucho del centro y por ende es una muestra homogenea

stat.desc(df$monto)
#el monto de las transacciones no supera los 1000 pesos. Además, su distribución exhibe 
#un comportamiento uniforma y simétrico ya que la media es casi igual que la mediana y su 
#plot lo muestra 

hist(x=df$monto)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df_mode <- df %>% summarise_each(funs = Mode, ID_USER)
# el usuario con más transacciones es el 1958

df_1958 <- df %>% filter(ID_USER==1958) %>% nrow
#62 transacciones hizo el usuario 1958, en un periodo de un mes.

#suponemos que el sistema operativo "%%" es diferente a Android pero no es web (como ios o harmonyOS)

summary(df$os)
#la fecuencia de los distintos sistemas operativos está balanceada


summary(df$d_tarj)
# 70 por ciento de las compras hechas en esta base son con tarjeta física


summary(df$d_status_txn)
# casi el 70 por ciento de las compras son aceptadas


summary(df$d_is_prime)
#solo el 13% de la muestra tiene suscripción prime


summary(df$d_fraude)
# 3% de la muestra está clasificada como compra fraudulenta

summary(df$d_fraude==1 & df$d_status_txn==1)
#el 2% de las transacciones donde la clasificación era fraudulenta la compra fue aceptada

xtabs(~  genero + fraude, data = df)
#el sexo masculino tiene 16% más observaciones registradas como faudulentas 

xtabs(~  genero + status_txn, data = df)
# el sexo masculino tiene 26% más transacciones aceptadas que el sexo femenino

xtabs(~  fraude + os, data = df)
#al parecer la frecuencia de las transacciones categorizadas como fraudulentas están balanceadas

xtabs(~  is_prime + os, data = df)
#no parece haber mayor preferencia por la suscripción a través de los distinots os

#existe mayor cncentración de algun usuario?
hist(x=df$ID_USER)


#la distribución de la frecuencia de aparición de los usuarios muestra un comportamiento uniforme 

#¿existe alguna concentración de monto de la transacción de algún usuario?
ggplot(df, aes(x=ID_USER, y=monto)) +
  geom_point() +
  theme(legend.position="none")

#ahora creemos matrices de correlación lineal para ver si exsite estructura de dependencia lineal
#entre las variables 

df_correlations <- df %>% select(c(monto, linea_tc, interes_tc, dcto, 
                                   cashback))

matrix_df <- as.matrix(df_correlations)
mat_corr <- cor(matrix_df)
corrplot(mat_corr, method="color")

#observando la matriz de correlaciones no existe correlación lineal evidente entre las variables 
#numéricas 

#Para observar el 'efecto' de las variables categóricas en las variables numéricas haremos los 
#siguientes boxplots 

boxplot(df$monto ~ df$fraude, main='Monto por categoría de fraude',xlab='Fraude',ylab='Monto')

boxplot(df$monto ~ df$os, main='Monto por sistema op.',xlab='os',ylab='Monto')

boxplot(df$monto ~ df$is_prime, main='Monto por suscrp. prime.',xlab='prime',ylab='Monto')

boxplot(df$monto ~ df$genero, main='Monto por género.',xlab='genero',ylab='Monto')

boxplot(df$monto ~ df$ciudad, main='Monto por ciudad',xlab='ciudad',ylab='Monto')

boxplot(df$monto ~ df$establecimiento, main='Monto por establecimiento',xlab='establecimiento',ylab='Monto')

boxplot(df$monto ~ df$hora, main='Monto por hora',xlab='Hora',ylab='Monto')

boxplot(df$linea_tc ~ df$genero, main='linea de crédito por genero',xlab='genero',ylab='Monto')

boxplot(df$linea_tc ~ df$fraude, main='Línea de crédito por categoría de fraude',xlab='Fraude',ylab='Línea de crédto')

boxplot(df$interes_tc ~ df$fraude, main='Tasa de interés por categoría de fraude',xlab='Fraude',ylab='Tasa de interés')



#al parecer todos los grupos que se generan están balanceados en la muestra 

#En conclusión, no se encontraron patrones o indicios de algún mecanismo extraño que esté sucediendo en la base.

# Procedemos a hacer la clasificación de los usuarios. El algoritmo que se usará
# es un algoritmo de conglomerados no jerarquico de k-medias (para variables) numéricas

#generamos dos bases: una con los datos de variables numéricas y otra con los datos de vars categóricas
df_num <- df %>% select(ID_USER, monto, hora, linea_tc, interes_tc, dcto, cashback, device_score)


# agreagamos los datos a nivel usuario en la base de vars num
df_prom <- df_num %>%
  group_by(ID_USER) %>%
  summarise_all(mean) %>%
  as.data.frame() %>% select(-ID_USER)

row.names(df_prom) <- df_prom$ID_USER


#ahora estandaricemos las variables ya que tienen distintas escalas entre sí 
df_prom <- scale(df_prom) 


#Hacemos pruebas para determinar cuántos clusters hacer

#gráfica de codo
maxk <- 10 #número de clusters a considerar máximo
ESS <- sapply(1:maxk,function(k)kmeans(df_prom,k,nstart=10)$tot.withinss)
plot(1:maxk,ESS, type="b",pch=16)

#la gráfica de codo nos arroja un dibujo donde podemos considerar 2 clusters. Veamos
# que nos arroja la función NbClust. Esta función considera muchas pruebas y escoge como
#la prueba de la siluetam KL, Hartigan, Scott, etc y escoge una ponderación y lo que la 
#el veredicto que tiene más frecuencia tiene. 

#enlace compleeto quiere decir que se tomará el máximo de las distancis entra dos pares
#de ítems que pertnecen cada uno a clusters diferentes
NbClust(data=df_prom, method = "complete")
#En este caso arrojó que 2 clusters es lo correcto.

set.seed(14072022) # fija la semilla por reproducibilidad
km1 <- kmeans(df_prom,centers = 2, nstart = 100)
km1
#el porcentaje de los ejes es el porcentaje de explicación de la variación de los datos 
fviz_cluster(km1, 
             data = df_prom, 
             ellipse.type = "euclid", 
             star.plot = T, 
             repel = T,
             ggtheme = theme(legend.position = "bottom"))
#claramente se aprecian dos clusters 

#el porcentaje de los ejes es el porcentaje de explicación de la variación de los datos 

clusters <- km1$cluster %>% as.data.frame() 
colnames(clusters) <- c('tipo_cliente')
df_prom <- cbind(df_prom, clusters)

#ahora recuperamos la variaable cluster para pegarla a la base grande

library(tibble)
df_prom <- tibble::rownames_to_column(df_prom, "ID_USER")
df <- df %>% mutate(ID_USER=as.character(ID_USER))
df_prom <- df_prom %>% dplyr::select(ID_USER, tipo_cliente)

df <- df %>% left_join(df_prom, by="ID_USER")

#para clasificar a los usuarios usaremos un model logístico sencillo.
#usaremos como variable dependiente la dummy de fraude 
df_log <- df %>% dplyr::select(monto, hora, establecimiento, ciudad, linea_tc,
                        interes_tc, status_txn, dcto, cashback, device_score,
                        os, d_tarj, d_genero, d_is_prime, d_fraude)

#imputamos datos missing con un randomforest para no perder observaciones en la regresión
#library(missForest)
#df_imp <- missForest(df_log)
#df_log <- df_imp$ximp
#se interrumpió por la demora que toma

#para no perder observaciones se hará una imputación arbitraria de la variable género y 
#os. Las variabes establecimiento y ciudad no se harán porque casi la mitad de la muestra tiene missing en esas var.
df_log %>% skim()
df_log$d_genero[is.na(df_log$d_genero)] <- 1#mujer

df_log$os[is.na(df_log$os)] <- "ANDROID" 

df_log <- df_log %>% dplyr::select(-c(establecimiento, ciudad))

#ajustamos modelo logit con toda la especificación para ver significancia de coeficientes
#y poder escoger un modelo más chico
modelo_logit <- glm(d_fraude ~ .:. ,family = binomial(link=logit), data = df_log)
summary(modelo_logit)

#dividimos base en entrenamiento y prueba
set.seed(14072022)
sample <- sample(c(TRUE, FALSE), nrow(df_log), replace=TRUE, prob=c(0.7,0.3))
train <- df_log[sample, ]
test <- df_log[!sample, ]

#nos quedamos con las explicativas que son significativas del modelo y ajustamos modelo
#a muestra de entrenamiento

modelo_logit_red <- glm(d_fraude ~ monto + d_genero + dcto + d_is_prime + linea_tc + d_tarj +
                          device_score + d_is_prime + status_txn + os + cashback +
                          monto:device_score + monto:d_tarj + status_txn:d_genero +
                          dcto:d_tarj + cashback:device_score + monto:os + cashback:d_tarj +
                          os:d_is_prime,
                        family = binomial(link=logit), data = train)

summary(modelo_logit_red)

# Devuelve los valores ajustados, del predictor lineal:
head(predict(modelo_logit_red))
# Devuelve las probabilidades ajustadas:
head(predict(modelo_logit_red,type = "response"))

#hacemos matriz de confusión
library(caret)
predicted <- predict(modelo_logit_red, test, type="response")

#encontremos cota óptima de probabilidad para maximizar precisión
library(InformationValue)
optimal <- optimalCutoff(test$d_fraude, predicted)[1]

#creamos confusion matrix
confusionMatrix(test$d_fraude, predicted)

#Evaluemos nuestra matriz de confusión
#obtendremos las siguientes métricas de nuestra tabla de confusión 
#sensitivity, specificity y total missclasification rate.
library(ISLR)

sensitivity(test$d_fraude, predicted)

specificity(test$d_fraude, predicted)

misClassError(test$d_fraude, predicted, threshold=optimal)

#tiene un error de misclassification muy bajo por lo que el modelo no es tan malo para predecir. 
