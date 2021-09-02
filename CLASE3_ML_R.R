
setwd("D:\\R_MACHINE_LEARNING")   ## Establecemos directorio de trabajo

## Cargamos librerías
library(tidyverse)
library(readr)
library(caret)

## Leemos  bases
seguros<- read_csv("BASES\\insurance.csv", col_names=T,
                  col_types= list("i", col_factor(levels=NULL), "d", col_factor(levels=NULL), 
                                  col_factor(levels=c('no','yes')), col_factor(levels=NULL),"d"),
                  na= c("","NA","?"))
colnames(seguros)
seguros

## Leemos la base Customer Churn (Para tener una variable objetivo categórica)
abandono<- read_csv("BASES\\Customer_Churn.csv", col_names=T, na= c("","NA","?") )
colnames(abandono)
abandono %>% view()

## Leemos la base AUTO
auto<- read_csv("BASES\\auto.csv")
colnames(auto)
auto

## Generemos las funciones auxiliares
## YA

## carguemos las funciones auxiliares
source("funcionesAuxiliares.R")

datosFaltantes(abandono) %>% filter(Nulos>0)
encuentraCat(abandono)
encuentraNum(abandono)

## Paea generar pdf interactivo
# rmarkdown

######################################################
### Introducción de variables categóricas al modelo

summary(lm(Costo~ IMC + Fumador + Hijos + Region + Sexo, data= seguros))
## Para variables numéricas: Las betas indican el costo adicional de aumentar una unidad
## Para variables categóricas: Las betas indican el costo adiconal de tener la propiedad x respecto 
# a la clase basal

summary(seguros$Hijos)

# Para esto es necesario transformar las variables categóricas
# en las correspondientes dummies

## dummyVars(Formula, data, sep, levelsOnly, fullRank)

# Fórmula: Fórmula para indicar las variables sobre las que se debe calcular las dummies
# data: Base de datos a trabajar
# sep: Separador entre el nombre de la variable y la categoría al generar las dummies
# levelsOnly: TRUE para que el nombre de la variable no aparezca como prefijo en las dummies
# fullRank: TRUE para quitar la primer categoría antes de generar las variables dummies

# Le indicamos a R quevariables queremos transformar a dummies y como lo haga 
genDummies<- dummyVars( ~ Sexo + Fumador, data= seguros, sep="_", levelsOnly= F, fullRan= T)

# Obtenemos dummies
head(  predict(genDummies, seguros), 5  )


# Le indicamos a R quevariables queremos transformar a dummies y como lo haga 
genDummies<- dummyVars(Costo ~ ., data= seguros, sep="_", levelsOnly= F, fullRan= T)
head(  predict(genDummies, seguros), 5  )


## Datos Con zero varianza
# * Indice de frecuencia: El valor más frecuente sobre el segundo valor más frecuente. Cercano a 1 para variables
#                         con buen comportamiento y mientras mayor sea, la variable es mas desequilibrada
# * Porcentaje de valores únicos: Número de valores únicos dividido por el número total de muestras.
#                                 Se acerca a cero a medida que aumenta la granularidad de los datos

##############################################
### nearZeroVar(x, freqCut= 95/5, uniqueCut= 10, saveMetrics= FALSE, names= FALSE)
##############################################
# x: Base de datos para analizar.
# freqCut: Punto de corte del índice de frecuencia.
# uniqueCut: Punto de corte del porcentaje de valores únicos.
# saveMetrics: Si es FALSE, muestra los índices de las columnas que sobrepasan los puntos de corte.
#              en caso contrario, regresa un dataframe con los indicadores.
# names: Si es FALSO, se regresan los índices de las columnas, en caso contrario regresa los nombres.

# Si nos interesa medir estos índices sobre los valores de una variable y podemos usar la función

##############################################
# checkConditionalX(x, y)  donde, "y" es una variable aleatoria categórica
##############################################

seguros %>% nearZeroVar(saveMetrics=TRUE)

table(seguros$Hijos )

seguros %>% 
  mutate(Hijos_desb= if_else(Hijos %in%c("0","1","2","3"), "pocos", as.character(Hijos))) %>% 
  nearZeroVar(saveMetrics=TRUE)

seguro_ej<- seguros %>% 
 mutate(Hijos_desb= if_else(Hijos %in%c("0","1","2","3"), "pocos", as.character(Hijos)))

head(seguro_ej)

quitar<- seguro_ej %>% nearZeroVar(saveMetrics=F, names=T)
seguro_ej %>% select(-quitar)


## Predictores correlacionados
# Algunos modelos como Componentes Principales funcionan mejor cuando hay correlación sin embargo 
# otros como la regresión lineal se afectan por lo que es importante poder detectar a las variables 
# que están correlacionadas

## findCorrelation(x, cutoff= 0.9, verbose= FALSE, names= FALSE)
# x: Matríz de correlación de las variables a testear
# cutoff: Punto de corte para decir que una correlación es alta
# verbose: TRUE para mostrar detalles 
# names: TRUE para regresar nombres de columnas, en otro caso regresa índices

matCor<- seguros %>% 
  select( encuentraNum(seguros)$Columna  ) %>% 
  cor()

varRelacionadas<- findCorrelation(matCor, cutoff = .75, names= TRUE)
varRelacionadas  #cuando no hay variables que pasen el punto de corte no regresa nada

matCorAuto<- auto %>% 
  select( encuentraNum(auto)$Columna  ) %>% 
  cor()

varRelacionadas<- findCorrelation(matCorAuto, cutoff = .75, names= T, verbose = T)
varRelacionadas  # Regresa un vector con las variables que puedo eliminar (correlacionadas)

auto %>% select(-varRelacionadas)


### Preprocesamiento de datos numéricos

## preProcess(x, method = c("center", "scale"), na.remove = TRUE,
##            k = 5, knnSummary = mean, verbose = FALSE,  freqCut = 95/5, cutoff = 0.9)

# x: Dataframe a trabajar
# method = Vector con las tranformación que se aplicarán a x, las opciones son "center", "scale",
#          "knnImpute", "bagImpute", "medianImpute", "corr"= findCorrelation, "nzv"= nearZeroVar
# na.remove: TRUE para eliminantes
# k: Núr datos faltamero de vecinos para knnImpute
# knnSummary: Métrica a calcular para la imputación de valores
# freqCut: Parámetro para nearZeroVar 
# cutoff: Parámetro para findCorrelation
# verbose: TRUE Para tener información de las trasnformaciones aplicadas durante el cálculo

### Apliquemos la funcion a la base de seguros para tener una base mas limpia
#seguros, auto, abandono
## Al aplicar el preprocesamiento hay que quitar la variable "Costo" para que no se vea afectada
peProc<- seguros %>% select(-Costo) %>% 
  preProcess(method= c("center", "scale", "nzv", "corr", "knnImpute"), 
             k= 5, knnSummary= mean, cutOff= .75)

## Obtenemos índices de filas para base de entrenamienbto y validación
set.seed(1)
muestra<- createDataPartition(seguros$Costo, p=0.7, list=F, groups= 10)

## Generamos bases
entrena<- predict(peProc, seguros[muestra, ]  )
valida<- predict(peProc, seguros[-muestra, ]  )

## Generamos dummies
genDummies<- dummyVars(~ ., data= seguros, sep="_", levelsOnly= F, fullRan= T)
entrena<- predict(genDummies, entrena)
valida<- predict(genDummies, valida)

## Generamos modelo con las variables procesadas
Mod<- lm(Costo~ ., data= data.frame(entrena))
Mod
summary(Mod)

#####################################################
#### Selección del mejor modelo basado en AIC
Mod_Lin<- lm(Costo~ (Edad + IMC + Hijos_1 + Hijos_2 +Hijos_3 + Hijos_4 + Hijos_5 + Sexo_male +
                     Region_southeast + Region_northeast + Region_northwest)*(Fumador_yes),
             data= data.frame(entrena))
summary(Mod_Lin)

step(Mod_Lin, direction = "both", trace= 2, steps= 2000)
# direction: agregar o quitar Variables, "backward", "forward", "both"
# trace: Cuanta información imprime en pantalla R
# steps: Límite superior de ciclos en el proceso

Mejor_Modelo<- lm(Costo~ Edad + IMC + Hijos_1 + Hijos_2 + Hijos_3 + 
                    Hijos_4 + Region_southeast + Region_northeast + Region_northwest + 
                    Fumador_yes + IMC:Fumador_yes + Region_southeast:Fumador_yes + 
                    Region_northwest:Fumador_yes, data= data.frame(entrena))

summary(Mejor_Modelo)

## Base de Validación 
pred<- predict(Mejor_Modelo, data.frame(valida))
real<- data.frame(valida)$Costo

ggplot() +
  geom_line(mapping = aes(x= seq(1:50), y= pred[1:50], col= "red"))+
  geom_line(mapping = aes(x= seq(1:50), y= real[1:50], col= "blue"))

e<- data.frame(Resid= real/pred-1)
summary(e)

e %>% 
  ggplot(aes(Resid)) +
  geom_histogram()

#################
#### Ejercicio
# * Cargar la base auto con los tipos de dato correcto
# * Preprocesar la base con la función "preProcess"
# * Generar las variables dummies para el modelo
# * Generar base de entrenamiento y validación
# * Generar modelo de regresión lineal con la base auto
# * Encontrar la mejor combinación de variables para el modelo con la función step()

auto<- read_csv("BASES\\auto.csv", col_types= cols(cilindros= col_factor(levels = NULL),
                                                   origin= col_factor(levels = NULL)),
                na= c("NA", "", "?"))

prepeocesa_auto<- auto %>% select(-mgp) %>% 
  preProcess(method = c("center", "scale", "knnImpute", "corr", "nzv"),
             k= 5, knnSummary = mean, verbose = FALSE,  freqCut = 95/5, cutoff = 0.8)

## Obtenemos índices de columnas para base de entrenamienbto y validación
set.seed(5)
muestra<- createDataPartition(auto$mgp, p=0.7, list=F, groups= 10)

## Generamos bases
entrena<- predict(prepeocesa_auto, auto[muestra, ]  )
valida<- predict(prepeocesa_auto, auto[-muestra, ]  )

## Generamos modelo de regresión lineal
Modelo_auto<- lm(mgp~ cilindros*peso*aceleracion*origin,
                 data= entrena)

step(Modelo_auto, direction = "both", trace= 2, steps= 5000)


Modelo_auto  <- lm(mgp~ cilindros + peso + aceleracion + origin + cilindros:peso + 
                     peso:aceleracion, data= entrena)
summary(Modelo_auto)

pred_entrena<- predict(Modelo_auto, entrena)
e<- data.frame(Resid= pred_entrena/entrena$mgp-1)
summary(e)

e %>% ggplot(aes(Resid)) + geom_histogram()

## Veamos algunos ejemplos de las predicciones 
head(cbind(entrena$mgp,pred_entrena , e),20)

pred_valida<- predict(Modelo_auto, valida)
e<- data.frame(Resid= pred_valida/valida$mgp-1)
summary(e)

e %>% 
  ggplot(aes(Resid)) + 
  geom_histogram()


##################################################
################### REGRESIÓN LOGÍSTICA

credito<- read_csv("BASES\\german_credit_data.csv", na= c("NA","","?"),
                   col_types= cols(Sexo= col_factor(), Resultado= col_factor())
)
credito

datosFaltantes(credito)
dim(credito)

# * Sustituir
# * Elmiminar
# * A veces los valores nulos proporcionan información

credito<- credito %>% 
  replace_na(list(Cuenta_Corriente = "SIN_CTA", Cuenta_Ahorro= "SIN_CTA"))



# Las variables de tipo factor no permiten agregar nuevas clases como en cuenta de ahorro
# para agregar una nueva clase a una variable de tipo factor hay que transformarla 
# primero a caracter, agregarle la nueva variable y después la regresamos a tipo factor

# as.character()
# agregamos nueva clase
# as.factor()

