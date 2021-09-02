
setwd("C:\\Users\\CAE_C\\Documents\\R_ML_MAY2020")

## Cargamos librerías
library(tidyverse)
library(readr)

## Leemos  base de seguros
seguros<- read_csv(".\\BASES\\insurance.csv", col_names = T, 
                   col_types = cols(Sexo= col_factor(levels=NULL), Fumador=col_factor(NULL), 
                                    Region= col_factor(NULL), Hijos= col_factor(levels=NULL)),
                   na= c("NA", "#N/A", ".", "NaN"))

auto<- read_csv(".\\BASES\\auto.csv", col_names = T, 
                col_types = cols(cilindros= col_factor(levels=NULL), origin=col_factor(NULL)),
                na= c("NA", "#N/A", ".", "NaN", "?"))

#############################################
##### Variable Categórica vs Categórica

## Cargamos la base de accidentes
accidentes<- read_csv('.\\BASES\\Accident_Information.csv', col_names=T)

accidentes[1:100,] %>% view()

### Ajustamos el tipo de accidente
accidentes %>% 
  group_by(Accident_Severity) %>% 
  summarise(obs=n())

accidentes<- accidentes %>% 
  mutate(Accident_Severity= if_else(Accident_Severity %in% c("Fatal", "Serious"), 
                                    "Serious",Accident_Severity))

accidentes %>% 
  group_by(Accident_Severity) %>% 
  summarise(obs=n())

# Creamos una variable de Hora del accidente categórica
install.packages("lubridate")
library(lubridate)  # Librería para manipulación datetime

# Trnsformemos la variable de hora en categórica
accidentes<- accidentes %>% 
  mutate(Periodo_Dia= if_else(Time<= hm("07:00"), "Madrugada",
                              if_else(Time<= hm("12:00"), "Manana",
                                      if_else(Time<= hm("20:00"), "Tarde","Noche")
                              )
                      )
  )

#var_cat<- c()
#for(i in colnames(data.frame(accidentes))){
#  if(class(data.frame(accidentes)[,i]) %in% c("factor", "caracter")){ 
#    var_cat<-c(var_cat, i)
#  }
#}

#var_num<- c()
#for(i in colnames(accidentes)){
#  if( !(class(data.frame(accidentes)[,i]) %in% c("factor", "caracter"))  ){ var_num<-c(var_num, i) }
#}

accidentes %>% 
  group_by(Periodo_Dia) %>%
  summarise(OBS= n()) %>% 
  mutate(porc_obs= OBS/dim(accidentes)[1])

# Veamos si el día de la semana ayuda a predecir el tipo de accidente

###### Gráfico de Mosaico
mosaicplot(~ Accident_Severity + Day_of_Week, accidentes, color=TRUE, shade=T)

#################
## Pueba Ji-cuadrada de correlación

#1. Generamos la tabla de contingencia: 
tabla<- accidentes %>% 
  select(Day_of_Week, Accident_Severity) %>% 
  table()
tabla

#2. Prueba ji-ciadrada para relación de variables categóricas
ji_cuad<- chisq.test(tabla)
names(ji_cuad)

ji_cuad$statistic   # Estadístico ji-cuadradada
ji_cuad$p.value     # Pi-value de la prueba (<=0.05 Entonces hay relación entre las variables) 
ji_cuad$observed    # Matríz de entrada
ji_cuad$expected    # Matríz calculada si las variables fueran independientes 
ji_cuad$residuals    # Matríz estandarizada de las diferencias entre observado vs esperados

### El tamaño de muestra en la prueba ji-cuadrada

i=100
for(i in seq(100,1000, by=100)){
  print(paste("Pi-value ", chisq.test(tabla/i)$p.value, 
              "con tamaño de muestra ", sum(tabla/i) , sep=" "))
}

### Riesgo Relativo
## Veamos si el número de autos involucrados se relaciona con los
## tipos de accidente

accidentes %>% 
  group_by(Number_of_Vehicles) %>% 
  summarise(OBS= n()) %>%
  mutate(POR_OBS= OBS/dim(accidentes)[1])
  
## Hacemos una prueba de la variable
accidentes %>% 
  mutate(Involucrados= if_else(Number_of_Vehicles>=3,"3+",
                               if_else(Number_of_Vehicles==1,"1","2"))) %>% 
  group_by(Involucrados) %>% 
  summarise(OBS= n())

## Guardamos la variable nueva
accidentes<- accidentes %>% 
  mutate(Involucrados= if_else(Number_of_Vehicles>=3,"3+",
                               if_else(Number_of_Vehicles==1,"1","2")))
  
#1. Generamos la tabla de contingencia: 
tabla<- accidentes %>% 
  select(Involucrados, Accident_Severity) %>% 
  table()

prop.table(tabla)
#2. Prueba Ji-cuadrada para relación
ji_cuad<- chisq.test(tabla)
ji_cuad$p.value
ji_cuad$expected
ji_cuad$residuals

# Para el RR queremoes calcular 
# p(serio|1auto)= (#(serio y 1auto)/N) / (#(1auto)/N)= #(serio y 1auto)/#(1auto)
p_1<- accidentes %>% filter(Accident_Severity=="Serious" & Involucrados=="1") %>% 
  dim() %>% .[1] / accidentes %>% filter(Involucrados=="1") %>% dim() %>% .[1]
p_1

p_3<- accidentes %>% filter(Accident_Severity=="Serious" & Involucrados=="3+") %>% 
  dim() %>% .[1] / accidentes %>% filter(Involucrados=="3+") %>% dim() %>% .[1]
p_3

p_2<- accidentes %>% filter(Accident_Severity=="Serious" & Involucrados=="2") %>% 
  dim() %>% .[1] / accidentes %>% filter(Involucrados=="2") %>% dim() %>% .[1]
p_2

RR_1<- p_1/p_3
RR_1  # Riesgo Relativo
print(" El riesgo relativo de tener un accidente severo es 59% mayor con 1 auto que con 3")

RR_2<- p_2/p_3
1/RR_2  # Riesgo Relativo
print(" El riesgo relativo de tener un accidente severo es 12% mayor con 3 auto que con 2")


### Odds Ratio de accidentes
t<- table(accidentes$Involucrados, accidentes$Accident_Severity)
t
          #P_Aserio 1Auto   /  #P_NoSerio 1Auto
odds_1<- (t[1,1]/sum(t[1,]))/(t[1,2]/sum(t[1,]))
odds_2<- (t[2,1]/sum(t[2,]))/(t[2,2]/sum(t[2,]))
odds_3<- (t[3,1]/sum(t[3,]))/(t[3,2]/sum(t[3,]))

OR_1<- odds_1/odds_3
OR_1  # Probabilidad de accidente serio 1 auto es 76% mayor que con 3 autos

OR_2<- odds_2/odds_3
1/OR_2 # Probabilidad de accidente serio 3 autos es 14% mayor que con 2 autos

## Con R podemos obtenerlo
library(questionr)

t<- table(accidentes$Involucrados, accidentes$Accident_Severity)
t
## cATEGORÍA BASAL 3+ AUTOS
OR_1<- odds.ratio(t[c(1,3),], levels= 0.95)
OR_2<- odds.ratio(t[c(2,3),], levels= 0.95)

## Medida de fuerza de asocicación
# Las pruebas anteriores nos dicen si hay relación entre las variables o no
# el odds ratio y RR nos indican una fuerza de relación entre elementos no entre variables
library(vcd)

accidentes %>% 
  select(Involucrados, Accident_Severity) %>% 
  table() %>% 
  assocstats()  # 0-1

#########################################################
#### Un caso especial: Discreta vs Discreta  (WoE) & IV

## Se utiliza mayormente con variables numérica vs categórica pero 
## discretizamos la varibale numerica

install.packages("InformationValue")
library(InformationValue)

accidentes %>% select(Periodo_Dia, Accident_Severity) %>%  table() %>%  prop.table()

obj<- if_else(accidentes$Accident_Severity=="Serious", 1, 0)

# Calcular tabla WoE
tw<- WOETable(X= as.factor(accidentes$Periodo_Dia), 
              Y=obj,
              valueOfGood = 1)

tw %>% view()

IV<- sum(tw$IV)  # Ver tabla para interpretar resultado

##################################################################
######################    MODELOS    #############################
##################################################################

### Generamos base de entrenamiento y validación
install.packages("caret")
library(caret)

## Obtenemos índices de las filas 70% Entrenar - 30% Validar
muestra<- createDataPartition(seguros$Costo, p=0.7, list= F, groups = 6)

## Generamos base de entrenamiento y validación
entrena<- seguros[muestra, ]
valida<- seguros[-muestra, ]

dim(entrena)
dim(valida)

dim(entrena)[1]/dim(seguros)[1]  # Porcentaje de base de entrenamiento
dim(valida)[1]/dim(seguros)[1]   # Porcentaje de base de validación

## General modelo de regresión para apredecir el costo del seguro basado en
## las características del pasiente 

Modelo_seguros<- lm(Costo ~ Edad + Fumador + IMC + Region, data= entrena)
summary(Modelo_seguros)

## Predicciones con el modelo
predict_ent<- predict(Modelo_seguros, entrena)
e<- entrena$Costo / predict_ent -1
ggplot() + geom_histogram(aes(e))
summary(e)
# 0.177%

##Predicciones con la base de validación  (datos nuevos que no conoce el modelo)
predict_val<- predict(Modelo_seguros, valida)
e_val<- valida$Costo / predict_val -1
ggplot() + geom_histogram(aes(e_val))
summary(e_val)
# -0.07263%

install.packages("sgd")
library(sgd)

Mod_seg_dg<- sgd(Costo~Edad + Fumador + IMC, data= entrena, model = 'lm',
                 sgd.control = list(method= "sgd", lr= "rmsprop", vervose= T, 
                                    reltol= 1e-10))

summary(Mod_seg_dg)

## Predicciones con el modelo
predict_sgd_ent<- predict(Mod_seg_dg, entrena)
e<- entrena$Costo / predict_ent -1
ggplot() + geom_histogram(aes(e))


summary(e)
# 0.177%




