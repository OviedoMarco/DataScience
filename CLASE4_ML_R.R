
setwd("D:\\R_MACHINE_LEARNING")   ## Establecemos directorio de trabajo

## Cargamos librerías
library(tidyverse)
library(readr)
library(caret)

## Cargamos las funciones auxiliares
source("funcionesAuxiliares.R")

###############################
### REGRESIÓN LOGÍSTICA

credito<- read_csv("BASES\\german_credit_data.csv", na= c("NA","","?"),
                   col_types= cols(Sexo= col_factor(),
                                   Resultado= col_factor())
)
credito

credito<- credito %>%
  replace_na(list(Cuenta_Ahorro= "SIN_CTA", 
                  Cuenta_Corriente= "SIN_CTA"))

credito

####################################################
### Separemos las variables numéricas y categóricas
col_cat<- encuentraCat(credito)
col_num<- encuentraNum(credito)
print(col_cat)
print(col_num)

credito |>  view()

credito |> 
  group_by(Periodo_Meses) |>  
  summarise(OBS= n()) |>
  mutate(PORC_OBS= OBS/sum(OBS)) |> 
  arrange(-OBS) |> View()

## Ajustemos la variable de periodo para poder usarla
credito |> mutate(Periodo_Cat= if_else(Periodo_Meses<=12, '<=1',
                                       if_else(Periodo_Meses<=24, '1-2', '2+'))) |> 
  group_by(Periodo_Cat) |> 
  summarise(OBS= n()) |>  mutate(PORC_OBS= OBS/sum(OBS))
# Con esta segmentación las clases quedan balanceadas, la vamos a conservar

credito<- credito |> 
  mutate(Periodo_Cat= if_else(Periodo_Meses<=12, '<=1',
                              if_else(Periodo_Meses<=24, '1-2', '2+')))

## Validación de Grupos
install.packages("InformationValue")
library(InformationValue)

credito |> group_by(Resultado) |>  summarise(n())

## X Variable con grupos (Debe ser factor)
## y Variable Objetivo binaria (Debe ser 0, 1)
t_woe<- WOETable(X= as.factor(credito$Periodo_Cat), Y= credito$Resultado, valueOfGood = 1)
t_woe
sum(t_woe$IV)

WOETable(X= as.factor(credito$Periodo_Meses), Y= credito$Resultado, valueOfGood = 1)

##################################
# Generar Muestra de entrenamiento y validación
set.seed(20)
muestra<- createDataPartition(credito$Resultado, p= 0.75, list=F)

## Generar Bases de entrenamiento y validación
entrena<- credito[muestra, ]
valida<- credito[-muestra, ]

dim(entrena)[1]/dim(credito)[1]
dim(valida)[1]/dim(credito)[1]

#####################################
##### Módelo de Regresión Logística

#############  GLM  #############
modelo_credito<- glm(Resultado~ Edad + Sexo + Periodo_Cat + Cuenta_Corriente + 
                       Cuenta_Ahorro + Monto_Credito, 
                     data= entrena,
                     family = "binomial")

modelo_credito
summary(modelo_credito)
## Buscamos el modelo que minimice la devianza residual
## La devianza nula es la devianza de un modelo que solo se entrena  con un B0
## La devianza del modelo saturado sería el caso contrario a la devianza nula

## Validación de variables paso a paso, compara modelos agregando 1 a 1 las
## variables y genera un test de anovas entre estos modelos
anova(modelo_credito, test = "Chisq")

# Edad -        Queda
# Sexo -        Sale
# Periodo_Cat - Queda
# Cuenta_Corriente - Queda
# Cuenta_Ahorro    - Queda
# Monto_Creito     - Sale

## Generamos el nuevo modelo conquitando las varibles que no generan beneficio
modelo_credito<- glm(Resultado~ Edad + Periodo_Cat + Cuenta_Corriente + 
                       Cuenta_Ahorro, 
                     data= entrena,
                     family = "binomial")

summary(modelo_credito)

## Coeficientes del modelo
coefficients(modelo_credito)
# Intervalos de confianza de los parametros 
confint(modelo_credito, level = 0.95)

## Interpretación de Parámetros
exp(coefficients(modelo_credito))
exp(confint(modelo_credito, level = 0.95))

## Predicciones
valida[1,c("Edad", "Periodo_Cat", "Cuenta_Corriente", "Cuenta_Ahorro")]

# Perfil Persona - 28 Años, PeriodoPago - 2+, CuentaCorriente - Medio, CuentaAhorro - Bajo
BetasxVar= -1.958439165 - (28*0.009622196) +  (0.999045798) + 0.619780999 + 0.911312279

# proba= 1/1+exp(0.3022784)
prob= 1/(1+exp(-BetasxVar))
prob

predict(modelo_credito, valida[1,c("Edad", "Periodo_Cat", "Cuenta_Corriente", "Cuenta_Ahorro")], 
        type= "response")

## Calculamos las probabilidades de todos los elementos de la base entrenamiento
pred_entrena<- predict(modelo_credito, entrena, 
                       type= "response")
## type= "response" Para que la predicción entregue probabilidades

## Basado en las probabilidades calculamos el grupo al que pertenecen
pred_ent_grupo<- if_else(pred_entrena<=0.5, "0","1")
real_ent_grupo<- factor(entrena$Resultado, levels = c(0,1))

## Matríz de Confusión Base de Entrenamiento
Mat_conf<- table(pred_ent_grupo, real_ent_grupo)
acc<- (Mat_conf[1,1] + Mat_conf[2,2])/sum(Mat_conf)
acc0<- (Mat_conf[1,1])/sum(Mat_conf[1,])
acc1<- (Mat_conf[2,2])/sum(Mat_conf[2,])

acc
acc0
acc1

## Matríz de Confusión Base de Validación
# Calculamos las probabilidades de todos los elementos de validacion
pred_valida<- predict(modelo_credito, valida, type= "response")

# Basado en las probabilidades calculamos el grupo al que pertenecen
pred_val_grupo<- if_else(pred_valida<=0.5, "0","1")
real_val_grupo<- factor(valida$Resultado, levels = c(0,1))

## Matríz de Confusión Base de Validación
Mat_conf_val<- table(pred_val_grupo, real_val_grupo)
acc<- (Mat_conf_val[1,1] + Mat_conf_val[2,2])/sum(Mat_conf_val)
acc0<- (Mat_conf_val[1,1])/sum(Mat_conf_val[1,])
acc1<- (Mat_conf_val[2,2])/sum(Mat_conf_val[2,])

acc
acc0
acc1

### SOLUCIONES A CLASES DESBALANCEADAS
## SOBREMUESTREO: Generamos muestras aleatorias de la clase con menos elementos para tratar de 
#                 obtener un  balance del 50 - 50
## SUBMUESTREO: Muestreamos la clase con mas elementos de manera que tengamos una base 
#               representativa y así obtenemos el 50 - 50
## PUNTO_CORTE: Cambiuamos el punto de corte de probabilidades para mover el porcentaje de 
#               buenos y malos que obtengo del modelo
## ENTRENAMIENTO CON CASTIGO: Definir una matríz de costos diferentes para cada categoría, dando mayor
#                             peso al grupo con menos elementos

####################################
##### Selección del punto de Corte
library(InformationValue)

## Calcular prediccion de la base
prediccion<- predict(modelo_credito, credito, type= "response")

## Generamos grupos de probabilidades
prediccion_gpo<- if_else(prediccion<0.1, "0-10",
                     if_else(prediccion<0.2, "10-20",
                         if_else(prediccion<0.3, "20-30",
                             if_else(prediccion<0.4, "30-40",
                                 if_else(prediccion<0.5, "40-50",
                                     if_else(prediccion<0.6, "50-60",
                                         if_else(prediccion<0.7, "60-70",
                                             if_else(prediccion<0.8, "70-80",
                                                 if_else(prediccion<0.9, "80-90", "90-100"
                                                 )
                                             )
                                         )
                                     )
                                 )
                             )
                         )
                     )
)

grupos_a_generar<- 20
prediccion_gpo<- floor(prediccion*grupos_a_generar)/grupos_a_generar
unique(grupos_a_generar)


## Cálculo de la tabla WoE
## Y debe ser numérica 0, 1
t_woe_pc<- WOETable(X= as.factor(prediccion_gpo), Y= as.numeric(credito$Resultado), valueOfGood = 1)
t_woe_pc

## El punto de corte se elige en donde hay cambio de signo

table(credito$Resultado)
## Matriz de confusión con nuevo punto de corte
grupos_pred<- factor(if_else(prediccion>=0.3, 0, 1), levels=c(0,1))
MC_PC<- table(grupos_pred, factor(credito$Resultado, levels=c(0,1)))
MC_PC
prop.table(MC_PC, 2)

##########################
### Ejercicio
# Generar modelo logístico 
# con la base cardio
cardio<- read_csv("BASES\\CARDIO.csv")
cardio<- cardio |> 
  mutate(Objetivo= factor(if_else(Padecimiento=="Con_Pad", 0, 1), levels= c(0,1)))

cardio$Padecimiento

# Generar Muestra de entrenamiento y validación
set.seed(20)
muestra<- createDataPartition(cardio$Padecimiento, p= 0.75, list=F)

## Generar Bases de entrenamiento y validación
entrena<- cardio[muestra, ]
valida<- cardio[-muestra, ]

dim(entrena)[1]/dim(cardio)[1]
dim(valida)[1]/dim(cardio)[1]

##### Módelo de Regresión Logística
modelo_cardio<- glm(Objetivo~ Sexo + RitmoCard + Presion_Art + Colesterol + ca,
                     data= entrena,
                     family = "binomial")

modelo_cardio
summary(modelo_cardio)

anova(modelo_cardio, test = "Chisq")

# Matríz de confusión
proba<- predict(modelo_cardio, entrena, type="response")
pred_ent<- if_else(proba>0.5,"1","0")
real_ent<- entrena$Objetivo
mc<- table(real_ent, pred_ent)
prop.table(mc, 1)

proba_val<- predict(modelo_cardio, valida, type="response")
pred_val<- if_else(proba_val>0.5,"1","0")
real_val<- valida$Objetivo
mc<- table(real_val, pred_val)
prop.table(mc, 1)

########### TAREA OPCIONAL 
## Generar un mejor modelo con el proceso de
## quitar y agregar variables


##########################################################
##################  Árboles de Decisión  #################

#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)       # Librería para generar árboles
library(rpart.plot)  # Librería para graficar árboles
library(caret)

set.seed(20)
muestra<- createDataPartition(credito$Resultado, p= 0.75, list=F)
entrena<- credito[muestra,] 
valida<- credito[-muestra,]

MiArbool<- rpart(Resultado ~ Edad + Cuenta_Corriente + Cuenta_Ahorro + Periodo_Cat + Vivienda + 
                   Trabajo + Sexo + Tipo, 
                 data= entrena,
                 method = "class",  # anova, poison, class, exp
                 parms= list(loss= matrix(c(0,1,1,0), byrow= TRUE, nrow=2), # Matríz de costos
                              split= "gini",               # Método para split "gini", "information"
                              prior= c(0.3, 0.7)           # Probabilidades a priori
                              ),
                 control = rpart.control(cp=0.001,  # Un split que no genera una disminución de cp no se realiza
                                         minsplit= 20,  # Número mínimo de observaciones para realizar un split
                                         maxdepth= 4,   # Profundidad mÃ¡xima
                                         minbucket= 5,  # Número de elementos para que un nodo sea terminal
                                         xval= 5)       # Validaciones cruzadas
)

MiArbool

