
setwd("D:\\R_Machine_Learning")   ## Establecemos directorio de trabajo

## Instalemos librerías que necesitaremos mas adelante
install.packages("randomForest")
install.packages("ROCR")
install.packages("pROC")
install.packages("corrplot")

## Cargamos librerías
library(tidyverse)
library(readr)
library(caret)

credito<- read_csv("BASES\\german_credit_data.csv", na= c("NA","","?"),
                   col_types= list(Sexo= col_factor(), Resultado= col_factor()))
credito

## SUSTITUIMOS NA's
credito<- credito %>%
  replace_na(list(Cuenta_Ahorro= "SIN_CTA", Cuenta_Corriente= "SIN_CTA"))

## Categorizamos la variable periodo
credito<- credito %>% 
  mutate(Periodo_Cat= ifelse(Periodo_Meses<=12, 'a.<=1',
                             ifelse(Periodo_Meses<=24, 'b.1-2',
                                    ifelse(Periodo_Meses<=36, 'c.2-3','d.>3'))))

############################################
###### Muestra de entrenamiento y validación
set.seed(15)

credito |> group_by(Resultado) |> summarise(n())
muestra<- createDataPartition(credito$Resultado, p= 0.75, list= F)
entrena<- credito[muestra, ]
valida<- credito[-muestra, ]

## Mantenemos el 70-30 de la variable objetivo
entrena |> group_by(Resultado) |> summarise(obs= n()) |> mutate(obs/sum(obs))
valida |> group_by(Resultado) |> summarise(obs= n())|> mutate(obs/sum(obs))


##########################################################
##################  Árboles de Decisión  #################
library(rpart)
library(rpart.plot)

MiArbol<- rpart(Resultado ~ Edad + Sexo + Cuenta_Ahorro + Cuenta_Corriente + Vivienda + 
                  Periodo_Cat + Tipo,                   # Fórmula
                data= entrena,                         # Base datos
                method= "class",                       # "anova", "poison", "class", "exp"
                parms= list( # Matríz de costos
                            split= 'information'),                      # Método de split "giny", "information"
                control= rpart.control(cp= 0.001,            # Factor de complejidad
                                       minsplit= 50,         # Número de elementos para generar split adicional
                                       minbucket= 30,        # Elementos para que un nodo sea terminal
                                       maxdepth= 9,         # Profundidad del árbol
                                       xval= 3               # Validacion Cruzada
                                       )
)

#########################
### Matríz de confusión

# Entrenamiento
prediccion_ent<- predict(MiArbol, entrena, type= "class")  # type= "class", "prob"
matconf_ent<- table(entrena$Resultado, prediccion_ent, dnn= c("Real", "Prediccion"))
matconf_ent
prop.table(matconf_ent, 1)

# Validación
prediccion_val<- predict(MiArbol, valida, type= "class")
matconf_val<- table(valida$Resultado, prediccion_val)
matconf_val
prop.table(matconf_val,1)

#############################
### Visualización del Arbol
MiArbol          # Nos indica el camino que sigue el arbol para el ajuste
plotcp(MiArbol)  # Gráfica de nivel de ajuste del arbol con diferentes factores de complejidad
MiArbol$cptable

### La poda de arbol, se genera con el factor de complejidad
MiArbol_poda<- prune(MiArbol, MiArbol$cptable[2, "CP"])
MiArbol_poda

# Visualización para comparar Arbol_Normal vs Arbol_Podado
rpart.plot(MiArbol)
rpart.plot(MiArbol_poda)

### Matríz de confusión con Arbol podado
# Entrenamiento
prediccion_ent<- predict(MiArbol_poda, entrena, type= "class")  # type= "class", "prob"
matconf_ent<- table(entrena$Resultado, prediccion_ent, dnn= c("Real", "Prediccion"))
matconf_ent
prop.table(matconf_ent, 1)

# Validación
prediccion_val<- predict(MiArbol_poda, valida, type= "class")
matconf_val<- table(valida$Resultado, prediccion_val)
matconf_val
prop.table(matconf_val,1)

### Disminuye el poder prtedictivo del arbol a cambio de ganar interpretabilidad
### y facilidad de aplicación si se pioensa implementar en un sistema de tiempo real


##############################
#### Arbol de regresión
##########################

## Leemos la base AUTO
auto<- read_csv("BASES\\auto.csv", col_names=T,
                col_types=list("d",col_factor(levels=NULL), "d","i","d","d",col_factor(levels=NULL),
                               col_factor(levels=NULL),"c"), na= c("","NA","?") )

auto |>
  group_by(cilindros) |>   
  summarise(n())

auto<- auto %>%
  filter(!(cilindros %in% c('3','5')))

auto<- auto %>% 
  replace_na(list(caballos= mean(auto$caballos, na.rm=T)))

## Muestra de entrenamiento y Validación
set.seed(20)
muestra<- createDataPartition(auto$mgp, p=0.7, list=F, groups= 6)
ent_auto<- auto[muestra, ]
val_auto<- auto[-muestra, ]

ArbolReg<- rpart(mgp ~ cilindros + desplasamiento + caballos + origin + aceleracion + 
                   peso,
                 data= ent_auto,
                 method= "anova",
                 control = rpart.control(cp=0.005, minsplit= 10, maxdepth= 10, minbucket= 5))

ArbolReg

## Errores base de entrenamiento
pred_ent<- predict(ArbolReg, ent_auto)
e<- ent_auto$mgp - pred_ent
ggplot() + geom_histogram(aes(e))

summary(e)
shapiro.test(e)
qqnorm(e)
qqline(e)

## Errores base de validacion
pred_val<- predict(ArbolReg, val_auto)
e<- val_auto$mgp - pred_val
ggplot() + geom_histogram(aes(e))

summary(e)
shapiro.test(e)
qqnorm(e)
qqline(e)

##########################################################
##################  BOSQUES ALEATORIOS  ##################
##########################################################

library(randomForest)
entrena<- entrena |> 
  mutate(Trabajo= as.factor(Trabajo), Vivienda= as.factor(Vivienda), Tipo= as.factor(Tipo),
         Cuenta_Ahorro= as.factor(Cuenta_Ahorro), Cuenta_Corriente= as.factor(Cuenta_Corriente))

valida<- valida |> 
  mutate(Trabajo= as.factor(Trabajo), Vivienda= as.factor(Vivienda), Tipo= as.factor(Tipo),
         Cuenta_Ahorro= as.factor(Cuenta_Ahorro), Cuenta_Corriente= as.factor(Cuenta_Corriente))

MiBosque<- randomForest(Resultado ~ Edad + Cuenta_Ahorro + Cuenta_Corriente + Sexo + Trabajo + 
                          Vivienda + Monto_Credito + Periodo_Cat,
                        data= entrena,
                        type= "classification",   # regression, classification o unsupervised
                        ntree= 200,               # Número de árboles en el bosque
                        mtry= 5,                  #   Número de variables a usar en cada árbol
                        nodesize= 60,             # Num elementosd para nodo terminal de cada árbol
                        maxnodes= 16,             # Num máximo de nodos en cada árbol
                        replace= TRUE,            # TRUE para generar muestras distintas en cada arbol
                        classwt= c(1,1.006),      # Matríz de costos para desbalance 
                        strata= "Resultado")      # Variable para estratificar las muestras

MiBosque
plot(MiBosque)
varImp(MiBosque)  # Importancia de las variables

########################
### Matríz de confusión 

# Entrenamiento
pred_bosque<- predict(MiBosque, entrena, type= "class")
mc<- table(entrena$Resultado, pred_bosque, dnn= c("Real", "Prediccion"))
mc
prop.table(mc,1)

# Validación
pred_bosque_val<- predict(MiBosque, valida, type= "class")
mc_val<- table(valida$Resultado, pred_bosque_val, dnn= c("Real", "Prediccion"))
mc_val
prop.table(mc_val, 1)

#################
## Curva Rock
#################
library(ROCR)
library(pROC)

# Paso 1: Obtenemos una predicción con el modelo a testear
pred<- predict(MiBosque, entrena, type= "class")

## Paso 2: Generamos un objeto roc
roc_obj<- roc( as.numeric(pred),  as.numeric(entrena$Resultado) )
roc_obj$auc   # Area bajo la curva

## Paso 3: Graficamos curva roc
ggroc(roc_obj, alpha= 0.5, colour= "blue", linetype="dashed", size= 2, legacy.axes = TRUE) + 
  ggtitle(  paste0( "Area bajo al curva ", round(roc_obj$auc*100,0), "%")) + 
  geom_segment(aes(x=1, xend=0, y=1, yend=0), color= "grey", size= 1.5)

###########################################
##### ANÁLISIS DE COMPONENTES PRINCIPALES

# * Los CP no son correlacionados
# * La solución es única 
# * Cada CP aporta una mantidad de varianza menor conforme tiene mayor índice VAR CP1 > VAR CP2

## las componentes principales se generan con el comando prcomp
comp_prin<- ent_auto |> 
  select(desplasamiento, caballos, peso, aceleracion) |> 
  prcomp( scale= T)

##-------------------------------------------------------------
## Elanálisis de componentes principales fue generado para variables numéricas 
## pero si queremos agregarvariables categóricas podemos hacer uso de las siguientes
## matríces:
# MATRÍZ DE CORRELACIONES TETRACORICA, POLICORICA
# Numerica-numerica
# numerica-categorica
# categorica-categorica
##-------------------------------------------------------------

names(comp_prin)

library(corrplot)
ent_auto %>% select(mgp, desplasamiento, caballos, peso, aceleracion) %>% cor() %>%
  corrplot()

### Como las variables están altamente correlacionadas es muy probable que tengamos una
### reducción de dimensión bastante buena (quedarme con 2 ó 3 CP)

# Análsis y nombramiento de CP
comp_prin

## La primer CP podemos llamarla Rendimiento
## Factor de Fricción
## Las posteriores están dominadas por alguna variable
## por lo que el nombre corrrsponde a la variable dominante

## Proporción de varianza explicada
summary(comp_prin)

### Con 2 CP explicamos el 92% de las varianza original, con 1 CP explicamos el 80% de la varianza
### original por lo tanto podemos trabajar y obtener resultados muy similares con las 5 variables
### originales y |las primeras 2 componentes o posiblemente solo 1.

## Nuevas coordenadas

auto_prc<-data.frame(Rendimiento= comp_prin$x[,1],
                     FacFriccion= comp_prin$x[,2],
                     Auto= ent_auto$nombre,
                     mpg=  ent_auto$mgp
                    )[1:20,]
auto_prc
ggplot(auto_prc) + 
  geom_label(mapping = aes(Rendimiento, FacFriccion, label= Auto))

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library("ggbiplot")

ggbiplot(comp_prin, groups= ent_auto$cilindros, ellipse= T, circle= T, obs.scale=T)
