# limpio la memoria
rm( list=ls() )
gc()

# cargo librerias
library(PerformanceAnalytics) # chart.Correlation
library(questionr) #freq
library(sjPlot) #sjt.xtab
library(ggplot2)
library(ResourceSelection) #hoslem.test
library(caret) #createDataPartition

# Dataset: Haberman's Survival Data
# URL: https://archive.ics.uci.edu/ml/datasets/Haberman's%2BSurvival

# Atributos
    # Edad: edad del paciente al momneto de la cirugia
    # año_cirugia: Año en el que fue realizada la cirugia
    # nodos: Cantidad de nodos encontrados en axilas
    # supervivencia:
        # 0 = el paciente murio antes de los 5 años
        # 1 = el paciente sobrevivio 5 años o mas        

####################################
#   Carga & Limpieza de Dataset
####################################

# Cargo Dataset
haberman <- read.csv('data/haberman.data.txt', header = F)

# Seteo nombre de columnas
colnames(haberman) <- c('edad', 'año_cirugia', 'nodos', 'supervivencia')

# Codifico No Supervivencia = 0
haberman[haberman$supervivencia == 2,4] <- 0

####################################
#       Analizo los Datos
####################################

summary(haberman)

qplot(haberman$edad)
sjt.xtab(haberman$supervivencia,haberman$nodos,show.row.prc = TRUE,show.col.prc = TRUE)
hist(haberman$año_cirugia)
freq(haberman$supervivencia)
freq(haberman$nodos)

# A partir del analisis de los datos se puede observar:
# El 73,53% de los pacientes sobrevivio > 5 años
# La edad media de los pacientes es de 52 años
# El rango de años en los que fueron llevadas a cabo las cirugias fue entre 1958 y 1969
# El 44,44% de los pacientes no tuvo nodos, de los cuales el 86% sobrevivio.

####################################
#       Particiono los Datos
####################################

set.seed(3456)
indice <- createDataPartition(haberman$supervivencia, p=0.70, times=1, list=F)

train <- haberman[indice,]
test <- haberman[-indice,]

####################################
#       Modelo 1
####################################

mylogit <- glm(supervivencia ~ . , data = train, family = "binomial")
summary(mylogit)

# las variables edad & año de cirugia no son significativa.
# se prueba un nuevo modelo sin ellas.

####################################
#       Modelo 2
####################################

mylogit <- glm(supervivencia ~ nodos , data = train, family = "binomial")
summary(mylogit)

# Test Hosmer-Lemeshow
hoslem.test(train$supervivencia, mylogit$fitted.values)

# Odds Ratio
coef(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# Aplico el modelo al dataset de test
preds <- predict(mylogit, test, type='response')
preds <- ifelse(preds > 0.5,1,0)

# Realizo Matriz de Confusion
table(test$supervivencia, preds)

####################################
#       Conclusiones
####################################

# Accuracy del modelo 73,62%
# Error de Clasificacion 26,37%
# Se utilizo un segundo modelo con 2 variables menos, el cual solo disminuye 1 punto el AIC.
# Se realizo test de Hosmer-Lemeshow y no se rechaza la hipotesis nula, por lo tanto, la distribucíon de observados y predichos es similar.
# El coeficiente nodos es -0.072, por lo tanto, a medida que disminuye aumenta la probabilidad de supervivencia.
# El odds ratio de nodos es cercano a 1, por lo tanto es significativo.
