# limpio la memoria
rm( list=ls() )
gc()

# Cargo librerias
library(PerformanceAnalytics) # chart.Correlation
library(car) #influencePlot
library(MASS) #boxcox
library(lmtest) #dwtest

# Dataset: Fish
# URL: https://www.kaggle.com/aungpyaeap/fish-market
# Atributos
    # Species: 7 tipos de especies mas comunes en los mercados.
    # Lenght1 / Lenght2 / Lenght3: distintas mediciones de largo.
    # Height: alto del animal.
    # Width: ancho del animal.
    # Weight: peso del animal.

####################################
#   Carga & Limpieza de Dataset
####################################

# Cargo Datasets
fish <- read.csv('data/fish.csv', stringsAsFactors=T)

# Imputo media a observacion con target = 0
fish[41,2] <- mean(c(120,110,120))

summary(fish)

chart.Correlation(fish[,2:7], histogram = TRUE, lm = TRUE, method = "pearson")

# Existe una correlacion positiva casi perfecta entre las variables Length1, Length2 & Length3.
# Todas las variables estan fuertemente correlacionadas positivamente con la variable target.
# Se procede a imputar la media de Weigth a la observacion 41. La cual tiene un peso = 0.
# Esto debe haber sido un error de carga en el dataset.
# Se tomaron 3 observaciones de la misma especie (Roach) con medidas similar de Largo, Alto & Ancho.
# Esta serian las observaciones 40,42 & 43

####################################
#   Modelo 1: Todas las variables
####################################

# Armo Modelo con todas las variables
modelo_m1 <- lm(Weight ~ ., data = fish )
summary(modelo_m1)

# Luego de inspeccion visual solamente seria significativas las varias: Species & Length1

# Uso modelo de seleccion automatica
step(modelo_m1, direction = "backward")

##############################################
#   Modelo 2: Solo Variables Significativas
##############################################

# Elimino las Variables No Significativas 
# No hay variacion en el AIC & me quedo con un modelo parsimonioso

modelo_m2 <- lm(Weight ~ Species + Length1, data = fish)

summary(modelo_m2)

####################################
#         Compruebo Supuestos
####################################

# 1) Normalidad (Test de Shapiro-Wilk, Histograma & qq-plot)
shapiro.test(modelo_m2$residuals) 
hist(modelo_m2$residuals, breaks=10)
qqPlot(rstandard(modelo_m2))

# 2) Homocedasticidad (Test de Breush-Pagan)
bptest(modelo_m2)
plot(fitted(modelo_m2), rstandard(modelo_m2))

# 3) Calculo la Media de los Residuos
summary(modelo_m2$residuals)

# 4) Test de Autocorrelacion de los Errores (Durbin-Watson)
dwtest(modelo_m2) 

# Conclusiones:
# Rechazo la normalidad de los residuos.
# NO rechazo la homocedasticidad de los residuos.
# Los residuos tienen media = 0.
# Rechazo la H0 de NO Autocorrelacion.

####################################
#   Transformacion de Box & Cox
####################################

# Saca Casos con Target <= 0 
# (Para poder aplicar Box&Cox - sino da error xq todos los resultados deben ser positivos)

fish2 <- fish[fish$Weight>0,]

bc <- boxcox(Weight ~ Species + Length1, data = fish2)

lambdas<- bc$x
lik <- bc$y

resultados_bc <- cbind(lambdas, lik)
resultados_bc <- resultados_bc[order(-lik),]
lambda <- resultados_bc[1,1]

head(resultados_bc)

####################################
#   Modelo 3: Post Transformacion
####################################

modelo_m3 <- lm(Weight^(lambda) ~ Species + Length1, data = fish2)

summary(modelo_m3)

# 1) Normalidad (Shapiro-Wilk)
shapiro.test(modelo_m3$residuals) 

# Se rechaza el supuesto de normalidad.

####################################
#   Modelo 4: Robusto
####################################

modelo_rob <- rlm(Weight ~ Species + Length1, data= fish)

summary(modelo_rob)

# Calculo los p-value
coefs= as.data.frame(summary(modelo_rob)$coefficients) 
coefs$pvalue<- 2*(1-pt(q=abs(coefs$`t value`),df=nrow(fish)-3))
coefs

# Obtengo el peso de las observaciones
wts <- modelo_rob$w 
names(wts)<-rownames(fish)
sort(wts)


####################################
#          Conclusiones
####################################

# Luego de haber probado distintas alternativas
# el modelo robusto reduce el std error del modelo original (modelo_m2)
# manteniendo las mismas variables significativas.
# Sin embargo, el modelo ajustado por el metodo Box&Cox presenta un R2 Ajustado
# superior al modelo original pero el supuesto de normalidad de los residuos tampoco
# se cumple en este caso. Se recomienda utilizar el modelo robusto.
