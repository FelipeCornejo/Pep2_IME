# EP-13
# Inf. y Modelos Estadísticos
# Arturo Cadenas (20.468.370-0)
# Claudio Muñoz (20.003.395-7)
# Bryan Salas (19.316.410-2)
# Miguel Salinas (20.215.515-4)

# Librerías
if (!require(readxl) ) {
  install.packages("readxl", dependencies = TRUE )
  require (readxl)
}
if (!require(ggpubr) ) {
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}
if (!require(dplyr) ) {
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}
if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}
if (!require(corrplot)){
  install.packages("corrplot", dependencies = TRUE )
  require (corrplot)
}
if (!require(caret)){
  install.packages("caret", dependencies = TRUE )
  require (caret)
}
if (!require(pROC)){
  install.packages("pROC", dependencies = TRUE )
  require (pROC)
}

# Se cargan de datos.
datos <- read.csv2(file.choose(),header=TRUE)
datos_num <- as.data.frame(apply(datos, 2, as.numeric))
end <- nrow(datos)
# Se calcula el IMC
valores_IMC <- as.data.frame(datos_num$Weight[1:end]/((datos_num$Height[1:end]/100)^2))
colnames(valores_IMC) <- "IMC"
datos <- cbind(datos, valores_IMC) #Se agrega el IMC a los datos

# Se determina el estado nutricional (EN) y se agregan a los datos
datos <- mutate(datos, EN= as.integer(IMC >= 25))

datos_num <- as.data.frame(apply(datos, 2, as.numeric))

# Recordar las ocho posibles variables predictoras seleccionadas de forma aleatoria en el ejercicio anterior.
predictoras <- c("Height",
                 "Ankle.Minimum.Girth",
                 "Forearm.Girth",
                 "Wrists.diameter",
                 "Chest.depth",
                 "Knee.Girth",
                 "Biacromial.diameter",
                 "Biiliac.diameter")



# Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la clase EN,
# justificando bien esta selección.

correlacion <- round(cor(datos_num, datos_num$EN), 2)
print(correlacion)

# Obtenemos que las variables con mayor correlación con EN son:
# EN = 1; IMC = 0.80; Waist.Girth = 0.67; Weight = 0.66; Hip.Girth = 0.65.
# La variable EN se determina a partir del valor de IMC, por lo que omitiremos esta variable ya que sería lo mismo que ajustar un modelo 
# con la misma variable que buscamos predecir.
# 
# De esta manera seleccionamos la variable Waist.Girth, pues es la siguiente con mayor correlación; siendo esta es de tipo directa
# y además se suele usar para estimar la cantidad de grasa corporal, lo que se relaciona con el peso de una persona.

predictor <- "Waist.Girth"


# Usando el entorno R, construir un modelo de regresión logística con el predictor seleccionado en el paso
# anterior.

datos_num$EN <- factor(datos_num$EN)

# Ajustar modelo usando validaciónn cruzada

modelo <- train(EN ~ Waist.Girth, data = datos_num, method = "glm",
                family = binomial(link = "logit"),
                trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
                )

print(summary(modelo))

# Evaluar el modelo
cat("Evaluación del modelo basada en validación cruzada :\n")
matriz <- confusionMatrix(modelo$pred$pred, modelo$pred$obs)
print(matriz)


# Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar, recordadas en el punto 3, para agregar al modelo obtenido en el paso 3

f <- as.formula(paste("EN", paste(c(predictor, predictoras), collapse = "+"), sep = " ~ "))
print(f)

modelo.completo <- train(f, data = datos_num, method = "glm",
                        family = binomial(link = "logit"),
                        trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
                        )
print(summary(modelo.completo))

# Evaluar el modelo completo
cat("Evaluación del modelo completo basada en validación cruzada :\n")
matriz.modelo.completo <- confusionMatrix(modelo.completo$pred$pred, modelo.completo$pred$obs)
print(matriz.modelo.completo)

# Para elegir las variables predictoras se utilizará la función step(), sin embargo 
# esta no soporta los modelos ajustados con Train
# modelo.escalonado <- step(modelo, scope = list(lower = modelo, upper = modelo.completo), direction = "both", trace = 0)

#### Modelos usando gml
set.seed(1111)
# Separar conjuntos de entrenamiento y prueba .
n <- nrow(datos_num)
n_entrenamiento <- floor(0.8 * n)
muestra <- sample.int( n = n, size = n_entrenamiento, replace = FALSE)

entrenamiento <- datos_num[ muestra, ]
prueba <- datos_num[ - muestra, ]

# Ajustar modelo .
simple_model <- glm(EN ~ Waist.Girth, family = binomial(link = "logit"), data = entrenamiento)
print(summary(simple_model))

complete_model <- glm(f, family = binomial(link = "logit"), data = entrenamiento)
print(summary(complete_model))

stepped_model <- step(simple_model, scope = list(lower = simple_model, upper = complete_model), direction = "both", trace = 0)
print(summary(stepped_model))
cat ( " AIC =" , AIC ( stepped_model ) , "\n\n" )

# Obtenemos que el modelo propuesto añade los siguientes 6 predictores:
# 
#  Waist.Girth        0.27307    0.04709   5.799 6.66e-09 ***
#  Height            -0.39042    0.04972  -7.853 4.06e-15 ***
#  Forearm.Girth      0.83796    0.14374   5.830 5.55e-09 ***
#  Chest.depth        0.21687    0.13471   1.610 0.107407    
#  Knee.Girth         0.61150    0.12939   4.726 2.29e-06 ***
#  Biiliac.diameter   0.38541    0.11917   3.234 0.001221 ** 

# Como se nos pide añadir entre dos y cinco predictores se decide tomar las 5 variables, 
# obviando la variable Waist.Grith ya que esta se encuentra incluida desde el punto 2
# donde se nos pide elegir una variable predictora.

# Quedandonos el siguiente modelo

predictoras.final <- c("Height", "Forearm.Girth", "Chest.depth", "Knee.Girth", "Biiliac.diameter")
f.final <- as.formula(paste("EN", paste(c(predictor, predictoras.final), collapse = "+"), sep = " ~ "))
print(f.final)

modelo.final <- train(f.final, data = datos_num, method = "glm",
                     family = binomial(link = "logit"),
                     trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE)
)
print(summary(modelo.final))



# Evaluar el modelo final
cat("Evaluación del modelo final basada en validación cruzada :\n")
matriz.modelo.final <- confusionMatrix(modelo.final$pred$pred, modelo.final$pred$obs)
print(matriz.modelo.final)

####


# Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben
# cumplir.

modelo.final.glm <- glm(f.final, family = binomial(link = "logit"), data = datos_num)

# Independencia de los residuos .
cat("Verificación de independencia de los residuos\n")
cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\ n " )
print(durbinWatsonTest(modelo.final.glm, max.lag = 5))

# Podemos concluir que los residuos son, en efecto, independientes.

# Se evalua si cada predictor se relaciona linealmente con la variable de respuesta.

datos.PR <- select(datos_num, Waist.Girth, Height, Forearm.Girth, Chest.depth, Knee.Girth, Biiliac.diameter, EN)
datos.PR <- apply(datos.PR, 2, as.numeric)
correlacion <- round(cor(datos.PR), 2)
correlacion
corrplot(correlacion, method="number", type="upper")

# Como se puede observar se verifica que existe una relación lineal con los predictores y 
# la variable de respuesta.

# Verificación de multicolinealidad.
cat("Verificación de colinealidad\n")
cat("- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n")
cat("\nVIF:\n")
vifs <- vif(modelo.final.glm)
print(vifs)
cat ("- Tolerancias :\n")
print (1 / vifs )
cat("\nPromedio VIF:")
print(mean(vifs))

# Como umbral de preocupación se toma un valor de 10, como se pudo verificar 
# ninguna variable predictora superó el umbral, sin embargo, puede verse afectada 
# por cierta colinealidad dado que hay valores que son mayores a 5. además el VIF 
# promedio es mayor a 1, por lo que podría haber sesgo en el modelo. Por esta razón
# se considera eliminar la variable Height para mejorar el modelo y obtener uno mas
# confiable.


predictoras.mejorado <- c("Forearm.Girth", "Chest.depth", "Knee.Girth", "Biiliac.diameter")
f.mejorado <- as.formula(paste("EN", paste(c(predictor, predictoras.mejorado), collapse = "+"), sep = " ~ "))
modelo.mejorado <- train(f.mejorado, data = datos_num, method = "glm",
                         family = binomial(link = "logit"),
                         trControl = trainControl(method = "cv", number = 5, savePredictions = TRUE))

# Evaluar el poder predictivo de los modelos en datos no utilizados para construirlo (o utilizando validación 
# cruzada) y revisar las respectivas curvas ROC.

# Separar conjuntos de entrenamiento y prueba .
n <- nrow ( datos )
n_entrenamiento <- floor(0.8 * n )
muestra <- sample.int( n = n , size = n_entrenamiento , replace = FALSE )
entrenamiento <- datos[ muestra, ]
prueba <- datos[- muestra , ]

# Evaluar el modelo
cat("Evaluación del modelo basada en validación cruzada:\n")
matriz <- confusionMatrix(modelo$pred$pred, modelo$pred$obs)
print(matriz)

roc.modelo <- roc(modelo$pred$obs, as.numeric(modelo$pred$pred))
plot(roc.modelo, main = "Modelo Variable única")


# Evaluar el modelo final
cat("Evaluación del modelo final basada en validación cruzada :\n")
matriz.modelo.final <- confusionMatrix(modelo.final$pred$pred, modelo.final$pred$obs)
print(matriz.modelo.final)

roc.modelo.final <- roc(as.numeric(modelo.final$pred$obs), as.numeric(modelo.final$pred$pred))
plot(roc.modelo.final, main = "Modelo con todos los predictores")

# Evaluar el modelo mejorado
cat("Evaluación del modelo mejorado basada en validación cruzada :\n")
matriz.modelo.mejorado <- confusionMatrix(modelo.mejorado$pred$pred, modelo.mejorado$pred$obs)
print(matriz.modelo.mejorado)

roc.modelo.mejorado <- roc(modelo.mejorado$pred$obs, as.numeric(modelo.mejorado$pred$pred))
plot(roc.modelo.mejorado, main = "Modelo Mejorado")

# En relación a los datos y los gráficos que se pueden observar los mejores modelos son
# el modelo con las 5 variables predictoras junto con el modelo mejorado, sin embargo
# como se mencionó anteriormente el modelo con las 5 variables predictoras es posible que presente 
# sesgo por lo que se considera el modelo mejorado como modelo más preciso.


