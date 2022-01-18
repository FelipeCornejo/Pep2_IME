# EP-12
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

# Se cargan de datos.
datos <- read.csv2(file.choose(),header=TRUE, sep = "")

# Un estudio recolectó medidas anatómicas de 247 hombres y 260 mujeres (Heinz et al., 2003). Estas mediciones
# están disponibles en el archivo Body-EP12.csv que acompaña a este enunciado. El estudio incluyó nueve
# mediciones del esqueleto (ocho diámetros y una profundidad de hueso a hueso) y doce mediciones de grosor
#(circunferencias) que incluyen el tejido.

# Se pide construir un modelo de regresión lineal múltiple para predecir la variable Peso.
set.seed(8370)

# Como la semilla se trata de un numero par se filtra la tabla dejando solo mujeres
tabla <- filter(datos, Gender == 0)

#Se hace una copia de la tabla y se elimina la variable peso
aux <- tabla
aux$Weight <-NULL
#Se obtienen los nombres de las varaibles
variables<-colnames(aux)
#Se seleccionan al azar 8 varaibles predictoras
predictoras<-sample(variables, size = 8)
print(predictoras)


# Se pasan los datos a tipo numérico
datos_num <- as.data.frame(apply(tabla, 2, as.numeric))

# Predictor Elegido: Waist.Girth -> Grosor a la altura de la cintura en cm
# Pues tiene una correlación alta, esta es de tipo directa, y además se suele 
# usar para estimar la cantidad de grasa corporal lo que se relaciona con el peso de una persona.
cor(datos_num$Waist.Girth, datos_num$Weight)

# Muestra de 50 mujeres
datos_num <- datos_num[sample(nrow(datos_num), 50),]

# Ajustar modelo con R.
modelo <- lm(Weight ~ Waist.Girth, data = datos_num)
print(summary(modelo))

# Graficar el modelo.
p <- ggscatter(datos_num, x = "Waist.Girth", y = "Weight", color = "blue", fill = "blue",
               xlab = "Grosor a la altura de la cintura [cm]", ylab = "Peso [Kg]")

p <- p + geom_smooth(method = lm, se = FALSE , colour = "red")
print(p)

# Crear gráficos para evaluar el modelo .
plot(modelo)



# 6. Usando herramientas para la exploración de modelos del entorno R, buscar entre dos y cinco predictores de
# entre las variables seleccionadas al azar en el punto 3, para agregar al modelo de regresión lineal simple
# obtenido en el paso 5.


f <- as.formula(paste("Weight", paste(c("Waist.Girth", predictoras), collapse = "+"), sep = " ~ "))

print(f)

# Modelo con todas las variables predictoras del punto 3
models.completo <- lm(f, data = datos_num)

# Seleccion de predictores hacia adelante (forward)
# Se toma como modelo de la izquierda al modelo simple del punto 5 (modelo) y como el modelo de la derecha, al models.completo
# el cual incluye todas las variables predictoras del punto 3 junto con "Waist.Girth" usado en el modelo simple del punto 5
models.forward = step(modelo, scope = list(lower = modelo, upper = models.completo), direction = "forward", trace = 1)

# Modelo propuesto por el metodo Forward
summary(models.forward)


# Seleccion de predictores hacia atrás (backward)
models.backward = step(models.completo, scope = list(lower = modelo, upper = models.completo), direction = "backward", trace = 1)

# Modelo propuesto por el metodo backward
summary(models.backward)

# Observamos, que para ambos metodos, el modelo propuesto es el mismo, el cual incluye: "Waist.Girth", "Knee.Girth", "Height" y "Forearm.Girth"


# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben cumplir.

# Condiciones RLM

# 1. Las variables predictoras deben ser cuantitativas o dicotómicas (de ahí la necesidad de variables indicadoras para manejar más de dos niveles).
#    R: Las variables predictoras son todas numéricas a nivel de intervalo y ninguna de ellas corresponde a una constante.

# 2. La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#    R: Las variables peso ("Weight") es cuantitativa y continua.

# 3. Los predictores deben tener algún grado de variabilidad (su varianza no debe ser igual a cero). En otras palabras, no pueden ser constantes.
#    R: Observando los resultados, ninguna varianza es igual a 0.

varianzas.WG <- var(datos_num$Waist.Girth)
varianzas.KG <- var(datos_num$Knee.Girth) 
varianzas.Ht <- var(datos_num$Height)
varianzas.FG <- var(datos_num$Forearm.Girth)

print(varianzas.WG) 
print(varianzas.KG)
print(varianzas.Ht)
print(varianzas.FG)

# 4. No debe existir multicolinealidad. Esto significa que no deben existir relaciones lineales fuertes entre dos o más predictores (coeficientes de correlación altos).
#    R: Observando los resultados, podemos ver que se cumple la condicion

vifs <- vif(models.forward)
cat("\nVerificar la multicolinealidad :\n")
cat("- VIFs :\n")
print( vifs )

cat("- Tolerancias :\n")
print(1/vifs)

cat("- VIF medio :", mean (vifs), "\n")

# 5. Los residuos deben ser homocedásticos (con varianzas similares) para cada nivel de los predictores.
#    R: Obtenemos como resultado de la prueba: Chisquare = 3.25478, Df = 1, p = 0.071215, por lo que el supuesto de homocedasticidad se cumple.

prueba.Ncv <- ncvTest(models.forward)
print(prueba.Ncv)

# 6. Los residuos deben seguir una distribución cercana a la normal centrada en cero.
#    R: Obtenemos como resultado de la prueba: p-value = 0.7716, por lo que podemos asumir que el supuesto se cumple.

prueba.shapiro = shapiro.test(models.forward$residuals)
print(prueba.shapiro)

# 7. Los valores de la variable de respuesta son independientes entre sí.
#    R: Obtenemos como resultado de la prueba: p = 0.986, por lo que podemos concluir que los residuos son, en efecto, independientes.

prueba.durbin <- durbinWatsonTest(models.forward)
print(prueba.durbin)


# 8. Cada predictor se relaciona linealmente con la variable de respuesta.
#    R: Como observamos en los resultados, el supuesto se cumple

datos.PR <- select(datos_num, Waist.Girth, Knee.Girth, Height, Forearm.Girth, Weight)
correlacion <- round(cor(datos.PR), 2)
correlacion
corrplot(correlacion, method="number", type="upper")

################################################################################


# 8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación cruzada).
set.seed(8370)
# Crear conjuntos de entrenamiento y prueba .
n <- nrow(datos_num)
n_entrenamiento <- floor(0.7 * n)
muestra <- sample.int(n = n, size = n_entrenamiento, replace = FALSE)
entrenamiento <- datos_num[muestra, ]
prueba <- datos_num[-muestra, ]

#variables[, !(colnames(variables) %in% predictoras), drop = FALSE]

# Ajustar modelo con el conjunto de entrenamiento.
modelo <- lm(Weight ~ Waist.Girth, data = entrenamiento)
print(summary(modelo))

# Calcular error cuadrado promedio para el conjunto de entrenamiento.
mse_entrenamiento <- mean (modelo$residuals ** 2)
cat("MSE para el conjunto de entrenamiento: ", mse_entrenamiento, "\n")

# Hacer predicciones para el conjunto de prueba.
predicciones <- predict(modelo, prueba)
# Calcular error cuadrado promedio para el conjunto de prueba.
error <- sapply(prueba[["Weight"]],as.double) - predicciones
mse_prueba <- mean(error ** 2)
cat("MSE para el conjunto de prueba: ", mse_prueba)

# Hay una diferencia considerable entre los errores cuadráticos medios, por lo que 
# se considera que con la semilla utilizada es imprudente decir que el modelo sea generalizable.
# Sin embargo, esto puede deberse a la separación aleatoria de los datos, por lo que usando otra semilla
# podría ser posible obtener un modelo que pueda ser generalizable.









