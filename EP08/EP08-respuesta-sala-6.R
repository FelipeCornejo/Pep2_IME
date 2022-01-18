# EP-08
# Inf. y Modelos Estadísticos
# Arturo Cadenas (20.468.370-0)
# Claudio Muñoz (20.003.395-7)
# Bryan Salas (19.316.410-2)
# Miguel Salinas ()


library (tidyverse)
library (ggpubr)
library (ez)
library (MASS)
library (car)

# Pregunta 1

#Los desórdenes alimenticios son un problema de salud mental bastante frecuente 
# que afectan, solo en Estados Unidos, a millones de niños y adolescentes. El 
# paquete MASS de R incluye el conjunto de datos anorexia que reporta el peso 
# inicial, el peso final y el tratamiento recibido por 72 mujeres con diagnóstico
# de anorexia. Determine si existen diferencias significativas en el peso inicial 
# de las mujeres para cada tratamiento.

# Importación de datos
datos <- MASS::anorexia
datos <- datos[, c("Treat", "Prewt")] 
datos[["Treat"]] <- factor(datos[["Treat"]])
datos[["instancia"]] <- factor(1:nrow(datos))


# Se Establece un nivel de significación
alfa <- 0.025

# Formulación de Hipótesis.
# H0: No existen diferencias significativas en el peso inicial de las mujeres para 
# cada tratamiento.
# HA: Existen diferencias significativas en el peso inicial de las mujeres para 
# cada tratamiento.


# Comprobación de normalidad utilizando gráfico Q-Q.
g <- ggqqplot(datos,
              x = "Prewt",
              y = "Treat",
              color = "Treat")

g <- g + facet_wrap(~Treat)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)


# Se verifican las condiciones para utilizar ANOVA:

# - Se puede suponer que las muestras son obtenidas de manera aleatoria e independiente.
# - La escala con la que se mide "Prewt" (peso en [Kg] antes del tratamiento), tiene 
#   las propiedades de una escala de intervalos iguales.
# - Se puede suponer razonablemente que la población de origen sigue una distribución
#   normal, la cual se puede observar por medio del gráfico Q-Q, se debe tener en cuenta
#   que existen algunos valores que pueden ser atípicos, además se tiene que las muestras 
#   son relativamente pequeñas, por lo que se utiliza un nivel de significación 
#   más exigente igual a alfa = 0,025.
# - Las muestras tienen varianzas aproximadamente iguales, se comprueba al proceder
#   con la función leveneTest() la cual realiza la prueba de homocedasticidad.
#   Además, al realizar el procedimiento de ANOVA con ezANOVA(), esta incluye
#   dicha prueba.



# Prueba de homocedasticidad de Levene.
leveneTest(y = datos$Prewt, group = datos$Treat, center = "median")

# H0: Las varianzas de las muestras son iguales.
# HA: Al menos una de las muestras tiene varianza diferente a alguna de las demás.

# Al ser realizada la prueba de Levene para la homogeneidad de Varianzas, se obtiene
# un p= 0.3969, mucho mayor al nivel de significación por lo que se puede concluir
# con un 97,5% de confianza que las varianzas de las muestras son iguales.



# Procedimiento ANOVA con aov().
cat("Procedimiento ANOVA usando aov\n\n")
prueba <- aov(Prewt ~ Treat, data = datos)
print(summary(prueba))


# Procedimiento ANOVA con ezANOVA ().
cat("\n\ nProcedimiento ANOVA usando ezANOVA \n\n")
prueba2 <- ezANOVA(data = datos,
                   dv = Prewt,
                   between = Treat,
                   wid = instancia,
                   return_aov = TRUE)

print(prueba2)

# Gráfico del tamaño del efecto .
g2 <- ezPlot(data = datos,
             dv = Prewt,
             wid = instancia ,
             between = Treat ,
             y_lab = "Prewt promedio",
             x = Treat)

print(g2)

# Conclusión de prueba ANOVA:
# Con respecto al resultado obtenido y al obtener un p = 0.552, muy superior al
# nivel de significación, se falla al rechazar la hipótesis nula, por lo tanto,
# se concluye con un 97,5% de confianza que no existen diferencias significativas 
# en el peso inicial de las mujeres para cada tratamiento.


# Se aplica un análisis post hoc  sin importar el resultado de la prueba ómnibus.
# Prueba HSD de Tukey.
# Se utiliza esta prueba para buscar las diferencias significativas entre los
# diferentes pares de medias.
post_hoc <- TukeyHSD(prueba,
                     "Treat",
                     ordered = TRUE,
                     conf.level = 1 - alfa)

print(post_hoc)

# Conclusión de prueba post hoc.
# Con respecto a los resultados de la columna p_adj, se puede observar que para
# cada uno de los valores p asociados a las diferencias de tratamientos, ninguno 
# de estos tiene un p menor al nivel de significación (alfa = 0.025), por lo que 
# ningún tratamiento tiene una diferencia significativa, lo cual afirma además 
# la conclusión entregada al ser realizada la prueba ANOVA.

