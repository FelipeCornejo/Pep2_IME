# EP-09
# Inf. y Modelos Estadísticos
# Arturo Cadenas (20.468.370-0)
# Claudio Muñoz (20.003.395-7)
# Bryan Salas (19.316.410-2)
# Miguel Salinas (20.215.515-4)

# Librerías
library ( tidyverse )
library ( ggpubr )
library ( nlme )
library ( emmeans )
library ( ez )

# Pregunta 1
# El siguiente código R carga los datos que aparecen en una tabla que compara las 
# mejores soluciones encontradas por cuatro algoritmos para instancias del problema 
# del vendedor viajero con solución óptima conocida, tomados desde una memoria de 
# título del DIINF. Con estos datos responda la pregunta de investigación: 
# ¿Hay algoritmos mejores que otros?

texto <- ("
Instancia Optimo R R2 R3 G
'brock400_2' 26 15.6 16.3 17.2 19
'brock400_4' 30 13.8 16.3 17.4 19
'C2000.9' 77 54.2 56.6 59.4 63
'c-fat500-10' 123 122 122 122 123
'hamming10-2' 509 340.2 416.1 419.4 509
'johnson32-2-4' 13 13 13 13 13
'keller6' 56 31.5 40 42.5 45.2
'MANN_a81' 1097 1079.2 1079.2 1079.2 1092.9
'p-hat1500-1' 9 3.9 5.1 5.9 7
'p-hat1500-3' 91 72.8 74.7 81.6 83
'san1000' 12 4.6 4.6 4.7 7
'san400_0.7_1' 37 16.6 17.5 17.5 18
'san400_0.9_1' 97 41.1 51.4 53.4 89
'frb100-40' 97 63.4 73.7 77.5 79
'frb59-26-1' 56 36.2 42.9 45.3 45
'1et.2048' 313 229.4 265.4 277.9 289.4
'1zc.4096' 376 270.8 290.2 304.4 325.5
'2dc.2048' 21 12.6 15.7 16.9 18
")

# Importación de datos
datos <- read.table(textConnection(texto), header = TRUE)

# Llevar dataframe a formato largo .
datos <- datos %>% pivot_longer(c("Optimo", "R", "R2", "R3", "G"),
                                names_to = "algoritmo", 
                                values_to = "distancia")

datos[["algoritmo"]] <- factor(datos[["algoritmo"]])



# Formulación de Hipótesis.
# H0: No existen diferencias significativas en la solución de los algoritmos, por lo que no hay
#     algoritmos mejores que otros.
# HA: Existen diferencias significativas en la solución los algoritmos, por lo que hay
#     algoritmos mejores que otros.

# Se verifican las condiciones para utilizar ANOVA:
# - Se puede suponer que las muestras son obtenidas de manera aleatoria e independiente.
# - La escala con la que se mide la distancia tiene las propiedades de una escala de intervalos iguales.
# - Se utiliza la función de R ezANOVA() que incluye una prueba para verificar la condición de: 
#   la prueba de esfericidad de Mauchly.
# - Se puede suponer razonablemente que la población de origen sigue una distribución
#   normal, la cual se puede observar por medio del gráfico Q-Q, se debe tener en cuenta
#   que existen valores que pueden ser atípicos, además se tiene que las muestras 
#   son relativamente pequeñas, por lo que se utiliza un nivel de significación 
#   bastante más exigente, igual a alfa = 0,01.
# Nivel de significación.
alfa <- 0.01

# Comprobación de normalidad .
g <- ggqqplot(datos , x = "distancia", y = "algoritmo", color = "algoritmo")
g <- g + facet_wrap(~ algoritmo )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Procedimiento ANOVA con ezANOVA ().
cat ("\n\nProcedimiento ANOVA usando ezANOVA\n\n")

prueba2 <- ezANOVA(data = datos , dv = distancia , within = algoritmo ,
                   wid = Instancia , return_aov = TRUE )

print ( summary ( prueba2$aov))

cat("Resultado de la prueba de esfericidad de Mauchly :\n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])


# El valor p obtenido en esta prueba es muy bajo (p = 1.060585e-22), de lo que 
# se desprende que los datos del ejemplo no cumplen con la condición de esfericidad
# por lo que se debe considerar la corrección de GGreenhouse-Geisser (e < 0.75) y no el valor
# sin corregir entregado por la prueba anterior.
cat("\n\nY factores de corrección para cuando no se cumple la\n")
cat("condición de esfericidad:\n\n")
print(prueba2$'Sphericity Corrections')


# Conclusión de prueba ANOVA:
# Con respecto al resultado obtenido y al obtener un p = 0.004817602, inferior al
# nivel de significación, se rechaza la hipótesis nula a favor de la alternativa, por lo tanto,
# se concluye con un 99% de confianza que existen diferencias significativas en los algoritmos, 
# por lo que hay algoritmos mejores que otros.      


# Gráfico del tamaño del efecto .
g2 <- ezPlot(data = datos , dv = distancia , wid = Instancia , within = algoritmo ,
             y_lab = "Distancia promedio", x = algoritmo )

print(g2)


# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(distancia ~ algoritmo , data = datos , random = ~1|Instancia )
medias <- emmeans(mixto , "algoritmo")
tukey <- pairs ( medias , adjust = "tukey")

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Conclusión de prueba post-hoc.
# Se utiliza esta prueba para buscar las diferencias significativas entre los
# diferentes algoritmos.
# Se concluye con un 99% de confianza que los pares de algoritmos 
# G - R
# Optimo - R
# Optimo - R2
# Optimo - R3
# Tienen diferencias significativas en la solución del problema del vendedor viajero.

# Pregunta 2
# El siguiente es (un resumen de) la descripción de un famoso experimento:
#  Naming the ink color of color words can be difficult. For example, if asked to 
# name the color of the word "blue" is difficult because the answer (red) conflicts 
# with the word "blue." This interference is called "Stroop Interference" after the 
# researcher who first discovered the phenomenon. This case study is a classroom 
# demonstration. Students in an introductory statistics class were each given three 
# tasks. In the "words" task, students read the names of 60 color words written in 
# black ink; in the "color" task, students named the colors of 60 rectangles; 
# in the "interference" task, students named the ink color of 60 conflicting color
# words. The times to read the stimuli were recorded.
# El siguiente código R carga los datos que se obtuvieron en este estudio. Con estos 
# datos, responda la siguiente pregunta de investigación: ¿hay diferencias en los 
# tiempos entre tareas?

texto <- ("
words colors interfer
9 14 42
13 22 47
21 19 44
13 24 29
23 17 37
17 21 31
15 19 38
26 24 33
18 19 44
21 20 38
16 15 32
21 17 35
18 19 31
17 23 34
16 16 36
19 15 31
")

# Importación de datos
datos2 <- read.table(textConnection(texto), header = TRUE)
datos2[["instancia"]] <- factor(1:nrow(datos2))

# Llevar dataframe a formato largo .
datos2 <- datos2 %>% pivot_longer(c("words", "colors", "interfer"),
                                names_to = "tareas", 
                                values_to = "tiempo")

datos2[["tareas"]] <- factor(datos2[["tareas"]])


# Formulación de Hipótesis.
# H0: No existen diferencias significativas en los tiempos entre tareas
# HA: Existen diferencias significativas en los tiempos entre tareas

# Se verifican las condiciones para utilizar ANOVA:
# - Se puede suponer que las muestras son obtenidas de manera aleatoria e independiente.
# - La escala con la que se mide el tiempo, tiene las propiedades de una escala de intervalos iguales.
# - Se utiliza la función de R ezANOVA() que incluye una prueba para verificar la condición de: 
#   la prueba de esfericidad de Mauchly.
# - Se puede suponer razonablemente que la población de origen sigue una distribución
#   normal, la cual se puede observar por medio del gráfico Q-Q.
# Nivel de significación.
alfa <- 0.05


# Comprobación de normalidad .
g <- ggqqplot(datos2 , x = "tiempo", y = "tareas", color = "tareas")
g <- g + facet_wrap(~ tareas )
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)


prueba2 <- ezANOVA(data = datos2 , dv = tiempo , within = tareas,
                   wid = instancia , return_aov = TRUE )

print ( summary ( prueba2$aov))

cat("Resultado de la prueba de esfericidad de Mauchly :\n\n")
print(prueba2[["Mauchly's Test for Sphericity"]])

# El valor p obtenido en esta prueba es mayor al nivel de significación (p = 0.1850452), de lo que 
# se desprende que los datos del ejemplo cumplen con la condición de esfericidad
# por lo que se debe considerar el valor, sin corregir, de la tabla entregada por ezANOVA().


# Conclusión de prueba ANOVA:
# Con respecto al resultado obtenido y al tener un p = 4.07e-13, inferior al
# nivel de significación, se rechaza la hipótesis nula a favor de la alternativa, por lo tanto,
# se concluye con un 95% de confianza que existen diferencias significativas en los tiempos de las tareas

g2 <- ezPlot(data = datos2 , dv = tiempo , wid = instancia , within = tareas ,
             y_lab = "tiempo promedio", x = tareas )

print(g2)


# Procedimiento post-hoc HSD de Tukey.
mixto <- lme(tiempo ~ tareas , data = datos2 , random = ~1|instancia )
medias <- emmeans(mixto , "tareas")
tukey <- pairs ( medias , adjust = "tukey")

cat("\n\nPrueba HSD de Tukey\n\n")
print(tukey)

# Conclusión de prueba post-hoc.
# Se utiliza esta prueba para buscar las diferencias significativas entre el
# tiempo de las diferentes tareas
# Se concluye con un 95% de confianza que los pares de tareas 
# colors - interfer
# interfer - words
# Tienen diferencias significativas en el tiempo.