# EP-11
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

# Importación de datos
# Se le debe ingresar el archivo "Datos-Casen-v2"
datos_casen <- read_xls(file.choose())

# Pregunta 1: Propongan una pregunta de investigación original, que involucre la
# comparación de las medias de dos grupos independientes. Fijando una semilla, 
# seleccionen una muestra aleatoria de hogares (250 < n < 500) y respondan la 
# pregunta propuesta utilizando una simulación Monte Carlo.

# Un estudio de la Universidad de Santiago de Chile desea comprobar si el nivel
# de escolaridad de los habitantes mayores a 30 años de dos regiones es o no
# similar, para ello se escoge la Región Metropolitana y la Región del Maule,
# puesto que esta última se ubica en general en un grupo de desarrollo bajo.

# Hipótesis a Contrastar:
# H0: El promedio del nivel de escolaridad de los habitantes mayores a 30 años es el
#     mismo en la región metropolitana y en la Región del Maule (ua - ub = 0)

# H0: El promedio del nivel de escolaridad de los habitantes mayores a 30 años es 
#     distinto en la región metropolitana y en la Región del Maule (ua - ub != 0)



# Definición de Funciones

# Función para calcular la diferencia de medias.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E_2.
calcular_diferencia <- function(muestra_1, muestra_2, FUN) {
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return (diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés.
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - FUN: función del estadístico E para el que se calcula la diferencia .
# Valor :
# - diferencia E_1 - E _2.
permutar <- function(muestra_1 , muestra_2, FUN) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación.
  permutacion <- sample (c( muestra_1 , muestra_2) , size = n_1 + n_2, replace = FALSE)

  # Asignar elementos a los dos grupos .

  permutacion_1 <- permutacion [1:n_1]
  permutacion_2 <- permutacion [n_1 + 1 : n_2]

  # Calcular y devolver la diferencia de medias .
  return (calcular_diferencia(permutacion_1, permutacion_2 , FUN))
}

# Función para calcular el valor p.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - valor_observado : valor del estadístico de interés para las muestras
# originales .
# - repeticiones : cantidad de permutaciones a realizar .
# - alternative : tipo de hipótesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# Valor :
# - el valor p calculado .
calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
    }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
    }
  else{
    numerador <- sum( distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
    }
  return(valor_p)
  }

# Función para graficar una distribución.
# Argumentos :
# - distribucion : distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot .
graficar_distribucion <- function(distribucion, ...) {
  
  observaciones <- data.frame(distribucion)

  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", ...)
  
  qq <- ggqqplot(observaciones , x = "distribucion", ...)

  # Crear una única figura con todos los gráficos de dispersión.
  figura <- ggarrange(histograma, qq ,ncol = 2 , nrow = 1)
  print(figura)
  }

# Función para hacer la prueba de permutaciones .
# Argumentos :
# - muestra_1 , muestra_2: vectores numéricos con las muestras a comparar .
# - repeticiones : cantidad de permutaciones a realizar .
# - FUN : función del estadístico E para el que se calcula la diferencia .
# - alternative : tipo de hipó tesis alternativa . "two.sided" para
# hipótesis bilateral , "greater" o "less" para hipótesis unilaterales .
# - plot : si es TRUE , construye el gráfico de la distribución generada .
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1 , muestra_2,
                                               repeticiones, FUN ,
                                               alternative, plot , ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa :", alternative , "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2 , FUN)
  cat("Valor observado :", observado , "\n")
  distribucion <- rep(NA, repeticiones)
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  if(plot){
    graficar_distribucion(distribucion, ...)
  }

  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, "two.sided")
  cat("Valor p:", valor_p , "\n\n")
}


set.seed(1412)
n <- 300
region_maule <- datos_casen %>% filter(region == "Región del Maule", edad > 30, esc != "NA")
esc_region_maule <- sample(region_maule$esc, n)
a <- as.numeric(esc_region_maule)

region_metropo <- datos_casen %>% filter(region == "Región Metropolitana de Santiago", edad > 30, esc != "NA")
esc_region_metropo <- sample(region_metropo$esc, n)
b <- as.numeric(esc_region_metropo)

print(shapiro.test(a))
print(shapiro.test(b))
# Al no cumplirse la normalidad, es posible emplear la prueba de permutaciones

R = 5999
contrastar_hipotesis_permutaciones(a, b, repeticiones = R, 
                                   FUN = mean, 
                                   alternative = "two.sided", 
                                   plot = TRUE,
                                   color = "blue", fill = "blue")

# Se muestra además el histograma y gráfico Q-Q de la distribución para la 
# diferencia de medias generada mediante permutaciones.


# Conclusión:
# Con respecto a la prueba realizada y utilizando para ello una simulación de
# Monte Carlo, el resultado del valor p obtenido de 0.0001666667, inferior a un
# nivel de significación de 0.05, por lo que se rechaza la hipótesis nula a favor 
# de la hipótesis alternativa, de esta manera se concluye con 95% de confianza que
# el promedio del nivel de escolaridad de los habitantes mayores a 30 años es 
# distinto en la región metropolitana y en la Región del Maule.

# ------------------------------------------------------------------------------

# Pregunta 2: Propongan una pregunta de investigación original, que involucre la 
# comparación de las medias de más de dos grupos independientes. Fijando una 
# semilla distinta a la anterior, seleccionen una muestra aleatoria de hogares 
# (400 < n < 600) y respondan la pregunta propuesta utilizando bootstrapping. 
# Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping
# aunque este no sea necesario.

if (!require(boot)) {
  install.packages("boot", dependencies = TRUE )
  require (boot)
}
if (!require(simpleboot)) {
  install.packages("simpleboot", dependencies = TRUE )
  require (simpleboot)
}

# Se quiere realizar un estudio, donde se desea saber si promedio de los ingresos (ytot)  
# es igual según el nivel de estudios que tenga la población
# (Nivel Enseñanza Media, Nivel Técnico y Nivel Profesional o Superior)

# Hipótesis a Contrastar:
# H0: El promedio de los ingresos de la población es el mismo para cada uno de los
#     Niveles de educación (Media, Técnico y Profesional)

# H0: El promedio de los ingresos es diferente en al menos uno de los niveles de 
#     educación (Media, Técnico y Profesional).


set.seed(1784)
n <- 500
Ingresos_Media <- select(datos_casen %>% filter(educ == "M. Hum. Completa"), contains("ytot"))
Ingresos_Tecnico <- select(datos_casen %>% filter(educ == "Técnico Nivel Superior Completo"), contains("ytot") )
Ingresos_Profesional <- select(datos_casen %>% filter(educ == "Profesional Completo"), contains("ytot"))
#Se muestrean los ingresos según el nivel de estudios
m_Media <- sample(Ingresos_Media$ytotcorh, n)
m_Tecnico <- sample(Ingresos_Tecnico$ytotcorh, n)
m_Profesional <- sample(Ingresos_Profesional$ytotcorh, n)

# Comprobar normalidad de las muestras .
print (shapiro.test(m_Media))
print (shapiro.test(m_Tecnico))
print (shapiro.test(m_Profesional))

#Como se puede ver el valor p para cada una de las pruebas de normalidad de las 
#Muestras es muy inferior a un nivel de significancia de incluso 0.01
#Por lo que se decide usar Bootstrap para realizar la prueba de hipótesis

educ <- c ( rep("Media", n), rep("Tecnico",n), rep("Profesional", n))
Ingresos <- c(m_Media, m_Tecnico, m_Profesional)
datos <- data.frame(Ingresos, educ)

# Calcular la diferencia observada entre las medias muestrales.
media_Media <- mean(m_Media)
media_Tecnico <- mean(m_Tecnico)
media_Profesional <- mean(m_Profesional)

cat ("diferencia Media - Tecnico:", media_Media - media_Tecnico, "\n\n" )
cat ("diferencia Media - Profesional:", media_Media - media_Profesional, "\n\n" )
cat ("diferencia Tecnico - Profesional:", media_Tecnico - media_Profesional, "\n\n" )

# Establecer el nivel de significación.
alfa <- 0.05

# Crear la distribución Bootstrap.
B <- 9999

# Distribución Bootstrap entre el nivel de educación Media y Técnico.
distribucion_bootstrap_Media_Tecnico <- two.boot(m_Media, m_Tecnico, FUN = mean, R = B)
valores <- data.frame(distribucion_bootstrap_Media_Tecnico$t)
colnames(valores) <- "valores"
graficar_distribucion(valores$valores)
cat("Distribución bootstrap:\n" )
cat("\tMedia:", mean(valores$valores), "\n")
cat("\tDesviación estándar:", sd(valores$valores), "\n\n")
#Construir el intervalo de confianza.
intervalo_M_T <- boot.ci(distribucion_bootstrap_Media_Tecnico, conf = 1-alfa,
                                  type = "bca")
print ( intervalo_M_T )


# Distribución Bootstrap entre el nivel de educación Media y Profesional.
distribucion_bootstrap_Media_Profesional <- two.boot(m_Media, m_Profesional, FUN = mean, R = B)
valores <- data.frame(distribucion_bootstrap_Media_Profesional$t)
colnames(valores) <- "valores"
graficar_distribucion(valores$valores)
cat("Distribución bootstrap:\n" )
cat("\tMedia:", mean(valores$valores), "\n")
cat("\tDesviación estándar:", sd(valores$valores), "\n\n")
#Construir el intervalo de confianza.
intervalo_M_P <- boot.ci(distribucion_bootstrap_Media_Profesional, conf = 1-alfa,
                         type = "bca")
print (intervalo_M_P)

# Distribución Bootstrap entre el nivel de educación Técnico y Profesional.
distribucion_bootstrap_Tecnico_Profesional <- two.boot(m_Tecnico, m_Profesional, FUN = mean, R = B)
valores <- data.frame(distribucion_bootstrap_Tecnico_Profesional$t)
colnames(valores) <- "valores"
graficar_distribucion(valores$valores)
cat("Distribución bootstrap:\n" )
cat("\tMedia:", mean(valores$valores), "\n")
cat("\tDesviación estándar:", sd(valores$valores), "\n\n")
#Construir el intervalo de confianza.
intervalo_T_P <- boot.ci(distribucion_bootstrap_Tecnico_Profesional, conf = 1-alfa,
                         type = "bca")
print(intervalo_T_P)


#Intervalos de confianza para cada una de las distribuciones Bootstrap
print("Intervalos de confianza:")
print(intervalo_M_T)
print(intervalo_M_P)
print(intervalo_T_P)

# Conclusión:
# Como se vio en las diferencias de las medias muestrales observadas que hay 
# diferencias notables entre las medias de los ingresos según su nivel de educación, 
# sin embargo las muestras no tienen una distribución normal por lo que se decidio utilizar
# Bootstrap obteniendo como resultado los intervalos de confianza de las diferencias 
# de las medias según cada nivel de educación. A partir de estos resultados es posible 
# concluir que: en promedio la población con un nivel de educación técnico tiene 
# mayores ingresos que la población con un nivel de educación media con una diferencia 
# entre 212419 y 463055. también se puede decir que en promedio la población con un nivel 
# de educación profesional tiene mayores ingresos que la población con un nivel de 
# educación media con una diferencia entre 837759 y 1153109. Y por último se concluye que 
# en promedio la población con un nivel de educación profesional tiene mayores ingresos que 
# la población con un nivel de educación técnica con una diferencia entre 502681 y 814756.
# Por lo que la hipótesis nula es rechazada concluyendo así que el promedio de los ingresos
# es diferente según el nivel de educación que se tenga.

# Análisis post - hoc .
# Función para calcular la media de las diferencias para dos columnas de una
# matriz de datos en formato ancho .
media_diferencias <- function(datos , columna_1 , columna_2) {
  media <- mean(datos[[columna_1]] - datos[[ columna_2]])
  return ( media )
}

# Función para generar la distribuciones de la diferencia de medias a
# partir de las permutaciones .
distribucion_diferencias <- function ( permutaciones , columna_1, columna_2, n) {
  R <- n
  distribucion <- c ()
  
  for ( i in 1: R ) {
    datos <- permutaciones[i,]
    diferencia <- media_diferencias(datos, columna_1, columna_2)
    distribucion <- c(distribucion, diferencia )
  }
  
  return ( distribucion )
}

if (!require(tidyverse ) ) {
  install.packages("tidyverse ", dependencies = TRUE )
  require (tidyverse )
}

Instancia <- factor (1:n)
datos_anchos <- data.frame(Instancia, Media = m_Media, Tecnico = m_Tecnico, Profesional = m_Profesional)

datos_largos <- datos_anchos %>% pivot_longer(c("Media", "Tecnico" ,
                                                              "Profesional"),
                                                          names_to = "Educación" ,
                                                          values_to = "Ingresos" )

datos_largos[["Educación"]] <- factor(datos_largos[["Educación" ]])
n<- nrow(datos_anchos)
dif_Media_Tecnico <- distribucion_diferencias(datos_anchos, "Media", "Tecnico", n)
dif_Media_Profesional <- distribucion_diferencias(datos_anchos, "Media" , "Profesional", n)
dif_Tecnico_Profesional <- distribucion_diferencias(datos_anchos, "Tecnico", "Profesional", n)

# Obtener valores p.
