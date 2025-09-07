### Limpiamos el ambiente de RStudio
remove(list = ls())

### Librerías usadas en el trabajo
library(rvest)         ## Web-scrapping con R
library(tidyverse)     ## Manipulación de datos
library(purrr)         ## Loop para unificar el web-scrapping
library(moments)       ## Cálculo del coeficiente de asimetría y la Kurtosis
library(modelsummary)  ## Tablas de salida en formato LaTeX
library(ggplot2)       ## Gramática de gráficos
library(gridExtra)     ## Matriz de gráficas en ggplot2
library(viridis)       ## Paleta de colores para gráficas

### URL de la página de GitHub del doctor Ignacio Sarmiento
url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/"

### Generamos las URLs de las páginas (del 1 al 10 por ejemplo)
paginas <- paste0(url_base, "geih_page_", 1:10, ".html")

### Función para extraer tablas en formato HTML de las páginas web 
extraer_tabla <- function(url_tabla){
  ## Leemos la página en formato HTML
  pagina <- read_html(url_tabla)
  ## Ubicamos los nodos que contienen las tablas HTML
  nodo_tabla <- pagina %>% 
    html_node("table")
  ## Extraémos las tablas ubicadas en la página escrapeada
  tabla <- nodo_tabla %>% 
    html_table(fill = TRUE)
  
  ## Renombramos las columnas sin título con rl caracter col_1
  names(tabla) <- if_else(
    is.na(names(tabla)) | names(tabla) == "",
    paste0("col_", seq_along(tabla)),
    names(tabla)
  )
  
  ## Convertimos la tabla con la entrada de la función (url_tabla)
  tabla %>% mutate(origen = url_tabla)
}

### Aplicamos el loop map, del paquete purrr, a las 10 páginas web con las tablas  
data_final <- map_dfr(paginas, extraer_tabla)

### Estudiamos la clase del objeto data_final
class(data_final)

### Estudiamos las columnas del data frame 
glimpse(data_final)