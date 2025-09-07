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