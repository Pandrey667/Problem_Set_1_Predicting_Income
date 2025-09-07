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

### Filtramos, con la librería Tidyverse, data_final
### Personas estrictamente mayores de 18 años
### Personas ocupadas (ocu =1)

df_1 <- data_final %>% 
  filter(age >= 18 & ocu %in% 1)


### Obtención de la variable salario por hora para cada individuo
df_2 <- df_1 %>% 
  ## Filtramos para obtener solo individuos tienen horas trabajadas
  filter(totalHoursWorked != 0) %>% 
  mutate(
    ## Creamos la variable ingreso_total que está compuesta por la sumatoria
    ## de impaes + isaes. Es decir: ingresos del trabajo principal + ingresos de un posible trabajo adicional
    ingreso_total = ifelse(!is.na(impa), impa, impaes) +
      ifelse(!is.na(isa),  isa,  isaes),
    ## Obtenemos el total de horas trabajadas al mes (multiplicamos totalHoursWorked x 4)
    horas_mes_Worked = totalHoursWorked * 4,
    ## El salario por hora (salario_hora) es la división del ingreso total sobre las horas trabajas al mes (obtenidas con anterioridad) 
    salario_hora = ingreso_total / horas_mes_Worked
  )

#################### Gráficos de las variables empleadas en las regresiones #############################################


### Filtramos los NA y valores iguales a cero en 
df_2 <- df_2 %>% 
  filter(!is.na(salario_hora)) %>% 
  filter(salario_hora != 0) %>% 
  filter(!is.na(maxEducLevel))

### Creamos la variable del logaritmo natural del salario por hora
df_2$ln_wage <- log(df_2$salario_hora)

### Convertimos df_2 a un objeto data frame
df_2 <- as.data.frame(df_2)


### Construímos el data frame variables_num con las variables numéricas
variables_num <- df_2 %>% 
  select(salario_hora, 
         ln_wage, age, 
         totalHoursWorked)
