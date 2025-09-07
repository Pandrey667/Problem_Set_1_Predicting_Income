### Recategorizamos la variable de estratos socieconómicos
df_2$estrato1 <- factor(df_2$estrato1,
                        levels = 1:6,
                        labels = c("Estrato 1","Estrato 2","Estrato 3",
                                   "Estrato 4","Estrato 5","Estrato 6"))


### Función para calcular la moda
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

### Calculamos N, Moda, frecuencia y proporción de la moda 
cat_summary <- df_2 %>%
  select(sex, formal, maxEducLevel, estrato1) %>%
  summarise(across(
    everything(),
    list(
      ## Tamaño muestral de las variables
      N = ~sum(!is.na(.x)),
      ## Número de categorías por cada variable
      Categories = ~length(unique(na.omit(.x))),
      ## Moda de cada variable estudiada
      Moda = ~get_mode(.x),
      ## Frecuencia de la moda
      FreqModa = ~max(table(.x, useNA = "no")),
      ## Porporción de la moda
      PropModa = ~max(prop.table(table(.x, useNA = "no")))
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )


### Exportamos la tabla de las variables categóricas en formato LaTeX
datasummary_df(cat_summary,
               output = "latex",
               title = "Estadísticas descriptivas de variables categóricas")


########################## Gráficas de las variables ########################################################################################################

#### Histograma del salario por hora
test_1 <- df_2 %>% 
  ggplot(., aes(x = salario_hora)) + 
  geom_histogram(bins = 15, 
                 fill = "darkgoldenrod1", 
                 color = "black") + 
  ylab('Frecuencia') + 
  xlab('Salario por hora (Pesos colombianos)') + 
  ggtitle('Salarios por hora') + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 17), 
        plot.caption = element_text(hjust = 0))
#labs(caption = "Fuente: Gran Encuesta Integrada de Hogares (GEIH) 2018. Gráfica de elaboración propia.")


#### Histograma del logaritmo natural del salario por hora
test_2 <- df_2 %>%
  ggplot(., aes(x = ln_wage)) + 
  geom_histogram(bins = 15, 
                 fill = "darkgoldenrod1", 
                 color = "black") + 
  ylab('Frecuencia') + 
  xlab('Escala logarítmica') + 
  ggtitle('Logaritmo natural del Salario por hora') + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  size = 17), 
        plot.caption = element_text(hjust = 0))
#labs(caption = "Fuente: Gran Encuesta Integrada de Hogares (GEIH) 2018")

### Matriz de gráficos del salario por hora
salarios_graph <- grid.arrange(test_1, test_2, ncol = 2)

### Exportamos la matriz de gráficos del salario
ggsave('salarios.jpeg', plot = salarios_graph, 
       width = 10, height = 6, dpi = 300)



