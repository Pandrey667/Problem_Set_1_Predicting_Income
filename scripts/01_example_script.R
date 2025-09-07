# ============================================
# 5. Predicting wages
# (a) Train-Test Split
# ============================================

#install.packages("caret")
library(caret)

set.seed(10101)  # Semilla para reproducibilidad

# Crear partición: 70% entrenamiento, 30% prueba
inTrain <- createDataPartition(
  y = db$totalHoursWorked,  # se usa outcome para balance
  p = 0.70,
  list = FALSE
)

training <- db[inTrain, ]
testing  <- db[-inTrain, ]

# Aseguramos que no haya NA/NaN/Inf en la variable dependiente
training <- training |> filter(is.finite(ln_wage))
testing  <- testing  |> filter(is.finite(ln_wage))


# -------------------------
# Verificación de la partición
# -------------------------
split_data <- data.frame(
  Split = factor(c("Training", "Testing")),
  Count = c(nrow(training), nrow(testing)),
  Percentage = c(nrow(training)/nrow(db)*100,
                 nrow(testing)/nrow(db)*100)
)

# -------- Gráfico 1: Distribución Train-Test --------
# =====================================================
# Tema global estilo limpio y claro
# =====================================================
library(ggplot2)
library(patchwork)

mi_tema <- theme_bw(base_size = 14, base_family = "sans") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 12, color = "black"),
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)
  )

theme_set(mi_tema)

# =====================================================
# 1. Distribución Train-Test
# =====================================================
p1 <- ggplot(split_data, aes(x = Split, y = Count, fill = Split)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")),
            vjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
  labs(
    title = "Distribución Train-Test (70%-30%)",
    y = "Número de observaciones",
    x = NULL
  ) +
  scale_fill_manual(values = c("Training" = "#1f77b4", "Testing" = "#ff7f0e")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.grid.major.x = element_blank())

# =====================================================
# 2. Proceso de partición
# =====================================================
partition_process <- data.frame(
  Conjunto = factor(c("Base completa", "Training", "Testing"),
                    levels = c("Base completa", "Training", "Testing")),
  Observaciones = c(nrow(db), nrow(training), nrow(testing))
)

p2 <- ggplot(partition_process, aes(x = Conjunto, y = Observaciones, fill = Conjunto)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = paste0("n = ", Observaciones)),
            vjust = -0.5, size = 4.5, fontface = "bold", color = "black") +
  labs(
    title = "Proceso de partición de datos",
    x = NULL,
    y = "Número de observaciones"
  ) +
  scale_fill_manual(values = c("Base completa" = "#6baed6",
                               "Training" = "#1f77b4",
                               "Testing" = "#ff7f0e")) +
  theme(panel.grid.major.x = element_blank())

# -------- Combinar en una sola figura (2 columnas, 1 fila) --------
# install.packages("gridExtra")
library(gridExtra)

grid.arrange(p2, p1, ncol = 2)  # 2 columnas, 1 fila
# grid.arrange(p1, p2, nrow = 2)  # 2 filas, 1 columna

# (b)
# install.packages("Metrics")
library(Metrics)   # para RMSE
library(caret)

# Asegurar mismos niveles en factores categóricos
categoricas <- c("female", "formal", "nivel_educativo", "relab", "sizeFirm")

for (var in categoricas) {
  testing[[var]] <- factor(testing[[var]], levels = levels(training[[var]]))
}


# Modelo 1: edad–salario simple
modelo_edadsal <- lm(ln_wage ~ age + agesqr, data = training)
pred1 <- predict(modelo_edadsal, newdata = testing)
rmse1 <- RMSE(pred = pred1, obs = testing$ln_wage)

# Modelo 2: edad–salario con controles
modelo_edadsal2 <- lm(
  ln_wage ~ age + agesqr + totalHoursWorked + female + formal + nivel_educativo + relab + sizeFirm,
  data = training
)
pred2 <- predict(modelo_edadsal2, newdata = testing)
rmse2 <- RMSE(pred2, testing$ln_wage)


sum(is.na(pred2))
sum(is.na(testing$ln_wage))
valid_idx <- !is.na(pred2)
rmse2 <- RMSE(pred2[valid_idx], testing$ln_wage[valid_idx])
rmse2

# Modelo 3: brecha de género simple
modelo_gen <- lm(ln_wage ~ female, data = training)
pred3 <- predict(modelo_gen, newdata = testing)
rmse3 <- RMSE(pred3, testing$ln_wage)

# Modelo 4: brecha de género con controles
modelo_gen2 <- lm(
  ln_wage ~ female + nivel_educativo + formal + oficio + relab +
    totalHoursWorked + sizeFirm + age + agesqr + microEmpresa,
  data = training
)
pred4 <- predict(modelo_gen2, newdata = testing)
rmse4 <- RMSE(pred4, testing$ln_wage)

valid_idx <- !is.na(pred4)
rmse4 <- RMSE(pred4[valid_idx], testing$ln_wage[valid_idx])
rmse4

# Especificación 5: polinomio cúbico en la edad
form_5 <- ln_wage ~ poly(age, 3, raw = TRUE) + female + formal
modelo5 <- lm(form_5, data = training)
pred5 <- predict(modelo5, testing)
rmse5 <- RMSE(pred5, testing$ln_wage)

# Especificación 6: interacciones edad*género
form_6 <- ln_wage ~ age * female + agesqr + formal + totalHoursWorked
modelo6 <- lm(form_6, data = training)
pred6 <- predict(modelo6, testing)
rmse6 <- RMSE(pred6, testing$ln_wage)

# Especificación 7: interacciones edad*nivel educativo
form_7 <- ln_wage ~ poly(age, 2, raw=TRUE) * nivel_educativo + female
modelo7 <- lm(form_7, data = training)
pred7 <- predict(modelo7, testing)
rmse7 <- RMSE(pred7, testing$ln_wage)

valid_idx <- !is.na(pred7)
rmse7 <- RMSE(pred7[valid_idx], testing$ln_wage[valid_idx])
rmse7


# Especificación 8: modelo con splines en edad
library(splines)
library(caret)
# install.packages("elasticnet")
library(lars)
library(elasticnet)
library(Metrics)


form_8 <- ln_wage ~ bs(age, df=5) + female + formal + nivel_educativo
modelo8 <- lm(form_8, data = training)
pred8 <- predict(modelo8, testing)
rmse8 <- RMSE(pred8, testing$ln_wage)

valid_idx <- !is.na(pred8)
rmse8 <- RMSE(pred8[valid_idx], testing$ln_wage[valid_idx])
rmse8


# Especificación 9: modelo regularizado (ridge con caret)
form_9 <- ln_wage ~ bs(age, df=10) + female + formal + nivel_educativo
modelo9 <- lm(form_9, data = training)
pred9 <- predict(modelo9, testing)
rmse9 <- RMSE(pred9, testing$ln_wage)

valid_idx <- !is.na(pred9)
rmse9 <- RMSE(pred9[valid_idx], testing$ln_wage[valid_idx])
rmse9

# Comparar resultados: Guardamos todo en una tabla para visualizar:

rmse_results <- data.frame(
  Modelo = c("Edad–Salario simple",
             "Edad–Salario + controles",
             "Género simple",
             "Género + controles",
             "Polinomio cúbico edad",
             "Edad*Género",
             "Edad*Nivel educativo",
             "Splines en edad",
             "Ridge regularizado"),
  RMSE = c(rmse1, rmse2, rmse3, rmse4,
           rmse5, rmse6, rmse7, rmse8, rmse9)
)

print(rmse_results)

library(ggplot2)

ggplot(rmse_results, aes(x = reorder(Modelo, RMSE), y = RMSE)) +
  geom_col(aes(fill = RMSE), width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(RMSE, 3)), 
            hjust = -0.2, color = "black", size = 4, fontface = "bold") +
  coord_flip() +
  labs(title = "Comparación del RMSE en distintos modelos",
       x = NULL, y = "RMSE") +
  scale_fill_gradient(low = "#6BAED6", high = "#08306B") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#08306B"),
    axis.text.y = element_text(face = "bold", color = "#333333"),
    axis.text.x = element_text(color = "#333333"),
    panel.grid.major.y = element_blank()
  )
# ================================
# Punto (c): Análisis de resultados
# ================================

library(ggplot2)
library(dplyr)

# 1. Identificar el modelo con menor RMSE
best_model_idx <- which.min(rmse_results$RMSE)
best_model_name <- rmse_results$Modelo[best_model_idx]
best_rmse <- rmse_results$RMSE[best_model_idx]

cat("El mejor modelo es:", best_model_name, 
    "con un RMSE de", round(best_rmse, 4), "\n")

# 2. Seleccionar predicciones y errores según el modelo ganador
# (ajustar nombres de objetos predX según tu script: pred1, pred2, ..., pred9)

pred_list <- list(pred1, pred2, pred3, pred4, pred5, pred6, pred7, pred8, pred9)
names(pred_list) <- rmse_results$Modelo

best_pred <- pred_list[[best_model_name]]

# Calcular errores de predicción
errors <- testing$ln_wage - best_pred

error_df <- data.frame(
  observed = testing$ln_wage,
  predicted = best_pred,
  error = errors
)

# 3. Graficar distribución de errores
ggplot(error_df, aes(x = error)) +
  geom_histogram(aes(y = ..density..), bins = 30, 
                 fill = "#1f77b4", color = "white", alpha = 0.7) +
  geom_density(color = "#d62728", size = 1.2, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(
    title = paste("Distribución de errores -", best_model_name),
    x = "Error (observado - predicho)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
    axis.text = element_text(face = "bold", color = "#333333")
  )

# 4. Identificar outliers (cola superior/inferior del IQR)
Q1 <- quantile(error_df$error, 0.25, na.rm = TRUE)
Q3 <- quantile(error_df$error, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- error_df %>%
  filter(error < (Q1 - 1.5 * IQR) | error > (Q3 + 1.5 * IQR))

cat("Número de outliers detectados:", nrow(outliers), "\n")
head(outliers, 10)  # mostrar los primeros 10

# ================================
# Punto (c): Análisis de resultados
# ================================

library(ggplot2)
library(dplyr)

# 1. Identificar el modelo con menor RMSE
best_model_idx <- which.min(rmse_results$RMSE)
best_model_name <- rmse_results$Modelo[best_model_idx]
best_rmse <- rmse_results$RMSE[best_model_idx]

cat("El mejor modelo es:", best_model_name, 
    "con un RMSE de", round(best_rmse, 4), "\n")

# 2. Seleccionar predicciones y errores según el modelo ganador
# (ajustar nombres de objetos predX según tu script: pred1, pred2, ..., pred9)

pred_list <- list(pred1, pred2, pred3, pred4, pred5, pred6, pred7, pred8, pred9)
names(pred_list) <- rmse_results$Modelo

best_pred <- pred_list[[best_model_name]]

# Calcular errores de predicción
errors <- testing$ln_wage - best_pred

error_df <- data.frame(
  observed = testing$ln_wage,
  predicted = best_pred,
  error = errors
)

# 3. Graficar distribución de errores
ggplot(error_df, aes(x = error)) +
  geom_histogram(aes(y = ..density..), bins = 30, 
                 fill = "#1f77b4", color = "white", alpha = 0.7) +
  geom_density(color = "#d62728", size = 1.2, linetype = "solid") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  labs(
    title = paste("Distribución de errores -", best_model_name),
    x = "Error (observado - predicho)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
    axis.text = element_text(face = "bold", color = "#333333")
  )

# 4. Identificar outliers (cola superior/inferior del IQR)
Q1 <- quantile(error_df$error, 0.25, na.rm = TRUE)
Q3 <- quantile(error_df$error, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- error_df %>%
  filter(error < (Q1 - 1.5 * IQR) | error > (Q3 + 1.5 * IQR))

cat("Número de outliers detectados:", nrow(outliers), "\n")
head(outliers, 10)  # mostrar los primeros 10

# ------------------------------------------------------
# 5) LOOCV para los dos mejores modelos
# ------------------------------------------------------

library(caret)
library(dplyr)
library(splines)

# Definimos fórmulas de los modelos
formulas_modelos <- list(
  "Edad–Salario simple"       = ln_wage ~ age + agesqr,
  "Edad–Salario + controles"  = ln_wage ~ age + agesqr + totalHoursWorked + female + formal + nivel_educativo + relab + sizeFirm,
  "Género simple"             = ln_wage ~ female,
  "Género + controles"        = ln_wage ~ female + nivel_educativo + formal + oficio + relab +
    totalHoursWorked + sizeFirm + age + agesqr + microEmpresa,
  "Polinomio cúbico edad"     = ln_wage ~ poly(age, 3, raw = TRUE) + female + formal,
  "Edad*Género"               = ln_wage ~ age * female + agesqr + formal + totalHoursWorked,
  "Edad*Nivel educativo"      = ln_wage ~ poly(age, 2, raw=TRUE) * nivel_educativo + female,
  "Splines en edad"           = ln_wage ~ bs(age, df=5) + female + formal + nivel_educativo,
  "Ridge regularizado"        = ln_wage ~ bs(age, df=10) + female + formal + nivel_educativo
)

# Función rápida de LOOCV usando hatvalues
loocv_lm_fast <- function(formula, data) {
  fit <- lm(formula, data = data)
  h <- hatvalues(fit)
  res <- residuals(fit)
  
  # Evitar divisiones por cero
  if (any(h >= 1)) {
    h[h >= 1] <- 0.999999
  }
  
  cv_errors <- (res / (1 - h))^2
  sqrt(mean(cv_errors, na.rm = TRUE))
}

# Seleccionar los dos mejores modelos según RMSE en test set
mejores <- rmse_results %>% arrange(RMSE) %>% head(2)

# Calcular LOOCV RMSE para esos modelos
loocv_results <- data.frame(
  Modelo = mejores$Modelo,
  RMSE_Test = mejores$RMSE,
  RMSE_LOOCV = sapply(mejores$Modelo, function(nombre) {
    formula_actual <- formulas_modelos[[nombre]]
    loocv_lm_fast(formula_actual, training)
  })
)

print(loocv_results)

library(knitr)

# Imprimir tabla en formato LaTeX
kable(loocv_results,
      format = "latex",
      booktabs = TRUE,
      caption = "RMSE en Test set y LOOCV para los dos mejores modelos",
      digits = 4)

