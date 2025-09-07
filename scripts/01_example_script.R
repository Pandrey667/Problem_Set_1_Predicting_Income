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
