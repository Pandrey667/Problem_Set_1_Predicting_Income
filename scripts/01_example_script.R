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

