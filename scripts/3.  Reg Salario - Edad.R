## Este scrip desarrolla el punto 3 Edad Salario

# intalación de paquetes 
library(tidyverse)
library(stargazer)
library(boot)

#Renombramos la base de datos
db <- df_2
# Verificamos las variables que usaremos
colnames(db)

# REGRESIÓN 1 : SALARIO - EDAD 

# Preparamos variables para regresión ( Log salario y  edad cuadrado junto a algunos controles)
db <- db %>% mutate(ln_wage = log(ingreso_total))
db <- db %>% mutate(agesqr = age^2)
db <- db %>%  mutate(female = factor(ifelse(sex == 0, 1, 0),
                                     levels = c(0, 1),
                                     labels = c("Hombre", "Mujer")))
db <- db %>% mutate(formal = factor(formal, levels = c(1, 0),
                                    labels = c("Formal", "Informal")))
db <- db %>% rename(nivel_educativo = p6210)
db$nivel_educativo <- factor(
  db$nivel_educativo,
  levels = c(1, 2, 3, 4, 5, 6, 7),
  labels = c("Ninguno",
             "Preescolar",
             "Básica primaria (1º - 5º)",
             "Básica secundaria (6º - 9º)",
             "Media (10º - 13º)",
             "Superior o universitaria",
             "No sabe, no informa")
)

db <- db %>%
  mutate(estrato1 = factor(estrato1,
                           levels = 1:6,
                           labels = c("Estrato 1","Estrato 2","Estrato 3",
                                      "Estrato 4","Estrato 5","Estrato 6")))

db <- db %>% mutate(jefe_hogar = ifelse(p6050 == 1, 1, 0))
db <- db %>%
  mutate(relab = factor(relab,
                        levels = 1:9,
                        labels = c("Obrero o empleado de empresa particular",
                                   "Obrero o empleado del gobierno",
                                   "Empleado doméstico",
                                   "Trabajador por cuenta propia",
                                   "Patrón o empleador",
                                   "Trabajador familiar sin remuneración",
                                   "Trabajador sin remuneración en empresas o negocios de otros hogares",
                                   "Jornalero o peón",
                                   "Otro")))

db <- db %>%
  mutate(
    sizeFirm = factor(
      sizeFirm,
      levels = 1:5,
      labels = c("Self-employed",
                 "2-5 workers",
                 "6-10 workers",
                 "11-50 workers",
                 ">50 workers")
    )
  )


# Creamos una base de datos filtrada con nuestras variables de interes y eliminamos los NA de estas variables
bd_seleccionados <- db %>% dplyr::select(
  ln_wage, totalHoursWorked, female, formal, nivel_educativo, age, agesqr, estrato1, jefe_hogar, relab, sizeFirm
)
#Eliminar las observaciones que tienen ingresos laborales cero o NAs. 
bd_seleccionados <- bd_seleccionados %>%
  filter(is.finite(ln_wage))



# Regresamos nuestro modelo simple
regedadsalario <- lm(ln_wage ~ age + agesqr, bd_seleccionados)
summary(regedadsalario)
stargazer(regedadsalario,
          type = "text",
          title = "Perfil edad-salario",
          dep.var.labels = "Log salario",
          covariate.labels = c("Edad", "Edad²"),
          keep.stat = c("n", "rsq"))


# Grafica para ver la edad donde el ingreso comienza a descender

#Calculo de edad maxima
b <- coef(regedadsalario)
edad_pico <- -b["age"] / (2 * b["agesqr"])

edades <- seq(min(bd_seleccionados$age, na.rm=TRUE),
              max(bd_seleccionados$age, na.rm=TRUE), 1)

salariopredicho <- b["(Intercept)"] + b["age"]*edades + b["agesqr"]*(edades^2)

df_grafico <- data.frame(edad = edades, salariopredicho = salariopredicho)

# Grafico
ggplot(df_grafico, aes(edad, salariopredicho)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = edad_pico, linetype = "dashed", color = "red") +
  geom_text(aes(x = edad_pico, y = max(salariopredicho),
                label = paste("Pico ≈", round(edad_pico,1), "años")),
            vjust = -0.5, color = "red") +
  labs(title = "Perfil edad–salario estimado",
       x = "Edad", y = "Log salario") +
  theme_classic()


ggsave("perfil_edad_salario.jpg", plot = last_plot(),
       width = 8, height = 6, dpi = 300)


# REGRESIÓN 1 : SALARIO - EDAD  - CONTROLES


# Para mejorar nuestro modelo incluimos las variables de control 
regedadsalario2 <- lm( ln_wage ~ age + agesqr + totalHoursWorked + female + formal + jefe_hogar + nivel_educativo + 
                         estrato1 + relab + sizeFirm,data = bd_seleccionados)
summary(regedadsalario2)
stargazer(regedadsalario2,
          type = "text",
          title = "Perfil edad-salario con controles",
          dep.var.labels = "Log salario",
          covariate.labels = c("Edad", "Edad²"),
          keep.stat = c("n", "rsq"))


#Calculo de edad maxima

b <- coef(regedadsalario2)
edad_pico2 <- -b["age"] / (2 * b["agesqr"])

# Secuencia de edades 
edades2 <- seq(min(bd_seleccionados$age, na.rm = TRUE),
               max(bd_seleccionados$age, na.rm = TRUE), by = 1)

# Valores predichos
salariopredicho2 <- b["(Intercept)"] + b["age"] * edades2 + b["agesqr"] * (edades2^2)

# Creamos un df para la grafica
df_grafico <- data.frame(edad = edades2, salariopredicho = salariopredicho2)


ggplot(df_grafico, aes(x = edad, y = salariopredicho)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = edad_pico2, linetype = "dashed", color = "red") +
  geom_text(aes(x = edad_pico2, y = max(salariopredicho, na.rm = TRUE),
                label = paste("Pico ≈", round(edad_pico2, 1), "años")),
            vjust = -0.5, color = "red") +
  labs(title = "Perfil edad–salario estimado (con controles)",
       x = "Edad", y = "Log salario") +
  theme_classic()

ggsave("Perfil_edad–salario_estimado_con_controles.jpg", plot = last_plot(),
       width = 8, height = 6, dpi = 300)



# Para mejorar nuestro modelo incluimos las variables de control sin estrato ni jefe de hogar.
# El analisis de nuestros controles nos hace reflexionar que la variable de estrato
# puede considerarse un mal control porque puede estar correlacionado con el salario
# como consecuencia del mismo. Ademas,  la variable jefe_hogar puede llegar a ser un mas control, 
# ya que el salario puede condicionar quién es jefe de hogar, no solo al revés.


regedadsalario3 <- lm( ln_wage ~ age + agesqr + totalHoursWorked + female + formal +  nivel_educativo + 
                        + relab + sizeFirm,data = bd_seleccionados)
summary(regedadsalario3)
stargazer(regedadsalario3,
          type = "text",
          title = "Perfil edad-salario con controles",
          dep.var.labels = "Log salario",
          covariate.labels = c("Edad", "Edad²"),
          keep.stat = c("n", "rsq"))


#Calculo de edad maxima

b <- coef(regedadsalario3)
edad_pico2 <- -b["age"] / (2 * b["agesqr"])

# Secuencia de edades 
edades3 <- seq(min(bd_seleccionados$age, na.rm = TRUE),
               max(bd_seleccionados$age, na.rm = TRUE), by = 1)

# Valores predichos
salariopredicho3 <- b["(Intercept)"] + b["age"] * edades2 + b["agesqr"] * (edades2^2)

# Creamos un df para la grafica
df_grafico <- data.frame(edad = edades3, salariopredicho = salariopredicho3)


ggplot(df_grafico, aes(x = edad, y = salariopredicho)) +
  geom_line(color = "purple") +
  geom_vline(xintercept = edad_pico2, linetype = "dashed", color = "red") +
  geom_text(aes(x = edad_pico2, y = max(salariopredicho, na.rm = TRUE),
                label = paste("Pico ≈", round(edad_pico2, 1), "años")),
            vjust = -0.5, color = "red") +
  labs(title = "Perfil edad–salario estimado (sin malos controles)",
       x = "Edad", y = "Log salario") +
  theme_classic()

ggsave("Perfil_edad–salario_estimado_sin_maloscontroles.jpg", plot = last_plot(),
       width = 8, height = 6, dpi = 300)

# Creamos tabla comparativa de las tres regresiones 
# Formato Texto R
stargazer(regedadsalario, regedadsalario2, regedadsalario3,
          type = "text",
          title = "Perfil edad–salario (comparativo)",
          dep.var.labels = "Log salario",
          column.labels = c("Básico", "Con controles", "Sin malos controles"),
          keep = c("age", "agesqr"),
          covariate.labels = c("Edad", "Edad²"),
          keep.stat = c("n", "rsq", "adj.rsq"))

# Formato latex
stargazer(regedadsalario, regedadsalario2, regedadsalario3,
          type = "latex", out = "tabla_edad_salario.tex",
          title = "Perfil edad–salario (comparativo)",
          dep.var.labels = "Log salario",
          column.labels = c("Básico", "Con controles", "Sin malos controles"),
          keep = c("age", "agesqr"),
          covariate.labels = c("Edad", "Edad²"),
          keep.stat = c("n", "rsq", "adj.rsq"))



# BOOTSTRAP DEL MODELO CON INTERVALOS DE CONFIANZA 

set.seed(2025)

#Creamos una función que me permita sacar los coeficientes de mi modelo simple
coeficientes <- function(bd_seleccionados, indices) {
  fit <- lm(ln_wage ~ age + agesqr, data = bd_seleccionados[indices, ])
  return(coef(fit))
}

#Corremos el bootstrap y obtenemos los indices y los erroees estandar
bootstrap <- boot(data = bd_seleccionados, statistic = coeficientes, R = 10000)
coeficientesboostrap <- bootstrap$t0
errores <- apply(bootstrap$t,2,sd)

# Crear una secuencia de edades para estimar el log del ingreso
edades <- seq(min(bd_seleccionados$age,na.rm=TRUE), max(bd_seleccionados$age,na.rm=TRUE),length=50)
edades

#Creamos los intervalos de confianza 

salario_estimado <- coeficientesboostrap[1] + coeficientesboostrap[2]*edades + coeficientesboostrap[3]*edades^2

icinferior <- (coeficientesboostrap[1] - 1.96*errores[1]) +
  (coeficientesboostrap[2] - 1.96*errores[2])*edades +
  (coeficientesboostrap[3] - 1.96*errores[3])*(edades^2)

icsuperior <- (coeficientesboostrap[1] + 1.96*errores[1]) +
  (coeficientesboostrap[2] + 1.96*errores[2])*edades +
  (coeficientesboostrap[3] + 1.96*errores[3])*(edades^2)


# Creamos un Data frame con los resultados de salario estimado e intervalos de confianza
df <- data.frame(
  edad = edades,
  salario_estimado = salario_estimado,
  limite_inferior = icinferior,
  limite_superior = icsuperior
)

df <- df %>% mutate(salario_estimado = exp(salario_estimado),
                    limite_inferior = exp(limite_inferior),
                    limite_superior = exp(limite_superior))


#Calculamos la edad pico en este caso
edad_pico_boot <- -coeficientesboostrap[2] / (2 * coeficientesboostrap[3])

#Extraemos la estimación de salario
punto_max <- df[which.max(df$salario_estimado), ]

# Gráfico con la etiqueta
ggplot(df, aes(x = edad, y = salario_estimado)) +
  geom_ribbon(aes(ymin = limite_inferior, ymax = limite_superior), alpha = 0.25, fill = "grey") +
  geom_line(linewidth = 1, color = "green") +
  geom_vline(xintercept = punto_max$edad, linetype = "dashed", color = "black") +
  geom_point(data = punto_max, aes(x = edad, y = salario_estimado), color = "black", size = 3) +
  geom_text(aes(x = punto_max$edad, y = punto_max$salario_estimado,
                label = paste0("Pico ≈ ", round(punto_max$edad, 1), " años\n$",
                               format(round(punto_max$salario_estimado, 0), big.mark = ","))),
            vjust = -1, color = "black", size = 3.2, fontface = "plain") +
  labs(title = "Perfil edad–salario con bootstrap",
       x = "Edad", y = "Salario en $") +
  theme_classic()

ggsave("perfil_edad_salario_bootstrap_con_pico.jpg", plot = last_plot(),
       width = 8, height = 6, dpi = 300)
