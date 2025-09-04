## Este scrip desarrolla el punto 3 Age-wage profile.

load("C:/Users/investigacion/Desktop/data_imputada_GEIH.RData")
library(dplyr)
library(stargazer)
library(ggplot2)

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

