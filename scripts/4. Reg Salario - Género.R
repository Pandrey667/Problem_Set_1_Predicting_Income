## Este scrip desarrolla el punto 4 The gender earnings GAP.

load("C:\\Users\\investigacion\\Desktop\\BD ML\\Taller 1 BDML\\bd_seleccionados.RData")

# REGRESIÓN SALARIO - FEMALE 

# Modelo simple de brecha salarial por género
reggenero <- lm(ln_wage ~ female, data = bd_seleccionados)
summary(reggenero)

stargazer(reggenero,
          title = "Brecha salarial por género",
          dep.var.labels = "Log salario",
          covariate.labels = c("Mujer"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          type = "text")


# REGRESIÓN SALARIO - FEMALE CON CONTROLES

#Alimentamos la base de datos seleccionados con otras variables que serviran como controles 


bd_seleccionados <- db %>%
  dplyr::select(
    ln_wage,
    nivel_educativo, formal, oficio, relab,
    totalHoursWorked, sizeFirm, estrato1,
    jefe_hogar, age, agesqr, female, microEmpresa
  )

#Eliminar las observaciones que tienen ingresos laborales cero o NAs. 
bd_seleccionados <- bd_seleccionados %>%
  filter(is.finite(ln_wage))

#Regresión con nuevos controles

reggenero2 <- lm(
  ln_wage ~ female +
    nivel_educativo + formal + oficio + relab +
    totalHoursWorked + sizeFirm + estrato1 +
    jefe_hogar + age + agesqr +  microEmpresa,
  data = bd_seleccionados
)


summary(reggenero2)
stargazer(reggenero2,
          title = "Brecha salarial por género (con controles)",
          align = TRUE,
          dep.var.labels = "Logaritmo del salario",
          covariate.labels = c("Mujer", "Nivel educativo", "Formalidad", 
                               "Ocupación (oficio)", "Relación laboral", 
                               "Horas totales de trabajo ", "Tamaño empresa",
                               "Estrato socioeconómico", "Jefe de hogar", 
                               "Edad", "Edad al cuadrado",
                               "Microempresa"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          type = "text")
