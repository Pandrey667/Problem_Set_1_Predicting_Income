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

#Regresión con nuevos controles ( Sin los malos controles previamente identificados) 

reggenero2 <- lm(
  ln_wage ~ female +
    nivel_educativo + formal + oficio + relab +
    totalHoursWorked + sizeFirm + age + agesqr +  microEmpresa,
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





# APLICACIÓN DE TEOREMA DE FWL CON BOOTSTRAP

#creamos variable female sin etiquetas para aplicar FWL
bd_seleccionados <- bd_seleccionados %>%
  dplyr::mutate(female_num = ifelse(female == "Mujer", 1, 0))


# Obtenemos los residuales de la regresión de los controles sobre la variable Female

bd_seleccionados$res_mujer <- resid(
  lm(female_num ~ nivel_educativo + formal + oficio + relab +
       totalHoursWorked + sizeFirm  +
       age + agesqr + microEmpresa,
     data = bd_seleccionados,
     na.action = na.exclude)
)


# Obtenemos los residuales de la regresión de los controles sobre el salario 
bd_seleccionados$res_salario <- resid(
  lm(ln_wage ~ nivel_educativo + formal + oficio + relab +
       totalHoursWorked + sizeFirm + 
       age + agesqr + microEmpresa,
     data = bd_seleccionados,
     na.action = na.exclude)
)

# Regresamos los residuales del salario contra residuales de female 
regbrecha <- lm(res_salario ~ res_mujer, data = bd_seleccionados)
stargazer(regbrecha,
          title = "Brecha salarial por género (Método FWL)",
          dep.var.labels = "Residuales del salario",
          covariate.labels = c("Residuales de mujer"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3,
          type = "text")


# Comparamos nuestros tres modelos 
stargazer(reggenero, reggenero2, regbrecha,
          title = "Comparación de modelos de brecha salarial",
          dep.var.labels = c("Logaritmo del salario",
                             "Logaritmo del salario (con controles)",
                             "Residuales del salario"),
          covariate.labels = c("Mujer", "Controles", "Residuales de mujer"),
          keep.stat = c("n", "rsq", "adj.rsq"),
          digits = 3,
          align = TRUE,
          type = "text")


## Usamos FWL  con bootstrap 

btrap <- function(data, index){
  d <- data[index, , drop = FALSE]
  d$res_mujer <- resid(
    lm(female_num ~ nivel_educativo + formal + oficio + relab +
         totalHoursWorked + sizeFirm + 
         age + agesqr + microEmpresa,
       data = d, na.action = na.exclude)
  )
  d$res_salario <- resid(
    lm(ln_wage ~ nivel_educativo + formal + oficio + relab +
         totalHoursWorked + sizeFirm + 
         age + agesqr + microEmpresa,
       data = d, na.action = na.exclude)
  )
  
  coef(lm(res_salario ~ res_mujer, data = d))[2]
}


btrap(bd_seleccionados, 1:nrow(bd_seleccionados))


set.seed(2025)
boot(bd_seleccionados, btrap, R = 10000)



#Grafica de predicción por genero

#Segmentamos las regresiones segun el género

modelohombres<- lm(ln_wage ~ age + agesqr,
                   data = bd_seleccionados, subset = (female == "Hombre"))
modelomujeres <- lm(ln_wage ~ age + agesqr,
                    data = bd_seleccionados, subset = (female == "Mujer"))



# Construimos una secuencia de edades para predecir
seqedades <- seq(min(bd_seleccionados$age, na.rm = TRUE),
                 max(bd_seleccionados$age, na.rm = TRUE), by = 1)


# Creamos un data frame con las edades y las edades al cuadrado
dataedad <- data.frame(age = seqedades, agesqr = seqedades^2)

# Predicciones
prediccionhombres  <- predict(modelohombres, newdata = dataedad, se.fit = TRUE)
prediccionmujeres  <- predict(modelomujeres, newdata = dataedad, se.fit = TRUE)


# Submuestras por género
basehombre <- subset(bd_seleccionados, female == "Hombre")
basemujer <- subset(bd_seleccionados, female == "Mujer")

# Edades pico con los coeficientes de cada modelo
picohombre <- -coef(modelohombres)["age"]   /(2 * coef(modelohombres)["agesqr"])
picomujer <- -coef(modelomujeres)["age"]   /(2 * coef(modelomujeres)["agesqr"])


# bootstrap para pico de edad en cada regresión
edadpico <- function(data, indices){
  d <- data[indices, , drop = FALSE]
  cf <- coef(lm(ln_wage ~ age + agesqr, data = d))
  -cf["age"]/(2*cf["agesqr"])
}
set.seed(2025)
bootstraph <- boot(basehombre, edadpico, R = 2000)
bootstrapm <- boot(basemujer,  edadpico, R = 2000)

ci_h <- quantile(bootstraph$t[,1], probs = c(0.025, 0.975), na.rm = TRUE)
ci_m <- quantile(bootstrapm$t[,1], probs = c(0.025, 0.975), na.rm = TRUE)

# Creamos u df para graficas
dfgrafico <- data.frame(
  edad    = seqedades,
  hombres = prediccionhombres$fit,
  mujeres = prediccionmujeres$fit
)



etiquetas <- sprintf("Edad pico Hombres: %.1f (IC95%% %.1f–%.1f)\nEdad pico Mujeres: %.1f (IC95%% %.1f–%.1f)",
                       picohombre, ci_h[1], ci_h[2],
                       picomujer, ci_m[1], ci_m[2])

ggplot() +
  geom_line(data = dfgrafico, aes(x = edad, y = hombres, color = "Hombres"), size = 1) +
  geom_line(data = dfgrafico, aes(x = edad, y = mujeres, color = "Mujeres"), size = 1) +
  geom_vline(xintercept = picohombre, color = "#1b9e77", linetype = "dashed") + 
  geom_vline(xintercept = picomujer,  color = "#d95f02", linetype = "dashed")  +
  annotate("rect", xmin = ci_h[1], xmax = ci_h[2],
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#66c2a5") + 
  annotate("rect", xmin = ci_m[1], xmax = ci_m[2],
           ymin = -Inf, ymax = Inf,
           alpha = 0.15, fill = "#fc8d62") +  
  annotate("text", x = max(seqedades) * 0.7, 
           y = max(c(dfgrafico$hombres, dfgrafico$mujeres), na.rm = TRUE),
           label = etiquetas, hjust = 0, vjust = 1, color = "black", size = 4) +
  labs(title = "Perfil Edad-Salario por Género",
       x = "Edad", y = "Log salario",
       color = "Género") +
  scale_color_manual(values = c("Hombres" = "#1b9e77", "Mujeres" = "#d95f02")) +
  theme_minimal(base_size = 14)



save(bd_seleccionados, file = "bd_seleccionados.RData")


