# Paquetes --------------------------------------------------------------------
library(dplyr)
library(broom)
library(ggcorrplot)
library(Hmisc)
library(car)
# EDA -------------------------------------------------------------------------
nba <- read.csv("./data/nba.csv")
dim(nba) # 458 observaciones, 28 variables 
sample_size <- floor(0.75 * nrow(nba)) # tamaño de la particion (floor aproxima)

set.seed(1234)
train_id <- sample(nrow(nba), size = sample_size)
train <- nba[train_id, ]
test <- nba[-train_id, ]

glimpse(nba) # vistazo rapido a las variables

# Comrpobacion de ausencia de valores duplicdos
sum(duplicated(nba))
#(HACER DICCIONARIO DE DATOS)

# Comprobacion de NA's
sum(is.na(nba)) # 8 na en todo el dataset.

nba[rowSums(is.na(nba)) > 0, ] 
# aplicando la fila de sumas al resultado de is.na(nba) y filtrando por mayor 
# a 0, se obtienen las filas que tienen NA. Son la 30 y la 38. 
# Tyler Lydon y Trey McKinney-Jones. Los elimino. 

nba <- nba[-c(30, 38),]

# Salarios mas alto 

nba[which.max(nba$Salary),] # Stephen Curry, salario mas alto 

# Correlaciones: todas las variables ------------------------------------------

rh <- rcorr(as.matrix(nba[, -c(1, 3, 6)]), type = "pearson")

ggcorrplot(rh$r, method = 'square', type = 'lower', lab = TRUE) +
  ggtitle("Todas las variables") +
  theme_bw() +
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle =  90)) # para ver bien el numero
# ventana nueva 
# correlacion mas fuerte con el salario: MP 0.51.

# Observando las correlaciones con el salario, podemos ver como para las 
# variables que se dividen en ofensivo, defensivo y total, la correlacion 
# es mayor en las variables que recogen ambas cosas (excepto rebotes)
# Correlacion mas negativa: numero del draft

# Correlaciones fuertes -------------------------------------------------------

correlaciones_fuertes <- nba %>% select(ORB., DRB., TRB., DWS, 
                                        OWS, WS, PER, WS.48, OBPM, BPM, VORP)
rh_fuertes <- rcorr(as.matrix(correlaciones_fuertes, type = "pearson"))
    
# Grafico de las fuertes 
ggcorrplot(rh_fuertes$r, method = 'square', type = 'lower', lab = TRUE) +
  ggtitle("Variables fuertes") +
  theme_bw() +
  theme_minimal() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.text.x = element_text(angle =  90))

# DRB y TRB: 0.91
# ORB y TRB : 0.8
# DWS y WS: 0.84 
# OWS y WS: 0.96 
# PER y WS/48: 0.93 
# PER y OBPM: 0.92
# PER y BPM: 0.86 
# WS/48 y OBPM: 0.93 
# OBPM y BPM: 0.91
# WS/48 y BPM: 0.87
# WS y VORP: 0.91
# OWS y VORP: 0.87


ggplot(data = nba, mapping = aes(x = Tm, y = Salary, fill = Tm)) +
  geom_col() # salario por equipos, mas o menos todos tienen un mismo nivel,
# excepto tot


ggplot(nba, aes(x = VORP, y = Salary, color = VORP)) +
  geom_point() 
  

# Voy a hacer un modelo solo con lo que mas se correlacione con el salario,
# por probar. 
# Otro modelo con todo
# Otro modelo solo con las variables agrupadas, sin distincion entre ofensivo
# ni defnesivo 
# Otro modelo con las interacciones entre ellas. 
# Otro modelo con algunas interacciones entre edad 
# Otro modelo con interacciones entre variables defensivas (Tapon, DRB)

# Grafico de todas las correlaciones, una a una con el salario 
nba_numericas <- nba %>% 
  select(-c(1, 3, 6))  # aqui estoy quitando algunas categoricas

# Dispersiones ----------------------------------------------------------------

for (var in names(nba_numericas)) {
  plot <- ggplot(nba_numericas, aes(x = nba[, var], y = Salary)) +
    geom_point() +
    labs(x = var) +
    theme_minimal()
  print(plot)
}
  # Draft -----
# Parece haber cierta tendencia decreciente, aunque hay muchos puntos 
# dispersos por encima. Sería interesante hacer bucket con draft, por 
# ejemplo, 10 en cada trozo

  # Edad -----
# Para misma edad, muchos salarios distintos. No creo que sirva de mucho
cor(nba$Salary, nba$Age) # sin embargo, la correlacion es del 0.33  
  
# Games ----- 
# idem: salarios muy diversos en mismo numero de partidos
cor(nba$Salary, nba$G) # pero la correlacion es del 0.29
  
# Minutos jugados -----
cor(nba$Salary, nba$MP) # Correlacion del 0.50

  # PER -----
# Cambio la escala? 
cor(nba$Salary, nba$PER) # 0.26 de PER

ggplot(nba_numericas, aes(x = PER, y = Salary)) +
  geom_point() +
  scale_x_log10()

# No sirve de mucho cambiar la esacala  

  # TS ----- 
# Muy concentrado: ojo, hay una con 1.5, Hay que investigar, puede ser erro
cor(nba$Salary, nba$TS) # 0.16

  # X3PAr -----
# No hay relacion aparente 
cor(nba$Salary, nba$X3PAr) # -0.07

  # FTr -----
cor(nba$Salary, nba$FTr) # nada, muhcos salarios para mismo rate. Revisar 
# valores de la derecha. 0.023
nba[nba$FTr > 0.9 , ] # tiradores de tiros libres

  # ORB ----- 
cor(nba$Salary, nba$ORB) # no sirve para nada (de manera individual)

  # DRB ----- 
cor(nba$Salary, nba$DRB) # 0.2, influye anque poco
# Parece que se pagan mejor los rebotes si son defensivos 
  
  # TRB -----
cor(nba$Salary, nba$TRB) # un 0.13

  # AST -----
cor(nba$Salary, nba$AST) # 0.26, puede ser interesante añadirla

  # STL -----
cor(nba$Salary, nba$STL.) # 0.03, no vale para nada 
nba[which.max(nba$STL.),] # valor maximo 

  # BLK -----
cor(nba$Salary, nba$BLK.) # 0.04, no vale para nada 

  # TOV -----
cor(nba$Salary, nba$TOV.) # -00.4, nada

  # USG -----
cor(nba$Salary, nba$USG.) # 0.29
# Efecto con asisetncias? y con DRB? y con per?
cor(nba$PER, nba$USG.) # con PER esta algo correlacionada
  
  # OWS ----- 
cor(nba$Salary, nba$OWS) # 0.56, la mas alta hasta ahora. 

  # DWS ----- 
cor(nba$Salary, nba$DWS) # 0.50

  # WS ----- 
cor(nba$Salary, nba$WS) # 0.59, mas alta que las otras dos

  # WS.48 -----
cor(nba$Salary, nba$WS.48) # muy baja respecto a las demas Shares, paso de esta

  # OBPM -----
cor(nba$Salary, nba$OBPM) # 0.26

 # DBPM ----- 
cor(nba$Salary, nba$DBPM) # 0.17

 # BPM -----
cor(nba$Salary, nba$BPM) # 0.30, mas que por separado

 # VORP -----
cor(nba$Salary, nba$VORP) # 0.57. 


# Densidades ------------------------------------------------------------------

for (var in names(nba_numericas)) {
  plot <- ggplot(nba_numericas, aes(x = nba[, var])) +
    geom_density() +
    labs(x = var, title = var) +
    theme_minimal()
  print(plot)
}


# Muchas de ellas estan concetnradas en torno a una media, no hay 
# mucha dispersion.

# Calculo de las varianzas relativas de cada variable, para 
# ver cuanto contribuyen 

varianza_total <- sum(
  apply(nba_numericas, 2, sd, na.rm = TRUE)^2
  ) # suma de todas las varianzas

which.min(
  apply(nba_numericas, 2, sd, na.rm = TRUE)^2 / varianza_total
  ) # la que menos aporta es TS., en terminos de varianza relativa. 


# Primer modelo ---------------------------------------------------------------
# Variables con mayor correlacion con el salario 

modelo1 <- lm(Salary ~ WS + VORP + MP  + Age + TS., data = nba)
summary(modelo1)
residuals(modelo1)

AIC(modelo1)

modelo2 <- lm(Salary ~ WS + VORP + MP  + Age + NBA_DraftNumber, data = nba)
AIC(modelo2)

library(MASS)

modelo3 <- lm(Salary ~., data = nba_numericas)
AIC(modelo3)
stepAIC(modelo3, direction = "backward") # segun esto, 
# lm(formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
# X3PAr + ORB. + TRB. + USG. + WS + OBPM, data = nba_numericas)

modelo_sugerido_backward <- lm(
  formula = Salary ~ NBA_DraftNumber + Age + G + MP + PER + 
X3PAr + ORB. + TRB. + USG. + WS + OBPM, 
data = nba_numericas)
summary(modelo_sugerido_backward)

# Pruebo con Forward 

# Defino el modelo inicial
modelo_inicial <- lm(Salary~1, data = nba_numericas)

# Defino el modelo final 
modelo_final <- lm(Salary~., data = nba_numericas)

# Ejecuto el Setpwise Forward
stepAIC(modelo_inicial, 
        direction = "forward",
        scope = list(lower = modelo_inicial,
                   upper = modelo_final)
)

# Modelo segun Forward 
modelo_sugerido_forward <- lm(formula = Salary ~ WS + Age + NBA_DraftNumber + 
                                USG. + G + MP + DRB. + VORP, 
                              data = nba_numericas)

AIC(modelo_sugerido_backward, modelo_sugerido_forward)
# El forward tiee algo menos de AIC 

summary(modelo_sugerido_forward) # el modelo tiene un error de 5 millones

mean(nba$Salary) # el salario medio es de 6 millones

# El modelo es una basura 

library(gvlma)
summary(gvlma(modelo_sugerido_forward))

residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(modelo_sugerido_forward)

qqPlot(modelo_sugerido_forward, id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
# Revisar 144 y 328 
