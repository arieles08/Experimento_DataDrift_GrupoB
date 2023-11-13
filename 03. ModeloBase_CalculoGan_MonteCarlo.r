#limpio la memoria
rm(list=ls())
gc()

require("data.table")
require("xgboost")
library(caret)
library(openxlsx)

#cargo los datasets
setwd( "C:\\Users\\leandro.morinigo\\OneDrive - Personal\\!4.DM_EconyFin\\! 0.Recuperatorio\\datasets")


# Importar archivos:
dtrain <- fread("dtrain.csv.gz")

dval <- fread("dval.csv.gz")

#dejo la clase en 0,1
dataset_generacion[ , clase01 := as.integer(clase=="SI") ]

#------------------------------------------
# voy a partir en train y test 10 veces de manera aleatoria, para poder hacer 10 train con sus 10 test y 10 evaluaciones de ganancia

set.seed(333) # Semilla inicial

# 10 iteraciones
n_iter <- 10

# Creamos 10 juegos de train y test
dtrain_n <- list()
dtest_n <- list()

for (i in 1:n_iter) {
  
  # División 50% - 50% en las primeras 5 iteraciones, 60% - 40% en las restantes
  if (i <= 5) {
    test_size <- 0.5
  } else {
    test_size <- 0.4
  }
  
  # División aleatoria de train y test
  idx <- sample(nrow(dtrain), nrow(dtrain) * test_size)
  dtest <- dtrain[idx, ]
  dtrainN <- dtrain[-idx, ]
  
  # Almacenamos los resultados en listas
  dtrain_n[[i]] <- dtrainN
  dtest_n[[i]] <- dtest
  
  # Nueva semilla para la siguiente iteración
  set.seed(333 + i*33)
}

# Ejemplo de acceso al primer juego de train y test
dtrain1 <- dtrain_n[[1]]
dtest1 <- dtest_n[[1]]

dim(dtrain1)
dim(dtest1)


#---------------------------------------------------------------
#ahora deberia de entrenar los 10 modelos, lo unico que cambiaria seria el conjunto de train: dtrain_n[[i]]

#Creo lista para los 10 modelos base
modelo_n <- list()

for (i in 1:n_iter) {
  #Aux
  dtrain_i <- dtrain_n[[i]]
  
  dgeneracion  <- xgb.DMatrix( data=  data.matrix( dtrain_i[ , !c("numero_de_cliente","clase","clase01"), with=FALSE]),
                               label= dtrain_i[ , clase01 ] )
  
  #llamo al XGBoost,  notar lo frugal de los hiperparametros
  set.seed( 102191 ) #mi querida random seed, para que las corridas sean reproducibles
  
  modelo  <- xgb.train(data= dgeneracion,
                       objective= "binary:logistic",
                       tree_method= "hist",
                       max_bin= 31,
                       base_score= mean( getinfo(dgeneracion, "label") ),
                       eta= 0.04,
                       nrounds= 300,
                       colsample_bytree= 0.6 )
  
  # Almacenamos los resultados en listas
  modelo_n[[i]] <- modelo
}

# Ejemplo de acceso al primer modelo
modelo1 <- modelo_n[[1]]
  
#---------------------------------------------------------------
#Aplico los 10 modelos sobre su respectivo conjunto de test, para obtener las 10 prediccion que realiza

predict_n <- list()

for (i in 1:n_iter) {
  #Aux
  dtrainN <- dtrain_n[[i]]
  dtest   <- dtest_n[[i]]
  modelo  <- modelo_n[[i]]
  #Genero matrix de los dtest correspondiente
  daplicacion  <- xgb.DMatrix( data= data.matrix( dtest[ , !c("numero_de_cliente","clase","clase01"), with=FALSE]) )
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, daplicacion )
  
  #uno las columnas de numero_de_cliente y la probabilidad recien calculada
  prediccion_final  <- cbind(  dtest[ ,c("numero_de_cliente", "clase")], aplicacion_prediccion )
  
  #le doy nombre a las columnas
  colnames( prediccion_final )  <- c( "numero_de_cliente", "clase", "prob_positivo" )

# Almacenamos los resultados en listas
predict_n[[i]] <- prediccion_final
}

# Ejemplo de acceso a la primer prediccion
predict1 <- predict_n[[1]]


#---------------------------------------------------------------
#Calculo las 10 Ganancias y se la agrego a c/u de los listados en predict_n (cliente, clase, prob, +GANANCIA)

# Auxiliar: Creo la función ganancia con 2 parametros, que luego usaré:
ganancia <- function(clase, prob) {
  if (prob > 1/40) {
    if (clase == "SI") {
      return(78000)
    } else {
      return(-2000)
    }
  } else {
    return(0)
  }
}

pred_n <- list()
for (i in 1:n_iter) {
  # Crear un data.table a partir de la lista
  predict_dt <- data.table(predict_n[[i]])
  
  # Agregar la columna de ganancia
  predict_dt[, Ganancia := ifelse(prob_positivo > 1/40 & clase == "SI", 78000, ifelse(prob_positivo > 1/40 & clase == "NO", -2000, 0))]

  # Almacenamos los resultados en la listas ya creada
  predict_n[[i]] <- predict_dt
}

# Ejemplo de acceso a la primer prediccion
predict_dt1 <- predict_n[[1]]

#------------------------------------------------------
#Exporto los arhivos y calculo las ganancias de c/u

# Unir los data.tables en un solo data.frame
predict_df <- do.call(cbind, predict_n)

# Exportar a Excel
write.xlsx(predict_df, "10pred_ModBase_train-test.xlsx")

# Calcular la ganancia para cada data.table y guardar en un vector
ganancias <- sapply(predict_n, function(dt) sum(dt$Ganancia))

#normalizar la ganancia 5 primeros 100/50 = 2 y 5 siguientes 100/40 = 2.5
ganancia_normalizada <- ifelse(seq_along(ganancias) <= 5, ganancias * 2, ganancias * 2.5)

summary(ganancia_normalizada)
