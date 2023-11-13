# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")

#cambiar lineas: 15, 34,  71, 72, 88, 91, 95, 96, y el nombre 10.1 -> por el que corresponde

#------------ Carga el Dataset -----------------------------------------------------------------------------
# Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("~/buckets/b1/") 

#cargo el dataset
dataset <- fread("./datasets/06.1.rank_cero_fijo.csv.gz") 

#check
print(dim(dataset))

#----------------Modifica Dataset: agrega clase binaria y Ganancia-------------------
# Genero CLASE_BINARIA: clase01
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

# Agrego campo de Ganancia_INDIVIDUAL -> es INDEPENDIENTE de la predicción -> conociendo la clase_ternaria real -> ya se puede saber
dataset[  , ganancia :=  ifelse( clase_ternaria == "BAJA+2", 273000, -7000  ) ]

#check
table(dataset$foto_mes, dataset$clase01 )

#---------- Levanta salida de Optimización Bayesiana: para elegir lo TopN Parametros -----------------------------------------------------------------------------
# queda guardada en top_parametros y se puede llamar a cada uno como top_parametros$top1 ó top_parametros$top2...

#leo la salida de la optimizaciob bayesiana
ruta  <- "~/buckets/b1/exp/HT8230_rankcerofijo2/"
nombre  <- "BO_log.txt"

tb_log  <- fread( paste0(ruta,nombre))

#ordeno por Ganancia --> tener el TopN con mejor ganancia arriba
setorder( tb_log, -ganancia )

#Genero una lista con los Top10 mejores parametros de la BO: 
top_parametros <- list()

for (i in 1:10){
  #elijo ahora por ejemplo el Top1:
  parametros <- as.list(copy(tb_log[i]))
  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL
  
  # Guarda los parámetros en la lista
  top_parametros[[paste0('top', i)]] <- parametros
}

#como llamo ahora el top1 (podria llamar el top2, top3...top10):
#opcion 1: top_parametros$top1
#opcion 2: top_parametros[[1]]


#---------- Elijo 20 Semillas para correr el mismo modelo -----------------------------------------------------------------------------
# queda guardada en **semillas** y se puede llamar a cada uno como `semillas[1]` ó `semillas[2]`...

#leo la salida de la optimizaciob bayesiana
ruta  <- "~/buckets/b1/datasets/"
nombre  <- "Semillas1a20_2023.txt"

semillas  <- fread( paste0(ruta,nombre))
semillas  <- as.numeric(unlist(semillas))

#---------- Entrenamiento del LightGBM -----------------------------------------------------------------------------
# parametros$seed <- semillas[1] semillas[2] ...
# param = top_parametros$top1 ó top_parametros$top2...

#--------------Carga de Parametros------------------------------------
#Elijo meses de entrenamiento y testeo:
# meses donde se entrena el modelo
training <- c(202012, 202101, 202102, 202103, 202104, 202105)
future <- c(202107) # meses donde se aplica el modelo

#Cantidad N del TopN de Parametros a seleccionar de la BO (Optimización Bayesiana):
topN <- 5

#Cantidad de Semillas a utilizar (1 a 20):
k <- 10

# creo la carpeta donde va a guardar las predicciones
setwd("~/buckets/b1/exp/")
dir.create("./Experim_Colab/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/Experim_Colab/")

#--------------Preparación previa-------------------------------------
#Genero el Dataset de TRAIN:
# establezco donde entreno:
dataset[, train := 0L]
dataset[foto_mes %in% training, train := 1L]

# establesco donde predigo:
dtest <- dataset[foto_mes %in% future]

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01", "ganancia"))

# dejo los datos de TRAIN en el formato que necesita LightGBM para Entrenar
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
  label = dataset[train == 1L, clase01]
)

#---------Recorro TopN parámetos de BO y N Semillas: Entrenamiento -> Predicción -> Feature Importance -> lista_predicciones-------------------------------------
#Lista donde guardo todas las predicciones generadas el en doble for:
lista_predicciones <- list()

for( i in 1:topN ){
  #Elijo los parametros del Modelo de la BO que usamos:  
  parametros <- top_parametros[[i]] #-> aca la semilla es la 1ra: 997751 -> despues es necesario cambiarla a la que queremos
  
  #VOY A TENER QUE: generar la prediccion en un dataset que tenga los campos basicos -> aca le agregue ganancia cuando en realidad no necesitamos
  tb_prediccion  <- dtest[  , list( numero_de_cliente, foto_mes, clase_ternaria, ganancia ) ]

  #--> posiblemente esta parte quede afuera de la iteración , no necesita estar en la iteración
  
    for( semilla in semillas[1:k] ){
      #Elijo Semilla y Parametros:
      parametros$seed  <- semilla #-> se cambia la semilla
      
      #ENTRENO el Modelo: genero el modelo con datos de TRAIN
      set.seed( parametros$seed )
      message("Entrenando el modelo")
      timestamp()
      modelo  <- lightgbm( data= dtrain,
                                 param=  parametros,
                                 verbose= -100 )
      timestamp()
      
      #Aplico el modelo a los datos nuevos
      message("Prediciendo")
      prediccion <- predict(modelo,
                            data.matrix(dtest[, campos_buenos, with = FALSE] ) )
      
      #agrego columnas con la probabilidad de cada predicción de c/semilla --> esto lo tengo que testear a ver si la prediccion que genera es la que le corresponde a ese
      tb_prediccion[ , paste0('prob_top',i,'_', semilla) := prediccion ]
      
      #Genero el .txt con el feature importance del modelo de la ultima semilla 
      if(semilla == semillas[k] && i == 1) {
        tb_importancia <- as.data.table(lgb.importance(modelo))
        FIname <- paste0('10.1.Top',i,'_sem',semillas[k],'_FImpo.txt')
        fwrite(tb_importancia,
               file = FIname,
               sep = "\t")   }
      
      #borro y limpio la memoria para la vuelta siguiente del for
      rm( modelo)
      
      message(paste0("fin semilla ", semilla))
    }
  
  #Guardo todas las pedicciones:
  lista_predicciones[[paste0('top', i)]] <- tb_prediccion
  
  #borro y limpio memoria para el siguiente for
  rm( tb_prediccion )
  rm( parametros)
  gc()
  
  message(paste0("fin Top ", i))
}

#-------------------- Calculo la GANANCIA-----------------------------------
#Desde el archivo lista_predicciones -> llamo a cada modelo -> de calculo la Ganancia

# Crea un nuevo data.table para almacenar los resultados -> Unico valor de Ganancia (promedio de la meseta) para cada modelo
resultados <- data.table(hyperparam = integer(), seed = integer(), ganancia = numeric(), pos_optima = integer())

#
for (i in 1:topN) {
  #Traigo el dataframe de predicciones de las k semillas para los TopN modelos, antes generado:
  tb_prediccion <- lista_predicciones[[i]]
  
  for (semilla in semillas[1:k]) {
    # Ordena por probabilidad descendente -> uso order porque es de data.table y asi funciona (setorder: NO funciona)
    tb_prediccion <- tb_prediccion[order(-tb_prediccion[[paste0('prob_top', i, '_', semilla)]])]
    
    # Calcula la ganancia acumulada
    tb_prediccion[, paste0('gan_acum_', semilla) := cumsum(ganancia)]
    
    # Calcula la ganancia suavizada: calcular media movil de 1001 puntos
    tb_prediccion[, paste0('gan_suavizada_', semilla) :=
                    frollmean(x = get(paste0('gan_acum_', semilla)),
                              n = 1001, align = "center", na.rm = TRUE, hasNA = TRUE)]
    
    # Encuentra el máximo de la ganancia suavizada y su posicion (punto de corte optimo y su único valor de ganancia)
    max_gan_suavizada <- max(tb_prediccion[, paste0('gan_suavizada_', semilla), with = FALSE], na.rm = TRUE)
    pos_optima <- which.max(tb_prediccion[, paste0('gan_suavizada_', semilla), with = FALSE] == max_gan_suavizada)
    
    # Almacena los resultados en el nuevo data.table
    resultados <- rbind(resultados, data.table(hyperparam = i, seed = semilla, ganancia = max_gan_suavizada, pos_optima = pos_optima))
    
  }
# Guarda la ganancia acumulada en un archivo CSV
ganame <- paste0('10.1.Top', i, '_', k, 'semillas_pred-Ganan.csv')
fwrite(tb_prediccion, 
       file = ganame, sep = ",")      
}

# Genera CSV con los resultados de única Gananancia para c/u de los
fwrite(resultados, file = '10.1.Resumen_Ganancia_por_modelo.csv', sep = ",")



