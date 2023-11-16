# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


require("data.table")


#---INICIO: Carga dataset y verifica--------------------------------------------------------  
#Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("D:\\OneDrive\\! DM en Econ y Fin 2023") 

#cargo el dataset
dataset <- fread("./#Experimentos Colaborativos/02.dataset_base.csv.gz") 

# minima exploración por fecha y target
table(dataset$foto_mes, dataset$clase_ternaria )

#SACAR campos vinculados a la clase adicionales:
cols_finales <- setdiff( colnames(dataset), c("add_last_mes", "add_mesn",   "add_lastn", "clase_gral_n", "clase_gral"))
dataset <- dataset[, ..cols_finales]


#---SELECCION VARIABLES LAGUEABLES-------------------------------------------------------- 

#estas son las columnas a las que se puede agregar lags 
campos_a_quitar  <- colnames(dataset)
campos_a_quitar  <- campos_a_quitar[campos_a_quitar %like% "^(add|clase)"]
cols_lagueables  <- copy(  setdiff( colnames(dataset), c("numero_de_cliente", "foto_mes", campos_a_quitar)  ) ) #152


#-----PROCESO: Applico Funciones para: +FE y fix_Drifting ------------------------------------------------------
#ordeno el dataset para aplicar luego los fixes
setorder( dataset, foto_mes, numero_de_cliente )

#FUNCION2: Genera LAGs 1, 3 Y 6

#LAGs Individuales:
lag1 <- TRUE

lag3 <- TRUE

lag6 <- TRUE

dim(dataset) # 2549133     155


if (lag1) {
  dataset[ , paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  }

if (lag3) {
  dataset[ , paste0(cols_lagueables, "_lag3") := shift(.SD, 3, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  }

if (lag6) {
  dataset[ , paste0(cols_lagueables, "_lag6") := shift(.SD, 6, NA, "lag"), 
           by = numero_de_cliente, 
           .SDcols = cols_lagueables ]
  }


dim(dataset)#2549133     611

155+152*3 #611

#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("D:/OneDrive/! DM en Econ y Fin 2023/#Experimentos Colaborativos/")

fwrite(dataset,
       file = "06.0.Baseline.csv.gz",
       sep = ",")

