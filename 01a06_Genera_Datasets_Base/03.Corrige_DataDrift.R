# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")

#--DEFINICION DE FUNCIONES para corregir el Data-DRIFTING------------------------------------------------------------------------

#FUNCION1: FIX Drifting con "rank_cero_fijo" (los positivos se rankean por su lado y los negativos por otro)
drift_rank_cero_fijo  <- function( campos_drift , dataset)
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ get(campo) ==0, paste0("rank_",campo) := 0 ]
    dataset[ get(campo) > 0, paste0("rank_",campo) :=   frank(  get(campo))  / .N, by= foto_mes ] 
    dataset[ get(campo) < 0, paste0("rank_",campo) :=  -frank( -get(campo))  / .N, by= foto_mes ]
    dataset[ , (campo) := NULL ] 
  }
}

#FUNCION2: FIX Drifting con "rank_directo" (todos los valores recien un unico ranking
drift_rank_directo  <- function( campos_drift , dataset)
{
  for( campo in campos_drift )
  {
    cat( campo, " " )
    dataset[ , paste0("rankd_",campo) :=  (frank( get(campo), na.last = "keep") ) / .N, by= foto_mes]
    dataset[ , (campo) := NULL ] 
  }
}
  
#FUNCION3: FIX Drifting con "Scalado z-score" (Xi - avg / desvio)
drift_zscale  <- function( campos_drift, dataset )
  {
    for( campo in campos_drift )
    {
      cat( campo, " " )
      dataset[, paste0("scale_",campo) := scale(get(campo),center = TRUE, scale = TRUE), by= foto_mes]
      dataset[ , (campo) := NULL ]
    }
  } 


#---INICIO: Carga dataset y verifica--------------------------------------------------------  
#Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("D:\\OneDrive\\! DM en Econ y Fin 2023") 

#cargo el dataset
dataset <- fread("./#Experimentos Colaborativos/02.dataset_base.csv.gz") 

#ordeno el dataset para aplicar luego los fixes
setorder( dataset, foto_mes, numero_de_cliente )

# minima exploración por fecha y target
table(dataset$foto_mes, dataset$clase_ternaria )

#--SELECCION COLUMNAS A CORREGIR------------------------------------------------------------------------
#1 VARIABLES MONETARIAS:
campos_monetarios  <- colnames(dataset) 
campos_monetarios  <- campos_monetarios[campos_monetarios %like% "^(m|Visa_m|Master_m|vm_m)"] # 73 variables / 155 = 47.1%

#--APLICA 3 CORRECCIONES: Rank_cero_fijo | Rank_directo | Estandarización_zscore ------------------------------------------------------------------------
#1) Rank_cero_fijo: Hago un rank cero fijo a las variables monetarias
dataset_rank <- copy(dataset)

drift_rank_cero_fijo( campos_monetarios , dataset_rank )

colnames(dataset_rank) 


#2) Rank_directo: Hago un rank comun a las variables monetarias
dataset_rankdir <- copy(dataset)

drift_rank_directo( campos_monetarios , dataset_rankdir)

colnames(dataset_rankdir) 


#3) Estandarización z-score: Hago un escalado a las variables monetarias
dataset_scale <- copy(dataset)

drift_zscale( campos_monetarios , dataset_scale )

colnames(dataset_scale ) 



#---EXPORTA DATABSE GENERADA A .CSV---------
setwd("D:/OneDrive/! DM en Econ y Fin 2023/#Experimentos Colaborativos/")

#EXPORTAMOS a un .CSV
fwrite( dataset_rank,
        file= "04.1.dataset_rankcerofijo.csv.gz",
        sep= "," )

fwrite( dataset_rankdir,
        file= "04.2.dataset_rankdirecto.csv.gz",
        sep= "," )      

fwrite( dataset_scale,
        file= "04.3.dataset_scale.csv.gz",
        sep= "," )


#------verifica  ratios de NA --------
#Original:
resultados <- list()
for (campo in campos_monetarios) {
  tbl <- dataset[
    ,
    list("na_ratio" = sum(is.na(get(campo)), na.rm = TRUE) / .N),
    foto_mes
  ]
  resultados[[campo]] <- tbl
}

#Rank_cero_fijo:
resultados2 <- list()
campos_rank <- paste0("rank_", campos_monetarios)  
for (campo in campos_rank) {
  tbl2 <- dataset_rank[
    ,
    list("na_ratio" = sum(is.na(get(campo)), na.rm = TRUE) / .N),
    foto_mes
  ]
  resultados2[[campo]] <- tbl2
}
  
#Rank:
resultados3 <- list()
campos_rankf <- paste0("rankd_", campos_monetarios)  
for (campo in campos_rankf) {
  tbl3 <- dataset_rankdir[
    ,
    list("na_ratio" = sum(is.na(get(campo)), na.rm = TRUE) / .N),
    foto_mes
  ]
  resultados3[[campo]] <- tbl3
}

#Scale:
resultados4 <- list()
campos_scale <- paste0("scale_", campos_monetarios)  
for (campo in campos_scale) {
  tbl4 <- dataset_scale[
    ,
    list("na_ratio" = sum(is.na(get(campo)), na.rm = TRUE) / .N),
    foto_mes
  ]
  resultados4[[campo]] <- tbl4
}

#Veo los resultados:
resultados <- rbindlist(resultados, use.names = TRUE, fill = TRUE, idcol = "variable")
resultados <- dcast(resultados, foto_mes ~ variable, value.var = "na_ratio")

resultados2 <- rbindlist(resultados2, use.names = TRUE, fill = TRUE, idcol = "variable")
resultados2 <- dcast(resultados2, foto_mes ~ variable, value.var = "na_ratio")

resultados3 <- rbindlist(resultados3, use.names = TRUE, fill = TRUE, idcol = "variable")
resultados3 <- dcast(resultados3, foto_mes ~ variable, value.var = "na_ratio")

resultados4 <- rbindlist(resultados4, use.names = TRUE, fill = TRUE, idcol = "variable")
resultados4 <- dcast(resultados4, foto_mes ~ variable, value.var = "na_ratio")
