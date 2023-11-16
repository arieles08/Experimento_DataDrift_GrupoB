# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection


# cargo las librerias que necesito
require("data.table")

# Aqui se debe poner la carpeta de la materia de SU computadora local (el Working Directory)
setwd("D:\\OneDrive\\! DM en Econ y Fin 2023") 

#cargo el dataset
dataset <- fread("./datasets/competencia_03.csv.gz") 

dim(dataset)


#Selecciono los meses que se van a usar:
dataset <- dataset[ foto_mes >= 202004 & foto_mes <= 202107,]

table(dataset$foto_mes)

#Exporto------------------------------------------------------
setwd("D:/OneDrive/! DM en Econ y Fin 2023/#Experimentos Colaborativos/")

fwrite(dataset,
       file = "02.dataset_base.csv.gz",
       sep = ",")



      

