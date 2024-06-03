# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("C:/Users/franc/Desktop/ITBA/00DataMining/") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")
#dataset <- dataset[, !("ctrx_quarter"), with = FALSE]
dtrain <- dataset[foto_mes == 202107]# defino donde voy a entrenar
dtrain[,ccomisiones_otras_rank:=frankv(ccomisiones_otras)]
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ . - ccomisiones_otras",
        data = dtrain, 
        xval = 0,
        cp = -0.945696099480609, 
        minsplit = 702, 
        minbucket = 299, 
        maxdepth = 6
) 


# grafico el arbol
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)


# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./exp/")
#dir.create("./exp/KA2002")

fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2002/K101_prueba_eliminar_variable.csv",
        sep = ","
)

importancia <- modelo$variable.importance
importancia <- round(importancia, digits = 5)

# Ordenar la importancia de variables y seleccionar las 10 primeras
importancia <- sort(importancia, decreasing = TRUE)
importancia_top10 <- head(importancia, 10)

print("Importancia de las variables (Top 10):")
print(importancia_top10)

# si quieres graficar la importancia de las variables (Top 10)
barplot(importancia_top10, main = "Importancia de las Variables (Top 10)",
        col = "blue", las = 2, cex.names = 0.7)

dataset$ccomisiones_otras_rank
#- Visa_msaldodolares - Master_fultimo_cierre - Master_madelantodolares - Visa_mfinanciacion_limite - minversion2 - ccajeros_propios_descuentos - ctarjeta_visa_descuentos - ctarjeta_master_descuentos - mforex_buy - catm_trx - Master_Finiciomora - Visa_mconsumosdolares