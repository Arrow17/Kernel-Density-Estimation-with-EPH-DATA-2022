# Encuesta Permanente de Hogares
# Distribución del ingreso de los hogares en aglomerados de mas de 500mil habitantes ###
# https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos
# Diccionario de variables: https://www.indec.gob.ar/ftp/cuadros/menusuperior/eph/EPH_registro_1T2021.pdf
# Datos del primer trimestre del 2021 

# Librerias

library(readxl)

# Gestionando la base de datos
dir <- file.choose()
eph <- read.table(file=dir, sep = ';', dec = '.',header = T)
dim(eph) # Muestra (nos interesan lad variables: "ITF" y "V1")
head(eph, 3) # Primeras 3 filas en los datos.
attach(eph)

# Distribucion del ingreso en miles de pesos de los individuos
hist(ITF/1000, xlab = 'Miles de Pesos', 
     main = 'Distrib. Ingreso Individual', axes = T)

mean(ITF, na.rm = T) # Estimación puntual del salario medio familiar en la población.
sd(ITF, na.rm = T)   # Estimación puntual del desvío estandard de los salario en la población.

ITF <- na.omit(ITF) # Quito algunos missings o datos faltantes

# Este reescalamiento nos ayuda a ver los posibles valores de h, ya que cuando
# los datos estan en media 0 y varianza 1, h esta en el intervalo 0 y 1, caso 
# contrario h puede tener distintos valores

ITFV <- scale(ITF) # Estandarizo los valores de ITF para que sea más fácil elegir "h"
ITFV

### Estimación no paramétrica de la densidad:

# Regla empirica ad hoc para elegir un ancho de banda de un estimador
# de densidad de kernell gaussiano
bw.nrd(x = ITFV) # h ≈1.06*n*hat.(sigma)/5.

# Estimacion no parametrica
non.par.dens = density(ITFV, bw = 0.10) 

par(mar = c(4,4,1,1))
hist(ITFV, main = 'Ingresos estandarizados', freq = F, 
     ylim = c(0,1), breaks = 100)
lines(non.par.dens$x,non.par.dens$y, col = 'blue', lty = 2 ,lwd = 2)

### Validación cruzada para hallar el h optimo considerando la funcion de riesgo a minimizar
bw.ucv(x = ITFV) 

# Se corre de nuevo el modelo con el h optimo 
non.par.dens.2 = density(ITFV, bw = 0.01363136)
hist(ITFV, main = 'Ingresos estandarizados', freq = F, 
     ylim = c(0,1), breaks = 100)
lines(non.par.dens.2$x,non.par.dens.2$y, col = 'red', lty = 2 ,lwd = 2)

# Si damos una mirada mas profunda a la distribucion de los ingresos
x11()
par(mar = c(4,4,1,1))
hist(ITFV, main = 'Ingresos estandarizados', freq = F, 
     ylim = c(0,1), breaks = 1000, xlim = c(-1,4))
lines(non.par.dens.2$x,non.par.dens.2$y, col = 'green', lty = 2 ,lwd = 2)


