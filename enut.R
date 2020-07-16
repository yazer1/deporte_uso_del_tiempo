#ELECTIVO CIENCIA ABIERTA Y SOFTWAR PARA CIENCIAS SOCIALES.
#MAGISTER CS MENCION SOCIOLOGIA DE LA MODERNIZACION
#POR: YAZER PRIMERA NADER
#UNIVERSIDAD DE CHILE 2020

# QUE VEREMOS: se mostraran la aplicacion de lo aprendido en clases para cargar base de datos,
# recodificar variables, seleccionar variables, entre otras mas.
# objetivo primordial: lograr aplicar los codigos en un Rscript sin ningun error
 
#---- 1. INICIO DE TRABAJO USO DEL TIEMPO Y DEPORTE----
# para iniciar debemos cargar los paquetes atulizar.

# ipak te permite instalar, llamar y cargar multiples paquetes sin hacerlo varias veces.
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# seleccionamos los paquetes a utilizar
packages <- c("ggplot2", "haven", "readr", "dplyr",
              "tidyverse", "summarytools")
ipak(packages)


#acontinuacion para codificar idiomas, tildes, acentos o eñes,
#debes ir a File --> Reopen with Encoding --> Seleccionar UTF-8

#Primero debes definir carpeta de trabajo, puede ser la misma carpeta que subirás a Github, creando adentro un RProject
setwd("D:/Nueva carpeta (3)/githud/trabajo/Data/InputData/DataSources")

#---- 2. IMPORTAR LOS DATOS A RSTUDIO----

# Leer base desde CSV
enut <- read.csv2("BASE_USUARIO_corregida.xlsx.csv")

# Guardarla en formato R en carpeta de trabajo
saveRDS(enut, file = "enut.rds")

# Leer base desde carpeta de trabajo
enut <- readRDS("enut.rds")

#---- 3. RECODIFICACION, SELECCION Y TRANSFORMACION DE VARIABLES----
# Análisis global de los datos
class(enut) # aparecera la clase de objeto
dim(enut) # aparecera las dimensiones de la matriz
names(enut) # apareceran los nombres de las variables

#se seleccionaran las variables, para asi tener una limpieza y mejor entorno de tranajo.

enut <- select(enut,region,edad,c13_1_1,tiempo,dia_semana,
               dia_fin_semana,t11_1_7,s41_1_1,s41_1_2,s41_2_1,s41_2_2)

#hacemos el cambio de nombre de las variables para mejor el entorno

enut <- rename(enut, sexo = c13_1_1, inf_tiempo = tiempo, fre_dep_semana = s41_1_1,
               tmp_dep_semana = s41_1_2, prac_dep_fsemana = s41_2_1, tmp_dep_fsemana = s41_2_2,
               sati_tiempo = t11_1_7)

# a) hacemos la recodificacion de sexo

#para saber sobre la variable se hace lo siguiente
table(enut$sexo) #1 = hombre, 2 = mujer
class(enut$sexo) #que clase es.

#despues la convertimos a un vector numerico para poder transformarla
enut$sexo <- as.numeric(enut$sexo)

#acontinuacion se hace la recodificacion
enut <- mutate(enut, sexoRec = recode(enut$sexo, "1" = "hombre",
                                        "2" = "mujer"))

class(enut$sexoRec) #veremos que queda como un objeto character
table(enut$sexoRec) #veremos el cambio

## b) recodificamos variable edad

range(enut$edad)
range(enut$edad, na.rm = T)

#para saber sobre la variable se hace lo siguiente
summary(enut$edad)
table(enut$edad) 
class(enut$edad) 

#despues la convertimos a un vector numerico para poder transformarla
enut$edad <- as.numeric(enut$edad)

#acontinuacion se hace la recodificacion
enut <- mutate(enut, edadrecod = recode(enut$edad, "1" = "adolecentes", "2" = "jovenes", "3" = "adultos",
                                        "4" = "adulto_mayor"))
table(enut$edadrecod)


## c) recodificamos variable satisfacion del tiempo en pratica de ocio o pasa tiempo.

#1. insatisfecho/ 2.insatisfecho  / 3. ni satisfecho, ni insatisfecho / 4. satisfecho / 5. totalmente satisfecho / 85. no aplica
# Recodificar en incomplacido, complacido y totalmente complacido
#para saber sobre la variable se hace asi.
table(enut$sati_tiempo)
class(enut$sati_tiempo)

#acontinuacion la convertimos en un vector numerico para poder transformarla
enut$sati_tiempo <- as.numeric(enut$sati_tiempo)

 
#para poder recodificar según tramos se hace asi
enut <- mutate(enut, sati_tmp_recod = car::recode(enut$sati_tiempo, "1:2 = 1; 3 = 2;
                                                  4:5 = 3; else = NA"))

table(enut$sati_tmp_recod)

#Convertir a factor para poner etiquetas
enut$sati_tmp_recod <- factor(enut$sati_tmp_recod, labels= c("incomplacido", "complacido",
                                                             "totalmente complacido"))

table(enut$sati_tmp_recod)


### BREVE ANALISIS DE MUESTRA.
table(enut$sexoRec, enut$sati_tmp_recod)

prop.table(table(enut$sexoRec, enut$sati_tmp_recod))*100

prop.table(table(enut$sexoRec, enut$sati_tmp_recod),1)*100

table(enut$edadrecod, enut$sati_tmp_recod)

prop.table(table(enut$edadrecod, enut$sati_tmp_recod))*100

prop.table(table(enut$edadrecod, enut$sati_tmp_recod),1)*100

freq(enut$sati_tmp_recod)

#---- RESULTADOS DE MUESTRA ----
#frecuencias simple y de %
table(enut$sati_tmp_recod)
table(enut$sexoRec, enut$sati_tmp_recod)
prop.table(table(enut$sexoRec, enut$sati_tmp_recod),1)*100

# RESULTADO 1:  tabla de doble entrada con "summarytools", sin ponderar
# Posicion sobre satisfacion del tiempo en pratica de ocio o pasa tiempo según sexo, perfil columna
ctable(enut$sexoRec, enut$sati_tmp_recod, prop = "c")
#posicion sobre satisfacion del tiempo en practica de ocio o pasa tiempo segun edad, perfil columna  
ctable(enut$edadrecod, enut$sati_tmp_recod, prop = "c")

# Estadísticos descriptivos variable edad
summary(enut$edad)

# RESULTADO A: estadísticos de resumen con "summarytools"
# Estadísticos de resumen variable edad
descr(enut$edad, transpose = T)
descr(enut$edad, transpose = T, 
      stats = c("min","q1","med","mean","q3","sd","iqr","cv","n.valid"))

# RESULTADO B: estadísticos resumen según una segunda variable
# Estadísticos de resumen variable edad según satisfacion del tiempo en practica de ocio o pasa tiempo, sin ponderar
with(enut, stby(data= edad, INDICES = sati_tmp_recod,
                        FUN = descr, 
                        stats = c("min","q1","med","mean","q3","sd","iqr","cv","n.valid"),
                        transpose = T))
