
# Si no te lee acentos o eñes, debes ir a File --> Reopen with Encoding --> Seleccionar UTF-8

#Primero debes definir carpeta de trabajo, puede ser la misma carpeta que subirás a Github, creando adentro un RProject

# Leer base desde CSV
enut <- read.csv2("BASE_USUARIO_corregida.xlsx.csv")

# Guardarla en formato R en carpeta de trabajo
saveRDS(enut, file = "enut.rds")

# Leer base desde carpeta de trabajo
enut <- readRDS("enut.rds")
