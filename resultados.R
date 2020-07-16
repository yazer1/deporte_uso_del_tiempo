
# Cargar base de datos
enut<-readRDS("enut-editada.rds")


#---- 1. RESULTADOS DE MUESTRA ----

#resultado N. 1
#frecuencias simple y de %
table(enut$sati_tmp_recod)
table(enut$sati_tmp_recod, enut$sexoRec)
prop.table(table(enut$sati_tmp_recod, enut$sexoRec),1)*100

tabla1<-ctable(enut$sati_tmp_recod, enut$sexoRec, style = 'rmarkdown', headings = F)

tabla2<-freq(enut$sati_tmp_recod, weights = enut$pond, style = 'rmarkdown')

#posicion sobre satisfacion del tiempo en practica de ocio o pasa tiempo segun edad, perfil columna  
tabla3<-ctable(enut$sati_tmp_recod, enut$sexoRec, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

tabla4<-ctable(enut$sati_tmp_recod, enut$edadrecod, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

#resultado N. 2
#frecuencias simple y de %
table(enut$Diasemrec)
table(enut$Diasemrec, enut$sexoRec, enut$sati_tmp_recod,)
prop.table(table(enut$Diasemrec, enut$sexoRec, enut$sati_tmp_recod),1)*100

tabla5<-freq(enut$Diasemrec, weights = enut$pond, style = 'rmarkdown')

tabla6<-ctable(enut$Diasemrec, enut$sexoRec, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

tabla7<-ctable(enut$Diasemrec, enut$edadrecod, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

#Resultado N.3
#frecuencias simple y de %
table(enut$Fdiasemrec)
table(enut$Fdiasemrec, enut$sexoRec, enut$sati_tmp_recod)
prop.table(table(enut$Fdiasemrec, enut$sexoRec, enut$sati_tmp_recod),1)*100

tabla8<-freq(enut$Fdiasemrec, weights = enut$pond, style = 'rmarkdown')

tabla9<-ctable(enut$Fdiasemrec, enut$sexoRec, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

tabla10<-ctable(enut$Fdiasemrec, enut$edadrecod, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

#Resultado N. 4
#frecuencias simple y de %
table(enut$Fdpsemrec)
table(enut$Fdpsemrec, enut$sexoRec, enut$sati_tmp_recod)
prop.table(table(enut$Fdpsemrec, enut$sexoRec, enut$sati_tmp_recod),1)*100

tabla11<-freq(enut$Fdpsemrec, weights = enut$pond, style = 'rmarkdown')

tabla12<-ctable(enut$Fdpsemrec, enut$sexoRec, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

tabla13<-ctable(enut$Fdpsemrec, enut$edadrecod, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

#Resultado N. 5
#frecuencias simple y de %
table(enut$Pdepfsemrec)
table(enut$Pdepfsemrec, enut$edadrecod, enut$sati_tmp_recod)
prop.table(table(enut$Pdepfsemrec, enut$sexoRec),1)*100

tabla14<-freq(enut$Pdepfsemrec, weights = enut$pond, style = 'rmarkdown')

tabla15<-ctable(enut$Pdepfsemrec, enut$edadrecod, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)

tabla16<-ctable(enut$Pdepfsemrec, enut$sexoRec, prop = "c",
       weights = enut$pond, style = 'rmarkdown', headings = F)


table(enut$sati_tmp_recod, enut$edadrecod)

table(enut$Diasemrec, enut$edadrecod)

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

#lista de resultados
resultados <- list(tabla1, tabla2, tabla3, tabla4, tabla5, tabla6, tabla7, tabla8,
                   tabla9, tabla10, tabla11, tabla12, tabla13, tabla14, tabla15, tabla16)

saveRDS(resultados, file = "resultados-enut.rds") 

# Limpiar entorno de trabajo
rm(list=ls())