# Elaborado por: Gabriela Navarro (201821568)
# Colaboradores: Daniela Quintero (201821754) y Maria Alejandra Saavedra (201815221)
# Fecha de elaboracion: 01/03/21
# Fecha ultima modificación: 

#SCRIPT TASK 1 - TALLER A 

#1. VECTORES

  # Vector 1 que contiene los números del 1 al 100
  vector1=c(1:100) 

  # Vector 2 que contiene numeros impares del 1 al 99
  vector2=vector1[vector1%%2==!0] #Se usa vector1[] para seleccionar los numeros de este vector. Luego se usa el argumento vector1%%2==!0, en donde %% nos permite obtener el residuo de una division. En este caso,que cada elemento del vector 1 se divida por 2, aquellos que tengan un residuo igual a 0 significa que son pares y los que tengan un residuo diferentes (==!) a 0 son impares

  # Vector 3 que contiene numeros pares del vector 1 
  vector3=vector1[-vector2] #Se usa vector1[] para seleccionar elementos de ese vector, luego se usa como argumento -vector2, pues al tener el signo negativo R devuelve todos los numeros del vector1 sin contar los del vector2

#2. LIMPIAR BASE DE DATOS

  #Para limpiar todo y tener la configuracion inicial
  rm(list = ls()) #Limpia el entorno de R
  pacman::p_load(here,tidyverse,reshape2) #Cargar y/o instalar paquetes requeridos
  library(readxl) #Para traer paquete específico que leer el archivo de Excel
  
  #Para importar la base de datos se usa readxl porque el archivo es de Excel
  cultivos=read_excel("data/input/cultivos.xlsx")
  view(cultivos) #para ver la base de datos 
  
  #Para crear una nueva base de datos donde se eligan las variables que se van a utilizar: Municipios y el resto de años. Además, se eligen desde la fila 5 porque el inicial son espacios nulos
  basecultivos= cultivos[5:nrow(cultivos),] %>% dplyr::select(.,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) 
  #se cambian los nombres de las columnas para poder identificarlos al hacer el pivoteo
  colnames(basecultivos)=c("MUNICIPIO", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
  #se descartan los espacios nulos
  intento1= subset(x = basecultivos, subset = is.na(MUNICIPIO) == F)
  
  #Para hacer el pivoteo se usa la nueva base pues es la que tiene los datos relevantes 
  intento2=melt(data = intento1,  id.vars=c("MUNICIPIO") , value.name = 'coca_hectarias')
  #¿Tienen que ser bajo municipios o departamentos o solo el TOTAL?


#3. GEIH
  
  #Para limpiar todo y tener la configuracion inicial
  rm(list = ls()) # limpia el entorno de R
  pacman::p_load(here,tidyverse,reshape2) # cargar y/o instalar paquetes a usar
  
  #Para importar la base de datos se usan readRDS porque están en formato R
  personas=readRDS("data/input/2019/Cabecera - Caracteristicas generales (Personas).rds")
  ocupados=readRDS("data/input/2019/Cabecera - Ocupados.rds")
  
  #Para verificar en personas que sean indentificadores unicos se usa el duplicated y table para verlo en la consola
  duplicated(personas$directorio) %>% table()

  duplicated(paste0(personas$directorio,personas$secuencia_p)) %>% table()

  duplicated(paste0(personas$directorio,personas$secuencia_p,personas$orden)) %>% table() # No hay duplicados en X

  #Para verificar en ocupados que sean identificadores unicos se usa el duplicated y table para verlo en la consola
  duplicated(ocupados$directorio) %>% table()

  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p)) %>% table()

  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p,ocupados$orden)) %>% table() # No hay duplicados en X
  #Para unir ya las bases con identificadores de directorio, secuencia_p y orden
  base=full_join(x=personas, y=ocupados, by=c("directorio", "secuencia_p", "orden"))
  base$ocupados=ifelse(is.na(base$mes.y), 0, 1)
  #Descriptores
  base %>% group_by(P6020) %>% summarize(promedio=mean(base$ocupados)) #Me estan saliendo iguales help
  ggplot(data=base, aes(x=P6020)) + geom_bar()


