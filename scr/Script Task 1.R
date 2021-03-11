# Elaborado por: Gabriela Navarro (201821568), Daniela Quintero (201821754) y Maria Alejandra Saavedra (201815221)
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
  
  #Para importar la base de datos se usa read_xlsx porque el archivo es de Excel
  cultivos=read_xlsx("data/input/cultivos.xlsx")
  view(cultivos) #para ver la base de datos 
  
  #Para crear una nueva base de datos donde se eligan las variables que se van a utilizar: Municipios y el resto de años. Además, se eligen desde la fila 5 porque el inicial son espacios nulos
  basecultivos = cultivos[5:nrow(cultivos),] %>% dplyr::select(.,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) 
  view (basecultivos)
  
  #Se cambian los nombres de las columnas para poder identificarlos al hacer el pivoteo
  colnames(basecultivos) = c("MUNICIPIO", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
  view (basecultivos)
  
  #Se descartan los espacios nulos
  basecultivos = subset(x = basecultivos, subset = is.na(MUNICIPIO) == F)
  view (basecultivos)
  
  #Ya se puede hacer pivoteo al tener los datos relevantes 
  basecultivos = melt(data = basecultivos,  id.vars=c("MUNICIPIO") , value.name = 'coca_hectarias')
  view (basecultivos)

  #Para que las columnas tengan los nombres correctos en la base final
  colnames(basecultivos)[2] = "Agno"
  view (basecultivos)
  
  #Para exportarla a output
  saveRDS(object = basecultivos, file = "data/output/basecultivos.rds")

#3. GEIH
  
  #3.1 Importar 

  # Para limpiar todo y tener la configuracion inicial
  rm(list = ls()) #Limpia el entorno de R
  pacman::p_load(here,tidyverse,reshape2) #Cargar y/o instalar paquetes a usar
  
  # Para importar la base de datos se usan readRDS porque están en formato R
  personas=readRDS("data/input/2019/Cabecera - Caracteristicas generales (Personas).rds")
  ocupados=readRDS("data/input/2019/Cabecera - Ocupados.rds")
  
  # Para verificar en la base de datos "personas" aquellos individuos que sean indentificadores unicos. Se usa "duplicated" y "table" para verlo en la consola
  duplicated(personas$directorio) %>% table()

  duplicated(paste0(personas$directorio,personas$secuencia_p)) %>% table()

  duplicated(paste0(personas$directorio,personas$secuencia_p,personas$orden)) %>% table() #No hay duplicados en X

  # Para verificar en ocupados que sean identificadores unicos. Se usa "duplicated" y "table" para verlo en la consola
  duplicated(ocupados$directorio) %>% table()

  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p)) %>% table()

  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p,ocupados$orden)) %>% table() # No hay duplicados en X
  
  #Para unir las bases con identificadores de directorio, secuencia_p y orden
  base=full_join(x=personas, y=ocupados, by=c("directorio", "secuencia_p", "orden"))
  base$ocupados=ifelse(is.na(base$mes.y), 0, 1)
  
  #3.2 Descriptivas
  
  base %>% group_by(P6020) %>% summarise(promedio=mean(ocupados)) 
  base %>% group_by(P6020) %>% summarise(num=sum(ocupados))
  
  ggplot() + geom_bar(data=base, aes(x=ocupados))
  h=subset(base, P6020==1)
  m=subset(base, P6020==2)
  ggplot() + geom_bar(data=m, aes(x=ocupados))
  ggplot() + geom_bar(data=h, aes(x=ocupados))
  subset(base, P6020==1) %>% ggplot() + geom_bar(data=base, aes(x=ocupados)) #me salen iguales wtf, salen diferentes como lo hice arriba
  subset(base, P6020==2) %>% ggplot() + geom_bar(data=base, aes(x=ocupados))
<<<<<<< HEAD

=======
  
  #Intento para categórica
  library(ggplot2)
  g1 = ggplot(data = base, mapping = aes(x=ocupados))
  g1 + geom_histogram(binwidth = 0.5, fill="blue", colour="white")
  g1+ geom_bar(data=base, aes(x=ocupados))
  g1+ geom_bar()+ labs(title = "o vs d", x= "Ocupados", y= "frecuencia")
  
  # Sexo P6020
  
>>>>>>> f1e76e03918f073eec26ba8cb73157f0382a48cc
  ggplot(data=base, aes(x=P6040)) + geom_bar() #tal vez este no hacerlo con barras sino otra que no se vea tan raro
  ggplot(data=base, aes(x=P6440)) + geom_bar()


