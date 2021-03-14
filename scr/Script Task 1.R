# Elaborado por: Gabriela Navarro (201821568), Daniela Quintero (201821754) y Maria Alejandra Saavedra (201815221)
# Fecha de elaboracion: 01/03/21
# Fecha ultima modificacion: 13/03/21

#SCRIPT TASK 1 - TALLER A 

#1. VECTORES

  # Vector 1 que contiene los numeros del 1 al 100
  vector1 = c (1:100) 

  # Vector 2 que contiene numeros impares del 1 al 99
  vector2 = seq (from=1, to=99, by=2 ) #Se establece una secuencia donde empieza por el numero 1 y termina en el 99 con saltos de dos numeros para tener solo impares

  # Vector 3 que contiene numeros pares del vector 1 
  vector3 = vector1 [-vector2] #Se usa vector1[] para seleccionar elementos de ese vector, luego se usa como argumento -vector2, pues al tener el signo negativo R devuelve todos los numeros del vector1 sin contar los del vector2

#2. LIMPIAR BASE DE DATOS

  #Para limpiar todo y tener la configuracion inicial
  rm(list = ls()) #Limpia el entorno de R
  pacman::p_load(here,tidyverse,reshape2) #Cargar y/o instalar paquetes requeridos
  
  #Para importar la base de datos se usa read_xlsx porque el archivo es de Excel
  cultivos = read_xlsx("data/input/cultivos.xlsx")
  view (cultivos) #para ver la base de datos 
  
  #Para crear una nueva base de datos donde se eligan las variables que se van a utilizar: Municipios y el resto de años. Además, se eligen desde la fila 5 porque el inicial son espacios nulos
  basecultivos = cultivos[5:nrow(cultivos),] %>% dplyr::select(.,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) 
  view (basecultivos)
  
  #Se cambian los nombres de las columnas para poder identificarlos al hacer el pivoteo
  colnames (basecultivos) = c("MUNICIPIO", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
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
  
  # Para unir las bases con identificadores de directorio, secuencia_p y orden
  base=full_join(x=personas, y=ocupados, by=c("directorio", "secuencia_p", "orden"))
  base$ocupados=ifelse(is.na(base$mes.y), 0, 1) #Para crear una nueva variable de ocupados. Esto se debe a que en la union los que no estan ocupados tienen NA
  
  # Para exportar la base de datos final que queda antes de hacer las descriptivas
  saveRDS(object=base, file = "data/output/baseocupados.rds")
  
  #3.2 Descriptivas
  
  summary(base)
  
  # Estadísticas descriptivas número de ocupados y desocupados con diferentes variables de agrupacion 
  
    #Agrupado por genero
    base %>% group_by(P6020) %>% summarise(promedio=mean(ocupados), desvest=sd(ocupados), var=var(ocupados), total=sum(ocupados))

    #Agrupado por tipo de contrato  
    base %>% group_by(P6430) %>% summarise(promedio=mean(ocupados), desvest=sd(ocupados), var=var(ocupados), total=sum(ocupados))

    #Agrupado por edad 
    base %>% group_by(P6040) %>% summarise(promedio=mean(ocupados), desvest=sd(ocupados), var=var(ocupados), total=sum(ocupados))

    #Agrupado por urbano/rural
    base %>% group_by(area.x) %>% summarise(promedio=mean(ocupados), desvest=sd(ocupados), var=var(ocupados), total=sum(ocupados))
    
  # Estadísticas descriptivas ingresos laborales promedio con diferentes variables de agrupacion 
    descript = subset(base,is.na(base$P6750) == F) #para sacar los NA de honorarios netos 
    descript2= subset(base, is.na(base$P6500) == F) #Para sacar los NA de ingresos mensuales - Al usar ambas variables de ingreso se consiguen los datos de todos los tipos de contratos
    
    #  #Agrupado por genero
    descript %>% group_by(P6020) %>% summarise(promedio = mean(P6750), desvest= sd(P6750), var = var(P6750), min = min(P6750), max = max(P6750))
    
    #Agrupado por tipo de contrato  
    descript %>% group_by(P6430) %>% summarise(promedio = mean(P6750), desvest= sd(P6750), var = var(P6750), min = min(P6750), max = max(P6750))
    descript2 %>% group_by(P6430) %>% summarise(promedio = mean(P6500), desvest= sd(P6500), var = var(P6500), min = min(P6500), max = max(P6500))
    
    #Agrupado por edad 
    descript %>% group_by(P6040) %>% summarise(promedio = mean(P6750), desvest= sd(P6750), var = var(P6750), min = min(P6750), max = max(P6750))
    
    #Agrupado por urbano/rural
    descript %>% group_by(area.x) %>% summarise(promedio = mean(P6750), desvest= sd(P6750), var = var(P6750), min = min(P6750), max = max(P6750))

  # Realizamos los graficos adecuadas para cada caso (con ayuda de: https://www.data-to-viz.com) y las exportamos
  
    #Gráfica de ocupados 
    library(ggplot2)
    g1=ggplot(data=base, aes(x= as.factor(ocupados), fill=as.factor(ocupados)))
    g1
    ggsave(plot = g1, file = "views/Grafica de Ocupados.jpeg")
    
    g12=g1 + geom_bar() + scale_fill_hue(c=40) + theme(legend.position="none")+labs(title="Ocupados y Desocupados", subtitle = "2019", x="Desocupados vs. Ocupados")
    g12
    ggsave(plot = g12, file = "views/Grafica de Ocupados 2.jpeg")
    
      
    #Gráfica edad 
    g21=ggplot(data=base, aes(x=P6040)) + geom_bar()
    g21
    ggsave(plot = g21, file = "views/Grafica de Edades.jpeg")
    
    
    g22=base %>% filter() %>% ggplot(aes(x=P6040))+geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
    g22
    ggsave(plot = g22, file = "views/Distribucion de Edades.jpeg")
    
  
    # GRAFICA PARA OCUPADOS CON DIFERENTES VARIABLES DE AGRUPACION
    #Grafica para ocupados por sexo 
    g3=base %>% group_by(P6020) %>% summarize(total=sum(ocupados)) %>% ggplot(data=., aes(x=P6020, y=total))+
      geom_bar(stat = "identity", fill="#f68060", alpha=.6, width = 0.8)+
      coord_flip()+xlab('sexo')+ylab('Cantidad de Personas ocupadas')+theme_bw()
    g3
    ggsave(plot = g3, file = "views/Grafica Ocupados por Sexo.jpeg")
    

    #Grafica para ocupados por edad
    g4=base %>% group_by(P6040) %>% summarize(total=sum(ocupados)) %>% ggplot(data=., aes(x=P6040, y=total))+
      geom_bar(stat = "identity", fill="#f20060", alpha=.6, width = 0.8)+
      xlab('Edad')+ylab('Cantidad de Personas ocupadas')+theme_bw()
    g4
    ggsave(plot = g4, file = "views/Grafica Ocupados por Edad.jpeg")
    
    
    #Grafica ocupados por tipo de contrato 
    g5=base %>% group_by(P6430) %>% summarize(total=sum(ocupados)) %>% ggplot(data=., aes(x=P6430, y=total))+
      geom_bar(stat = "identity", fill="#f70060", alpha=.6, width = 0.8)+
      xlab('Tipo de Contrato')+ylab('Cantidad de Personas ocupadas')+theme_bw()
    g5
    ggsave(plot = g5, file = "views/Grafica Ocupados por Tipo de Contrato.jpeg")
    
  
    #Grafica ocupados por area
    g6= base %>% group_by(area.x) %>% summarise(total=sum(ocupados)) %>% ggplot(data=., aes(x=total))+
      geom_density(fill="#59f3a2", color="#e8ecef", alpha=0.8)+
      xlab('Tipo de Contrato')
    g6
    ggsave(plot = g6, file = "views/Distribucion Ocupados por areax.jpeg")
    
    g61= base %>% group_by(area.x) %>% summarise(total=sum(ocupados)) %>% ggplot(data=., aes(x=total))+
      geom_histogram(binwidth = 30, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
      xlab('Tipo de Contrato')
    g61
    ggsave(plot = g61, file = "views/Grafica Ocupados por areax.jpeg")
    
    g62=base %>% group_by(area.y) %>% summarise(total=sum(ocupados)) %>% ggplot(data=., aes(x=total))+
      geom_density(fill="#59f3a2", color="#e8ecef", alpha=0.8)+
      xlab('Tipo de Contrato')
    g62
    ggsave(plot = g62, file = "views/Distribucion Ocupados por areay.jpeg")
    
    g63=base %>% group_by(area.y) %>% summarise(total=sum(ocupados)) %>% ggplot(data=., aes(x=total))+
      geom_histogram(binwidth = 30, fill="#69b3a2", color="#e9ecef", alpha=0.9)+
      xlab('Tipo de Contrato')
    g63
    ggsave(plot = g1, file = "views/Grafica Ocupados por areay.jpeg")
  
    # GRAFICA PARA INGRESO PROMEDIO CON DIFERENTES VARIABLES DE AGRUPACION
    #Grafica para ingresos promedio por sexo
    descript %>% group_by(P6020) %>% summarise(promedio = mean(P6750), desvest= sd(P6750), var = var(P6750), min = min(P6750), max = max(P6750))
    
    g7= descript %>% group_by(P6020) %>% summarise(promedio=mean(P6750)) %>% ggplot(data=., aes(x=P6020, y=promedio))+
      geom_bar(stat = "identity", fill="#f68020", alpha=.6, width = 0.8)+
      coord_flip()+xlab('sexo')+ylab('Ingreso Promedio')+theme_bw()
    g7
    ggsave(plot = g7, file = "views/Grafica de Ingreso Promedio por Sexo.jpeg")
  
    #Grafica para ingreso promedio por edad
    g8 = descript %>% group_by(P6040) %>% summarise(promedio=mean(P6750)) %>% ggplot(data=., aes(x=P6040, y=promedio))+
      geom_bar(stat = "identity", fill="#f68050", alpha=.6, width = 0.8)+
      coord_flip()+xlab('Edad')+ylab('Ingreso Promedio')+theme_bw()
    g8
    ggsave(plot = g8, file = "views/Grafica de Ingreso Promedio por Edad.jpeg")
    
    #Grafica para ingreso promedio por tipo de contrato 
    g9= descript %>% group_by(P6430) %>% summarize(promedio=mean(P6750)) %>% ggplot(data=., aes(x=P6430, y=promedio))+
      geom_bar(stat = "identity", fill="#f10000", alpha=.6, width = 0.8)+
      xlab('Tipo de Contrato')+ylab('Ingreso promedio')+theme_bw()
    g9 #Para el tipo de contrato 4, 5 y 9
    ggsave(plot = g9, file = "views/Grafica de Ingreso Promedio por Tipo de Contrato.jpeg")
    
    g91= descript2 %>% group_by(P6430) %>% summarize(promedio=mean(P6500)) %>% ggplot(data=., aes(x=P6430, y=promedio))+
      geom_bar(stat = "identity", fill="#f58029", alpha=.6, width = 0.8)+
      xlab('Tipo de Contrato')+ylab('Ingreso promedio')+theme_bw()
    g91 #Para el tipo de contrato 1, 2, 3 y 8
    ggsave(plot = g91, file = "views/Grafica de Ingreso Promedio por Tipo de Contrato 2.jpeg")
    
    
    #Grafica para ingreso promedio por area
    g1.0= descript %>% group_by(area.x) %>% summarise(promedio=mean(P6750)) %>% ggplot(data=., aes(x=promedio))+
      geom_density(fill="#59f3a2", color="#e8ecef", alpha=0.8) + xlab('Ingreso promedio')
    g1.0
    ggsave(plot = g1.0, file = "views/Grafica de Ingreso Promedio por Areax.jpeg")
    
    
    g1.2=descript %>% group_by(area.y) %>% summarise(promedio=mean(P6750)) %>% ggplot(data=., aes(x=promedio))+
      geom_density(fill="#59f3a2", color="#e8ecef", alpha=0.8) + xlab('Ingreso promedio')
    g1.2
    ggsave(plot = g1.2, file = "views/Grafica de Ingreso Promedio por Areay.jpeg")



