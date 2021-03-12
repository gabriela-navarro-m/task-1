# Elaborado por: Gabriela Navarro (201821568), Daniela Quintero (201821754) y Maria Alejandra Saavedra (201815221)
# Fecha de elaboracion: 01/03/21
# Fecha ultima modificación: 

#SCRIPT TASK 1 - TALLER A 

#1. VECTORES

  # Vector 1 que contiene los números del 1 al 100
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
  base$ocupados=ifelse(is.na(base$mes.y), 0, 1) #Para crear una nueva variable de ocupados. Esto se debe a que en la union los que no estan ocupados tienen NA cuando deberia ser 0 para mostrar que son desocupados
  
  # Para exportar la base de datos final que queda antes de hacer las descriptivas
  saveRDS(object=base, file = "data/output/baseocupados.rds")
  
  #3.2 Descriptivas
  
  summary(base)
  
  # Estadísticas descriptivas número de ocupados y desocupados con diferentes variables de agrupacion 
  
    #Agrupado por genero
    base %>% group_by(P6020) %>% summarise(promedio=mean(ocupados), desvest=sd(ocupados), var=var(ocupados), total=sum(ocupados))

    #Agrupado por tipo de contrato  
    base %>% group_by(P6430) %>% summarise(promedio=mean(ocupados), num=sum(ocupados), total=sum(ocupados))

    #Agrupado por edad 
    base %>% group_by(P6040) %>% summarise(promedio=mean(ocupados), num=sum(ocupados), var=var(ocupados))

    #Agrupado por urbano/rural
    base %>% group_by(area.x) %>% summarise(promedio=mean(ocupados), num=sum(ocupados), var=var(ocupados), total=sum(ocupados))
    

  # Realizamos los graficos adecuadas para cada caso (con ayuda de: https://www.data-to-viz.com) y las exportamos
  
    #Gráfica de ocupados 
    library(ggplot2)
    g1=ggplot(data=base, aes(x= as.factor(ocupados), fill=as.factor(ocupados)))
    g12=g1 + geom_bar() + scale_fill_hue(c=40) + theme(legend.position="none")+labs(title="Ocupados y Desocupados", subtitle = "2019", x="Desocupados vs. Ocupados")
    g12
      
    #Gráfica edad 
    g21=ggplot(data=base, aes(x=P6040)) + geom_bar()
    g21
    g22=base %>% filter() %>% ggplot(aes(x=P6040))+geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)
    g22
  
    #Grafica para ocupados por genero 
    g3=base %>% group_by(P6020) %>% summarize(total=sum(ocupados)) %>% ggplot(data=., aes(x=P6020, y=total))+
      geom_bar(stat = "identity", fill="#f68060", alpha=.6, width = 0.8)+
      coord_flip()+xlab('sexo')+ylab('Cantidad de Personas ocupadas')+theme_bw()
    g3

    #Grafica para ocupados por edad
    g4=base %>% group_by(P6040) %>% summarize(total=sum(ocupados)) %>% ggplot(data=., aes(x=P6040, y=total))+
      geom_bar(stat = "identity", fill="#f20060", alpha=.6, width = 0.8)+
      xlab('Edad')+ylab('Cantidad de Personas ocupadas')+theme_bw()
    g4
    
    #Grafica ocupados por departamento 
    
    
    
   
  ##########
  ggplot(data=base, aes(x=P6430)) + geom_bar()
  
  g2= ggplot(data = base, mapping = aes(x=area.x))
  g2 + geom_histogram(binwidth = 0.5, fill="blue", colour="white") + labs(title = "Areas", x= "Personas por areas") #Esta no esta sirviendo 
  
  
  ######### 
  ggplot() + geom_bar(data=base, aes(x=ocupados))
  h=subset(base, P6020==1)
  m=subset(base, P6020==2)
  ggplot() + geom_bar(data=m, aes(x=ocupados))
  ggplot() + geom_bar(data=h, aes(x=ocupados))
  subset(base, P6020==1) %>% ggplot() + geom_bar(data=base, aes(x=ocupados)) #me salen iguales wtf, salen diferentes como lo hice arriba
  subset(base, P6020==2) %>% ggplot() + geom_bar(data=base, aes(x=ocupados))

  
  #Intento para categórica



  ggplot(data=base, aes(x=P6040)) + geom_bar() #tal vez este no hacerlo con barras sino otra que no se vea tan raro
  ggplot(data=base, aes(x=P6440)) + geom_bar()
  
  
  
  # Tomando a la media y la varianza del genero
  summarise(base, mediana = median(P6020), variance = var(P6020))

  # El promedio de los ingresos laborales promedio por genero
  base %>% group_by(P6020) %>% summarise(num = mean(P6750))
  
  # El ingreso total de las personas desocupadas
  base %>% group_by(ocupados == 1) %>% summarise(num = mean(P6750))
  
  # Ingreso promedio, minimo y maximo, por genero y desocupados
  base %>% group_by(ocupados, P6020)%>%
    summarise (ingreso = mean(P6750), min_ingreso = min(P6750), max_ingreso = max(P6750))
  
  # Graficas
  
  # GRAFICAS DE BARRAS #
  
  # Grafica de barras de que muestra la cantidad de desocupados y ocupados 
  grafica_barra1 <- ggplot() + geom_bar(data = base, aes(x = ocupados))
  
  # Crear variables de mujeres y hombres
  hombres = subset(base, P6020 == 1)
  mujeres = subset(base, P6020 == 2)
  
  # Grafica de barras de que muestra la cantidad de desocupados y ocupados por genero
  grafica_barra2 <- ggplot() + geom_bar(data = mujeres, aes(x = ocupados))
  grafica_barra3 <- ggplot() + geom_bar(data = hombres, aes(x = ocupados))
  
  subset(base, P6020 == 1) %>% ggplot() + geom_bar(data = base, aes(x = ocupados)) 
  subset(base, P6020 == 2) %>% ggplot() + geom_bar(data = base, aes(x = ocupados))
  
  group_by(base,P6020 == 1) %>% ggplot() + geom_bar(data = base, aes(x = ocupados))
  
  grafica_barra4 <- hombres %>% ggplot() + geom_bar(data = base, aes(x = ocupados)) 
  grafica_barra5 <- mujeres %>% ggplot() + geom_bar(data = base, aes(x = ocupados))
  
  # Grafica de barras de que muestra la cantidad de desocupados y ocupados por urbano rural
  grafica_barra6 <- ggplot() + geom_bar(data = base, aes(x = ocupados))
  
  # HISTOGRAMA #
  ingresos = subset(base, is.na(P6750) == F)
  
  # Histograma que muestra la distribucion de ingresos
  grafica_histograma1 <- ggplot() + geom_histogram(data = base, aes(x = P6750))
  
  # Histograma que muestra la distribucion de ingresos por genero
  grafica_histograma2 <- ggplot() + geom_histogram(data = hombres, aes(x = P6750)) + xlab("Ingresos de Hombres") 
  grafica_histograma3 <-ggplot() + geom_histogram(data = mujeres, aes(x = P6750)) + xlab("Ingresos de Mujeres") 
  
  # Histograma muestra la distribucion de las edades
  grafica_histograma4 <- ggplot(data=base, aes(x = P6040)) + geom_bar() #tal vez este no hacerlo con barras sino otra que no se vea tan raro
  
  # Histograma muestra la distribucion de los tipos de contratos
  grafica_histograma5 <- ggplot(data=base, aes(x = P6440)) + geom_bar()
  
  # Para exportar graficas como JPEG se utiliza 
  # Graficas de barra
  ggsave(plot = grafica_barra1, file = "views/grafica_barra1.jpeg")
  ggsave(plot = grafica_barra2, file = "views/grafica_barra2.jpeg")
  ggsave(plot = grafica_barra3, file = "views/grafica_barra3.jpeg")
  ggsave(plot = grafica_barra4, file = "views/grafica_barra4.jpeg")
  ggsave(plot = grafica_barra5, file = "views/grafica_barra5.jpeg")
  ggsave(plot = grafica_barra6, file = "views/grafica_barra6.jpeg")
  ggsave(plot = grafica_barra7, file = "views/grafica_barra7.jpeg")
  # Histogramas 
  ggsave(plot = grafica_histograma1, file = "views/grafica_histograma1.jpeg")
  ggsave(plot = grafica_histograma2, file = "views/grafica_histograma2.jpeg")
  ggsave(plot = grafica_histograma3, file = "views/grafica_histograma3.jpeg")
  ggsave(plot = grafica_histograma4, file = "views/grafica_histograma4.jpeg")
  ggsave(plot = grafica_histograma5, file = "views/grafica_histograma5.jpeg")


