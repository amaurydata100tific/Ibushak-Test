
###                               Primero cargamos las paqueterías
pacman::p_load(tidyverse,car,haven, dplyr, stringr, survey, srvyr, ggplot2,ggrepel,viridis,rebus,
               lubridate,treemapify,treemap,stringi,readxl,googlesheets,googledrive,dplyr,car,haven)

### Cargamos las bases de datos de IBUSHAK que fueron importadas a excel desde el servidor SQL

#1) Directorio y apertura de bases

wd <- "C:/Users/lapnt/OneDrive/Documentos/Ibushak Prueba_Tecnica/"
setwd(wd)

Categorias_SubCategorias <- readxl::read_xlsx("Base_DatosIbushak.xlsx", sheet = "Categoría_SubCategoría") 
Item_DB <- readxl::read_xlsx("Base_DatosIbushak.xlsx", sheet = "ItmenID Nombre")
MarketPlace_DB <- readxl::read_xlsx("Base_DatosIbushak.xlsx", sheet = "MarketPlace")
Ordenes_Venta <- readxl::read_xlsx("Base_DatosIbushak.xlsx", sheet = "Ordenes_Venta")
Ordenes_Venta_Fecha <- readxl::read_xlsx("Base_DatosIbushak.xlsx", sheet = "Ordenes_Venta_Fecha")


### Respondemos a la pregunta 1: A) En una tarjeta, la venta en pesos y piezas de 2020 c
###                                 omparada con la venta del año previo

### Para esto necesitamos sólamente la base de datos de Ordenes_Venta y juntarla con su fecha

### Antes creamos las variables de año, mes y fecha de la base de datos para poder unirla después


Ordenes_Venta_Fecha$CREATE_DATE <- as.Date(Ordenes_Venta_Fecha$CREATE_DATE,format="%Y-%m-%d") 

Ordenes_Venta_Fecha$anio <- format(Ordenes_Venta_Fecha$CREATE_DATE,"%Y")
Ordenes_Venta_Fecha$mes <- format(Ordenes_Venta_Fecha$CREATE_DATE,"%m")
Ordenes_Venta_Fecha$dia <- format(Ordenes_Venta_Fecha$CREATE_DATE,"%d")


### Una vez que ya extraímos los datos ahora sólo agrupamos y sumamos la variable de monto y 
##            la variable de cantidad de la base de Ordenes Ventas (se necesita un INNER_JOIN SQL)

base <- Ordenes_Venta_Fecha %>% select('TRANSACTION_ID','anio')

pregunta1_P1 <- inner_join(Ordenes_Venta,base, by = 'TRANSACTION_ID') %>% group_by(anio) %>% 
  summarise(Total_Ingresos = sum(Monto)) %>% mutate(Tipo = "Ingresos del año")


pregunta1_P2 <- inner_join(Ordenes_Venta,base, by = 'TRANSACTION_ID') %>% group_by(anio) %>% 
  summarise(Total_Pieza = sum(Cantidad)) %>% mutate(Tipo = "Piezas vendidas")


pregunta1 <- inner_join(pregunta1_P1,pregunta1_P2, by ='anio')

## Eliminamos las bases de procesamiento y nos quedamos con la final
rm(base,pregunta1_P1,pregunta1_P2)  


#### Pregunta 2 En un matriz calcular la venta por Marketplace del año 2020 y 2019, 
####        así como su porcentaje de participación para cada año correspondiente.


base <- MarketPlace_DB %>% select('LeadSourceID','Marketplace')

base2 <- inner_join(Ordenes_Venta_Fecha,base, by = 'LeadSourceID')

base <- base2 %>% select('TRANSACTION_ID','anio','Marketplace')

#### Para obtener el número de trasacciones por año ya sólamente contamos la cantidad de filas de TRANSACTION_ID

pregunta2 <- inner_join(Ordenes_Venta,base, by = 'TRANSACTION_ID') %>% group_by(anio,Marketplace) %>% 
  summarise(Total_ventas = n()) %>% mutate(Porcentaje = Total_ventas/sum(Total_ventas)*100)

rm(base,base2)

#### Pregunta 3 En una gráfica de tendencia, representar la venta 2020 vs 2019 
####                y su variación entre ambos por cada mes

### Dado que la gráfia se hará en Power BI no hay que hacer nada más que unir las bases 

base <- Ordenes_Venta_Fecha %>% select('TRANSACTION_ID','CREATE_DATE','anio','mes','dia')

pregunta3 <- inner_join(Ordenes_Venta,base, by ='TRANSACTION_ID')

rm(base)

###     Pregunta 4 En una tabla calcular para el año 2021 la venta promedio diaria y
####                  la venta promedio semanal para cada mes correspondiente

### Se hace directamente en Power BI


#### Pregunta 5 F) En una tabla mostrar el top 5 de los artículos más vendidos (en pesos). 
########                              éstatabla debe contener:

# -Id del articulo
# -Nombre del articulo
# -Foto del articulo
# -Venta actual
# -Venta año previo
# -Variación venta actual vs año previo por articulo

### Para ésta pregunta necesitamos juntar la base de item con ordenes de venta y ordenes de venta con fecha

## Seleccionamos las tres variables que necesitamos de la base de ITEM
base <- Item_DB %>% select('ITEM_ID','Name_ID','PHOTOS')

## Juntamos las variables con un inner_join con la base de ordenes
base2 <- inner_join(Ordenes_Venta,base, by = 'ITEM_ID')

## Seleccionamos las dos variables que necesitamos de la base de fecha
base <- Ordenes_Venta_Fecha %>% select('TRANSACTION_ID','anio')

## Juntamos todo en una base final que contenga Monto, Nombre, Fecha de la venta, etc
base3 <- inner_join(base2,base, by = 'TRANSACTION_ID')

rm(base,base2)

### Una vez que ya tenemos las bases unidas con las variables que nos interesas, sólo calculamos el total 
### de pesos por producto, necesitamos crear una variable que sume el MONTO agrupado por ID del ITEM

pregunta5 <- base3 %>% group_by(ITEM_ID,anio) %>% 
  mutate(Cantidad_Total = sum(Monto)) %>% ungroup()

## Filtramos los ITEM por año para quedarnos con 2019 vs 2020
pregunta5 <- pregunta5[!duplicated(pregunta5[c('ITEM_ID','anio')]),]

## Creamos una base que tenga el año y la cantidad total por año
base <- pregunta5 %>% 
  select('ITEM_ID','anio','Cantidad_Total')

## Con un wider wide poteamos la base para tener una columna para 2019 y otra columna para 2020
### Además es necesario convertr la variable a numérica para aquellos datos con valor nulo (NA)
base <- pivot_wider(base, names_from = anio, values_from = Cantidad_Total) %>% 
  rename(Venta_actual = '2020') %>% rename(Venta_anio_anterior = '2019') %>% 
  mutate(Venta_actual = ifelse(is.na(Venta_actual),0,Venta_actual)) %>% 
  mutate(Venta_anio_anterior = ifelse(is.na(Venta_anio_anterior),0,Venta_anio_anterior))

## La juntamos a la derecha de la base final
pregunta5 <- right_join(pregunta5,base, by = 'ITEM_ID') 

## Cremoas una resta del año actual - año anterioir y seleccionamos las variables que nos interesan
pregunta5 <- pregunta5[!duplicated(pregunta5[c('ITEM_ID')]),] %>% 
  mutate(Variacion_anual = Venta_actual - Venta_anio_anterior) %>% 
  arrange(desc(Venta_actual)) %>% select('ITEM_ID','Name_ID','PHOTOS','Venta_actual','Venta_anio_anterior','Variacion_anual') %>% 
  slice(1:5)

rm(base3,base)

### Por último sólamente nos queda guardar las dataframe en un excel que será el que 
###llevemos a powerBi para empezar a graficar


openxlsx::write.xlsx(list(pregunta1=pregunta1,
                          pregunta2=pregunta2,
                          pregunta3=pregunta3,
                          pregunta5=pregunta5),
                     file = ("C:/Users/lapnt/OneDrive/Documentos/Ibushak Prueba_Tecnica/Querys_ABI.xlsx"))

