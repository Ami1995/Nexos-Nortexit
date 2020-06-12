#################    
#### NORTEXIT ####
#################

# Código elaborado por: Ami G. Sosa Vera

## Paquetes ----
library(pacman)
p_load(janitor, tidyverse, readr, readxl, dplyr, ggplot2, scales, sf, leaflet, stringr)

## Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999)

## Bases de datos ----

# Datos Abiertos (Dirección General de Epidemiología, corte a 09/06/2020)
posi_covid <- read_csv("01_datos/200609COVID19MEXICO.csv")
#View(posi_covid)


# Catálogo de entidades
entidades <- read_excel("01_datos/diccionario_datos_covid19/Catalogos_0412.xlsx", 
                        sheet = "Catálogo de ENTIDADES", range = "A1:C37")
#View(entidades)

# Aprobación por estado (Ranking Mitofsky, Marzo-Abril 2020)
aprobacion <- read_excel("01_datos/APROBACIÓN GOBERNADORES.xlsx")
#View(aprobacion)

# Anuario Estadístico y Geográfico del INEGI (2019) - Población por entidad (2018)

poblacion <- read_excel("01_datos/702825192242/2. Poblaci¢n_xls_19.xlsx", sheet = "2.1", range = "A301:I332", col_names = c("entidad", "total_pob", "hombres", "mujeres", "0-14", "15-64", "65+"), col_types = c("text", "numeric", "skip",  "numeric", "numeric", "skip", "numeric", "numeric", "numeric"))

#View(poblacion)

# Anuario Estadístico y Geográfico del INEGI (2019) - Finanzas públicas, 17.3 Recaudación fiscal neta de ingresos federales tributarios y no tributarios por entidad en millones de pesos. (2018) 

ingresos <- read_excel("01_datos/702825192242/17. Finanzas p£blicas_xls_19.xlsx", sheet = "17.3", range = "A377:S408", col_names = c("entidad", "total", "tot_imp", "renta", "val_agr", "prod_ser", "com_ext", "autos", "otros", "tot_contrib"), col_types = c("text", "numeric", "skip", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "skip", "skip", "skip", "numeric", "skip", "skip", "skip", "skip", "skip", "numeric"))
#View(ingresos)

## Glosario de nombres de variables:
# total - recaudación total de la entidad fed. 
# tot_imp - ingresos totales por impuestos (sin contribuciones)
# renta - impuesto sobre la renta
# valor agregado - impuesto al valor agregado
# prod_ser - impuesto especial sobre producción y servicios
# com_ext - impuesto al comercio exterior
# autos - impuesto sobre automóviles nuevos
# otros - otros (?)
# tot_contrib - total contribuciones (ingresos no tributarios)



# Anuario Estadístico y Geográfico del INEGI (2019) - Finanzas públicas, 17.6 Aportaciones federales para entidades federativas y municipios por entidad federativa según fondo millones de pesos. # Transferencias federales (aportaciones - recursos etiquetados y participaciones) (2018) 

aportaciones <- read_excel("01_datos/702825192242/17. Finanzas p£blicas_xls_19.xlsx", 
                           sheet = "17.7", range = "A735:J766", 
                           col_names = c("entidad", "total", "FONE", "FASSA", "FAIS", "FAM", "FAFM", "FASP","FAETA", "FAFEF"))
#View(aportaciones)

# Anuario Estadístico y Geográfico del INEGI (2019) - Finanzas públicas, 17.7 Participaciones federales pagadas a entidades federativas y municipios por entiddad federativa en millones de pesos. (2018) 

participaciones <- read_excel("01_datos/702825192242/17. Finanzas p£blicas_xls_19.xlsx", 
                              sheet = "17.6", range = "A772:K803", 
                              col_names = c("entidad", "total", "f_gral", "f_fmun", "tenencia", "com_ext", "extr_ptrl", "inc_econ", "prod_serv", "autos", "otros"))
#View(participaciones)

## Glosario de nombres de variables:
# total - participaciones totales pagadas por entidad fed. 
# f_gral - fondo general de participaciones
# f_fmun- fondo de fomento municipal
# tenencia - tenencia 
# com_ext - comercio exterior
# extr_ptrl - derecho adicional sobre extracción de petróleo 
# inc_econ - incentivos económicos 
# prod_serv - impuesto especial sobre producción y servicios
# autos - impuesto sobre automóviles nuevos
# otros - otros (?)

# Código para combinar bases y llegar a la base final ----

### Código para llegar al número de contagios por estado y la tasa por cada 100 mil habitantes 

## Casos confirmados por entidad federativa 

posi_entfed <- posi_covid %>% 
  group_by(ENTIDAD_RES) %>% 
  filter(RESULTADO == 1) %>% 
  count()

posi_entfed[,2] <- lapply(posi_entfed[,2], as.numeric) # transformar n de integer a double

casos_entidad <- merge(posi_entfed, entidades,  # combinar bases, mágico merge
                       by.x="ENTIDAD_RES", 
                       by.y = "CLAVE_ENTIDAD")

casos_entidad <- casos_entidad %>%  
  select(ENTIDAD_FEDERATIVA, n)

## Base con población, contagios acumulados totales y tasa de contagio (por cada 100 mil habitantes) 

poblacion <- poblacion %>% 
  mutate(entidad = str_to_upper(entidad))

casos_pob_entidad <- merge(casos_entidad, poblacion, 
                           by.x="ENTIDAD_FEDERATIVA", 
                           by.y = "entidad")

casos_pob_entidad <- casos_pob_entidad %>% 
  mutate(tasa_pob = (total_pob/100000),
         tasa_contagios_pob = round(x = (n/tasa_pob), digits = 2)) %>% 
  rename(casos_acum_covid = n)

casos_pob_entidad  

## Base simplificada: entidades, total de contagios acumulados y tasa de contagios 

achu <- casos_pob_entidad %>% 
  select(ENTIDAD_FEDERATIVA, casos_acum_covid, tasa_contagios_pob)

## Base transferencias federales 

transferencias_fed <- merge(aportaciones, participaciones,  # combinar bases, mágico merge
                       by.x="entidad", 
                       by.y = "entidad")

transferencias_fed <- transferencias_fed %>% 
  select(entidad, total.x, total.y) %>% 
  rename(aport_total = total.x, partic_total = total.y) %>% 
  group_by(entidad) %>% 
  mutate(transf_total = sum(aport_total + partic_total)) %>% 
  select(entidad, transf_total)
  
## Añadimos contagios

# Asegurarse de que las observaciones de la variable que usaremos para el merge de las bases sean idénticas. 

achu <- achu %>%     
  mutate(ENTIDAD_FEDERATIVA = str_to_title(ENTIDAD_FEDERATIVA)) 

transferencias_fed[8, "entidad"]
achu[8, "ENTIDAD_FEDERATIVA"] <- "Coahuila de Zaragoza"

transferencias_fed[7, "entidad"]
achu[7, "ENTIDAD_FEDERATIVA"] <- "Ciudad de México"

transferencias_fed[16, "entidad"]
achu[16, "ENTIDAD_FEDERATIVA"] <- "Michoacán de Ocampo"

transferencias_fed[23, "entidad"]
achu[23, "ENTIDAD_FEDERATIVA"] <- "Quintana  Roo"

transferencias_fed[30, "entidad"]
achu[30, "ENTIDAD_FEDERATIVA"] <- "Veracruz de Ignacio de la Llave"


transfycontagios <- merge(transferencias_fed, achu,  
                            by.x="entidad", 
                            by.y = "ENTIDAD_FEDERATIVA")

## Le agregamos los ingresos 

transfycontagios[23, "entidad"]
ingresos[23, "entidad"] <- "Quintana  Roo"


ingresos <- ingresos %>% 
  select(entidad, total) %>% 
  rename(ing_total = total)

# Base completa ----

dineroycontagios <- merge(ingresos, transfycontagios,  
                          by.x="entidad", 
                          by.y = "entidad")

## Entidades que pertenezcan al norexit (con base en el mapa más popular de Twitter)

norte <- dineroycontagios %>% 
  filter(entidad %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila de Zaragoza", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo León", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"))


## Entidades que no pertenezcan al norexit (con base en el mapa más popular de Twitter)

sur <- dineroycontagios %>% 
  filter(entidad %in% c("Campeche", "Chiapas", "Colima", "Ciudad de México", "Guerrero", "Hidalgo", "México", "Michoacán de Ocampo", "Morelos", "Oaxaca", "Tlaxcala", "Campeche", "Puebla", "Quintana  Roo", "Tabasco", "Veracruz de Ignacio de la Llave", "Yucatán")) 

####################
#### RESULTADOS ####
####################

# ¿Es verdad que los estados del norte aportan más y reciben menos? ----

# Para empezar, ¿cuánto recauda cada región?

sum(norte$ing_total)  # 1,090,090 mill. pesos 

sum(sur$ing_total)   # 2,805,481 mill. pesos

## El sur recauda 1,715,391 mill. pesos más que el norte. 
# Total nacional sum(dineroycontagios$ing_total) 

# ¿Cuánto les trasnfieren? ----

sum(norte$transf_total)   # 626,187.9 mill. pesos

sum(sur$transf_total)    # 951,267.5 mill. pesos 

## El sur recibe 325,079.6 pesos más que el norte. 
# Total nacional sum(dineroycontagios$transf_total)

# En esta catalogación, ¿quienes tienen más contagios?

sum(norte$casos_acum_covid)  # 34,979 contagios acumulados (corte al 9 de junio)

sum(sur$casos_acum_covid)   # 89,322 contagios acumulados (corte al 9 de junio)

## El sur tiene 54,343 más contagios acumulados que el norte (corte al 9 de junio)
# Total nacional sum(dineroycontagios$casos_acum_covid)

# ¿Los estados que aportan más dinero son los más enfermos? ----

# Si, pero son los estados fuera del nortexit. El sur recaudó más dinero a través de impuestos y contribuciones que el norte, recibió más transferencias federales y tiene más contagios que el norte. 

# ¿Qué gobernadores han capitalizado el momento (cambio en el Ranking de Mitofsky)?

# Nacional 
aprobacion %>% 
  arrange(-Incremento) %>% 
  print(n = 3)

# Jalisco, EDOMEX y Guerrero han sido los estados que que han tenido un mayor incremento en la aprobación de gobernadoras y gobernadores

# Norte
na <- aprobacion %>% 
  filter(Estado %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo Leon", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"))

na %>% 
  arrange(-Incremento) %>% 
  print(n = 3)

# Jalisco, NL y Tamaulipas han sido los estados del Norte que han tenido un mayor incremento de aprobación 

# Sur 
sa <- aprobacion %>% 
  filter(Estado %in% c("Campeche", "Chiapas", "Ciudad de México", "Guerrero", "Hidalgo", "EDOMEX", "Michoacán de Ocampo", "Morelos", "Oaxaca", "Tlaxcala", "Campeche", "Puebla", "Quintana Roo", "Tabasco", "Veracruz de Ignacio de la Llave", "Yucatán")) 

sa %>% 
  arrange(-Incremento) %>% 
  print(n = 3)

# México, Guerrero y Morelos han sido los estados del Sur que que han tenido un mayor incremento de aprobación 

# CORRELACIÓN - ¿Los estados que aportan más dinero son los más enfermos?
# casos acumulados e ingresos

# Norte
cor(norte$casos_acum_covid, norte$ing_total)  # 0.2310532
# plot(norte$casos_acum_covid, norte$ing_total)
lm(casos_acum_covid ~ ing_total, data = norte)
enfn <- lm(casos_acum_covid ~ ing_total, data = norte)
summary(enfn)  # Multiple R-squared:  0.05339

# Sur
cor(sur$casos_acum_covid, sur$ing_total)  # 0.8699649
# plot(sur$casos_acum_covid, sur$ing_total)
lm(casos_acum_covid ~ ing_total, data = sur)
enfs <- lm(casos_acum_covid ~ ing_total, data = sur)
summary(enfs)  # Multiple R-squared:  0.7568


# Las correlación del sur es fuerte, la del norte no tanto. 

# CORRELACIÓN - Transferencias?

# casos acumulados y transferencias federales

# Norte
cor(norte$casos_acum_covid, norte$transf_total)  # 0.4720195
plot(norte$casos_acum_covid, norte$transf_total)
lm(casos_acum_covid ~ transf_total, data = norte)
tn <- lm(casos_acum_covid ~ transf_total, data = norte)
abline(tn)
summary(tn) # Multiple R-squared:  0.2228


# Sur
cor(sur$casos_acum_covid, sur$transf_total)  # 0.8227108
plot(sur$casos_acum_covid, sur$transf_total)
lm(casos_acum_covid ~ transf_total, data = sur)
ts <- lm(casos_acum_covid ~ transf_total, data = sur)
summary(ts)  # Multiple R-squared:  0.6769 

# Las correlación del Sur entre transferencias y contagios totales es más fuerte que la del Norte. 

# Recordatorio: R-squared is always between 0 and 100%:
# 0% indicates that the model explains none of the variability of the response data around its mean.
# 100% indicates that the model explains all the variability of the response data around its mean.


# Tasa de contagios----

# Nacional
casos_pob_entidad %>% 
  mutate(pob_reg = sum(total_pob),
         tasa_reg = (pob_reg/100000),
         cont_reg = sum(casos_acum_covid),
         tc_reg = round(x = (cont_reg/tasa_reg), digits = 2))

## 99.18 (comparada con la del gobierno de 99.27), corte 9/06/2020

# Norte (población total / 100 mil hab.)

cn <- casos_pob_entidad %>% 
  filter(ENTIDAD_FEDERATIVA %in% c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", "COAHUILA DE ZARAGOZA", "DURANGO", "GUANAJUATO", "JALISCO", "NAYARIT", "NUEVO LEÓN", "QUERÉTARO", "SAN LUIS POTOSÍ", "SINALOA", "SONORA", "TAMAULIPAS", "ZACATECAS"))

cn %>% 
  mutate(pob_reg = sum(total_pob),
         tasa_reg = (pob_reg/100000),
         cont_reg = sum(casos_acum_covid),
         tc_reg = round(x = (cont_reg/tasa_reg), digits = 2))

## 67.52


# Sur
cs <- casos_pob_entidad %>% 
  filter(ENTIDAD_FEDERATIVA %in% c("CAMPECHE", "CHIAPAS", "COLIMA", "CIUDAD DE MÉXICO", "GUERRERO", "HIDALGO", "MÉXICO", "MICHOACÁN DE OCAMPO", "MORELOS", "OAXACA", "TLAXCALA", "CAMPECHE", "PUEBLA", "QUINTANA  ROO", "TABASCO", "VERACRUZ DE IGNACIO DE LA LLAVE", "YUCATÁN"))

cs %>% 
  mutate(pob_reg = sum(total_pob),
         tasa_reg = (pob_reg/100000),
         cont_reg = sum(casos_acum_covid),
         tc_reg = round(x = (cont_reg/tasa_reg), digits = 2))

## 121.17  EL SUR SIGUE TENIENDO UNA TASA MAS ALTA DE CONTAGIOS QUE EL NORTE 

# Visualizaciones ----

# Mapa del país

edos <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/mexicoTrends.geojson") %>% 
  st_transform(crs = 4326) 

# Mapa Nortexit (Nexos - título añadido a patita en Keynote) ----

edos_norexit <- edos %>%
  mutate(nrxt= ifelse(ENTIDAD %in% c("AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CHIHUAHUA", "COAHUILA DE ZARAGOZA", "DURANGO", "GUANAJUATO", "JALISCO", "NAYARIT", "NUEVO LEON", "QUERETARO DE ARTEAGA", "SAN LUIS POTOSI", "SINALOA", "SONORA", "TAMAULIPAS", "ZACATECAS"), 1, 0 ))

# Graficamos

plot(edos_norexit, max.plot = 1)

# Colores

palMuni <- colorFactor(palette = c("#fff459", "#af59ff"), 
                       domain = edos_norexit$nrxt)


leaflet(edos_norexit, options = leafletOptions(zoomControl = T)) %>% 
  addTiles() %>% 
  addPolygons(color = "black", 
              weight = 1, 
              fillColor = palMuni(edos_norexit$nrxt), 
              fillOpacity = 0.7) %>% 
  addLegend(position = "bottomleft",
            colors = c("#af59ff", "#fff459"), 
            labels = c("Norte", "Sur"), 
            opacity = 1) 

# Gráfica Impuestos federales recaudados (Nexos) ----

recauda.mas <- tibble(belong <- c("Norte","Sur"),
                      cantidad <- c(1090090, 2805481))

recauda.mas %>% 
  ggplot(aes(x = belong, y = cantidad)) +
  geom_col(fill = c("#af59ff", "#fff459")) + 
  scale_y_continuous(labels = comma) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid=element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5, vjust = 1, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.subtitle = element_text(size = 17, color = "black", hjust = 0.5,vjust = 1, face = "plain")) +
  labs(title = "Impuestos federales recaudados",
       subtitle = "Total por región (millones de pesos)",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") +
  geom_text(aes(label = c("$1,090,090", "$2,805,481")), position=position_dodge(width=0.9), hjust = 0.5, vjust = -0.2, size = 6, color = "black", fontface = 2) 

# Gráfica Transferencias federales (Nexos) ----

pertenece <- c("Norte","Sur")
cantidat <- c(626187.9, 951267.5)

recibe.mas <- tibble(pertenece, cantidat)

recibe.mas %>% 
  ggplot(aes(x = pertenece, y = cantidat)) +
  geom_col(fill = c("#af59ff", "#fff459")) + 
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16, color = "black"),
        axis.text.y = element_blank(),
        panel.grid=element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5, vjust = 1, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.subtitle = element_text(size = 17, color = "black", hjust = 0.5, vjust = 1, face = "plain")) +
  labs(title = "Transferencias federales",
       subtitle = "Total por región (millones de pesos)",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") +
  geom_text(aes(label = c("$626,187.9", "$951,267.5")), position=position_dodge(width=0.9), hjust = 0.5, vjust = -0.2, size = 6, color = "black", fontface = 2) 

# Gráfica incremento de aprobación (Nexos) ----


# Paleta de color por partido:
# Independiente - "#f2a1f7"
# MC - "#fd8104"
# MORENA - "#621131" 
# PAN - "#007abc"  
# PES - "#7704bd"
# PRD - "#ffde00"
# PRI - "#15a152"  

# Lo mismo pero por región

pafuera<- aprobacion %>% 
  mutate(juera= ifelse(Estado %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo Leon", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"), "Norte", "Sur"))

pafuera %>% 
  ggplot(aes(x = fct_reorder(Estado, `Aprobacion Abril`),
             y = `Aprobacion Abril`,
             fill = juera)) +
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = c("#af59ff", "#fff459")) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 14, color = "black", face = "bold"),
        legend.title = element_text(size = 11, color = "black", face = "bold"),
        legend.key.size = unit(1.5, "line"),
        legend.text = element_text(size = 9, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 1, color = "black", face = "italic"),
        plot.subtitle = element_text(size = 11, color = "black", face = "plain")) +
  labs(title = "Aprobación de gobernadores",
       fill = "Región",
       subtitle = "Por entidad y región, abril 2020",
       caption = "Fuente: Ranking de Mitofsky de desempeño de las y los gobernadores\nElaboración propia") +
  scale_y_continuous(labels = c("0%", "20%", "40%", "60%"))

# Incremento de aprobación 

# Asegurarse de que las observaciones de la variable que usaremos para el merge de las bases sean idénticas. 

entidades[5, "ENTIDAD_FEDERATIVA"]
aprobacion[8, "Estado"] <- "COAHUILA DE ZARAGOZA"

entidades[9, "ENTIDAD_FEDERATIVA"]
aprobacion[5, "Estado"] <- "CIUDAD DE MÉXICO"

entidades[15, "ENTIDAD_FEDERATIVA"]
aprobacion[11, "Estado"] <- "MÉXICO"   

entidades[16, "ENTIDAD_FEDERATIVA"]
aprobacion[16, "Estado"] <- "MICHOACÁN DE OCAMPO"

entidades[19, "ENTIDAD_FEDERATIVA"]
aprobacion[19, "Estado"] <- "NUEVO LEÓN"

entidades[22, "ENTIDAD_FEDERATIVA"]
aprobacion[22, "Estado"] <- "QUERÉTARO" 

entidades[24, "ENTIDAD_FEDERATIVA"]
aprobacion[25, "Estado"] <- "SAN LUIS POTOSÍ"

entidades[30, "ENTIDAD_FEDERATIVA"]
aprobacion[30, "Estado"] <- "VERACRUZ DE IGNACIO DE LA LLAVE"

aprobacion <- aprobacion %>%     
  mutate(Estado = str_to_upper(Estado)) 

aprobacion <- merge(entidades, aprobacion,                      
                  by.x="ENTIDAD_FEDERATIVA",  # mágico merge
                  by.y = "Estado")

aprobacion <- aprobacion %>%     
  mutate(ENTIDAD_FEDERATIVA = str_to_sentence(ENTIDAD_FEDERATIVA)) 

aprobacion[19, "ENTIDAD_FEDERATIVA"] <- "Nuevo León"
aprobacion[23, "ENTIDAD_FEDERATIVA"] <- "Quintana Roo"
aprobacion[24, "ENTIDAD_FEDERATIVA"] <- "San Luis Potosí"
aprobacion[3, "ENTIDAD_FEDERATIVA"] <- "Baja California Sur"
aprobacion[16, "ENTIDAD_FEDERATIVA"] <- "Michoacán"
aprobacion[8, "ENTIDAD_FEDERATIVA"] <- "Coahuila"
aprobacion[2, "ENTIDAD_FEDERATIVA"] <- "Baja California"
aprobacion[30, "ENTIDAD_FEDERATIVA"] <- "Veracruz"
aprobacion[7, "ENTIDAD_FEDERATIVA"] <- "Ciudad de México"

# Creo que todo eso era innecesario, pero bueno, perdone usted

# Gráfica Aprobación (Nexos) ----

aprobacion %>% 
  ggplot(aes(x = fct_reorder(ENTIDAD_FEDERATIVA, Incremento),
             y = Incremento,
             fill = Partido)) +
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = c("#f2a1f7", "#fd8104", "#621131", "#007abc", "#7704bd", "#ffde00", "#15a152" )) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14, color = "black"),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5, vjust = 1, color = "black", face = "bold"),
        legend.title = element_text(size = 14, color = "black", face = "bold"),
        legend.key.size = unit(1.5, "line"),
        legend.text = element_text(size = 11, color = "black", face = "plain"),
        plot.caption = element_text(size = 12, hjust = 1, color = "black", face = "italic"),
        plot.subtitle = element_text(size = 16, color = "black", face = "plain")) +
  labs(title = "Incremento en la aprobación de gobernadores",
       subtitle = "Por entidad y partido, abril 2020",
       caption = "Fuente: Ranking de Mitofsky de desempeño de las y los gobernadores\nElaboración propia") +
  scale_y_continuous(labels = c("-10%","0%", "20%", "40%", "60%"))


aprobacion %>% 
  arrange(-Incremento) %>% 
  select(ENTIDAD_FEDERATIVA) 

# Jalisco, EDOMEX y Guerrero han sido los estados que que han tenido un mayor incremento de aprobación 

# Norte
na <- aprobacion %>% 
  filter(ENTIDAD_FEDERATIVA %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo León", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"))

na.g <- na %>% 
  arrange(-Incremento) %>% 
  print()

# Jalisco, NL y Tamaulipas han sido los estados del Norte que han tenido un mayor incremento de aprobación 

# Sur 

sa <- aprobacion %>% 
  filter(ENTIDAD_FEDERATIVA %in% c("Campeche", "Chiapas", "Colima", "Ciudad de México", "Guerrero", "Hidalgo", "México", "Michoacán", "Morelos", "Oaxaca", "Tlaxcala", "Campeche", "Puebla", "Quintana Roo", "Tabasco", "Veracruz", "Yucatán")) 

sa %>% 
  arrange(-Incremento) %>% 
  print()

# EDOMEX, Guerrero y Morelos han sido los estados del Sur que que han tenido un mayor incremento de aprobación 

# Contagios COVID-19 ----

sum(norte$casos_acum_covid)  # 34,979
sum(sur$casos_acum_covid)  # 89,322
sum(dineroycontagios$casos_acum_covid) # 124301


# Gráfica Contagios Acumulados (Nexos) ----

acum <- tibble(belong <- c("Norte","Sur"),
                      cantidad <- c(34979, 89322))

acum %>% 
  ggplot(aes(x = belong, y = cantidad)) +
  geom_col(fill = c("#af59ff", "#fff459")) + 
  scale_y_continuous(labels = comma) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5, vjust = 1, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.subtitle = element_text(size = 17, color = "black", hjust = 0.5, vjust = 1, face = "plain")) +
  labs(title = "Casos acumulados de COVID-19",
       subtitle = "Total por región",
       caption = "Fuente: Datos Abiertos (Dirección General de Epidemiología, corte a 09/06/2020)\nElaboración propia") +
  geom_text(aes(label = c("34,979", "89,322")), position=position_dodge(width=0.9), hjust = 0.5, vjust = -0.2, size = 6, color = "black", fontface = 2)  

# Gráfica Tasa Contagios (Nexos)----
 
# Norte ## 67.52
# Sur ## 121.17
# Nacional ## 99.18 (comparada con la del gobierno de 99.27)

# Tasa

tasa <- tibble(belong <- c("Norte","Sur"),
                   cantidad <- c(67.52, 121.17))

tasa %>% 
  ggplot(aes(x = belong, y = cantidad)) +
  geom_col(fill = c("#af59ff", "#fff459")) + 
  scale_y_continuous(labels = comma) +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size = 20, color = "black", hjust = 0.5, vjust = 1, face = "bold"),
        plot.caption = element_text(size = 14, face = "italic"),
        plot.subtitle = element_text(size = 17, color = "black", hjust = 0.5, vjust = 1, face = "plain")) +
  labs(title = "Tasa de contagios de COVID-19 por región",
       subtitle = "Tasa por cada 100,000 habitantes de la población regional ",
       caption = "Fuente: Datos Abiertos (Dirección General de Epidemiología, corte a 09/06/2020)\nElaboración propia\nTasa nacional calculada = 99.18, la que reporta la Dirección General de Epidemiología = 99.27") +
  geom_text(aes(label = c("67.52", "121.17")), position=position_dodge(width=0.9), hjust = 0.5, vjust = -0.2, size = 6, color = "black", fontface = 2)  


# Gráficas adicionales ----

# Grupos de estados por región (facet wrap) Ingresos 

sale <- dineroycontagios %>% 
  mutate(se.sale = ifelse(entidad %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila de Zaragoza", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo León", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"), "Norte", "Sur"))

sale %>% 
  ggplot(aes(x = fct_reorder(entidad,
                             -ing_total),
             y = ing_total, 
             fill = se.sale)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  facet_wrap(~ se.sale, scales = "free_y") +
  scale_fill_manual(values = c( "#af59ff", "#fff459","#fff459",  "#af59ff",  "#af59ff",  "#af59ff", "#fff459",  "#af59ff", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#fff459", "#fff459", "#af59ff")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"),
        title = element_text(size = 12, color = "black", vjust = 1, face = "bold")) +
  labs(title = "Ingresos por entidad",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 

# País y diferencia por color 

sale %>% 
  ggplot(aes(x = fct_reorder(entidad,
                             -ing_total),
             y = ing_total,
             fill = se.sale)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  coord_cartesian(ylim=c(0, 350000)) +
  scale_fill_manual(values = c( "#af59ff", "#fff459","#fff459",  "#af59ff",  "#af59ff",  "#af59ff", "#fff459",  "#af59ff", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#fff459", "#fff459", "#af59ff")) +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        title = element_text(size = 12, color = "black", vjust = 1, face = "bold")) +
  labs(title = "Ingreso total por entidad (millones de pesos)",
       subtitle = "(CMDX llega a $ 2,092,180.021, sale de la gráfica)",
       caption = "Fuente: Datos Abiertos Secretaria de Salud,\nAnuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 

# Grupos de estados por región (facet wrap) Transferencias

sale <- dineroycontagios %>% 
  mutate(se.sale = ifelse(entidad %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila de Zaragoza", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo León", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"), "Norte", "Sur"))

sale %>% 
  ggplot(aes(x = fct_reorder(entidad,
                             -transf_total),
             y = transf_total, 
             fill = se.sale)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  coord_flip() +
  facet_wrap(~ se.sale, scales = "free_y") +
  scale_fill_manual(values = c( "#af59ff", "#fff459","#fff459",  "#af59ff",  "#af59ff",  "#af59ff", "#fff459",  "#af59ff", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#fff459", "#fff459", "#af59ff")) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        plot.caption = element_text(size = 10, face = "italic"),
        axis.ticks = element_blank(),
        title = element_text(size = 12, color = "black", vjust = 1, face = "bold")) +
  labs(title = "Transferencias federales por entidad",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 

# País y diferencia por color 

sale %>% 
  ggplot(aes(x = fct_reorder(entidad,
                             -transf_total),
             y = transf_total,
             fill = se.sale)) +
  geom_col() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c( "#af59ff", "#fff459","#fff459",  "#af59ff",  "#af59ff",  "#af59ff", "#fff459",  "#af59ff", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459",  "#af59ff", "#fff459", "#fff459", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#af59ff", "#fff459", "#fff459", "#af59ff")) +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        legend.key = element_blank(),
        legend.text = element_text(size = 13, color = "black"),
        legend.title = element_blank(),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        title = element_text(size = 12, color = "black", vjust = 1, face = "bold")) +
  labs(title = "Transferencias por entidad (millones de pesos)",
       subtitle = "(EDOMEX llega a $188,272.98)",
       caption = "Fuente: Datos Abiertos Secretaria de Salud,\nAnuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 


# Norte ingreso y transferencias  ----

# ingreso sum(norte$ing_total)  # 1,090,090 pesos 
# trasnferencias sum(norte$transf_total)   # 626,187.9 pesos

# Columnas con totales norte 

money <- c("Ingresos","Transferencias\nfederales")
dinero <- c(1090090, 626187.9)

perfil_norte <- tibble(money, dinero)

pnorte <- ggplot(perfil_norte, 
                 aes(x = "", 
                     y = dinero,
                     fill = money)) +
  geom_bar(width = 1, stat = "identity") + 
  scale_fill_manual(values=c("#af59ff", "#fff459")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        panel.grid = element_blank(), 
        axis.title = element_blank(),
        legend.title = element_text(size = 14, color = "black", face = "bold"),
        legend.key.size = unit(2.5, "line"),
        legend.text = element_text(size = 12, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "black", face = "italic")) +
  labs(fill = "Norte",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 

# Pie Norte (Gráfica que Segasi odiaría) 

pie_nor <- pnorte + coord_polar("y", start=0)

pie_nor + 
  blank_theme +
  theme(axis.text.x=element_blank(),
        legend.key.size = unit(2.5, "line"),
        legend.text = element_text(size = 12, color = "black", face = "plain"))


# Sur ingreso y transferencias  ----

# ingreso sum(sur$ing_total)   # 2,774,632 pesos
# trasnferencias sum(sur$transf_total)   # 929,288.8 pesos

# Columnas con totales norte 

money.s <- c("Ingresos","Transferencias\nfederales")
dinero.s <- c(2805481, 951267.5)

perfil_sur <- tibble(money.s, dinero.s)

psur <- ggplot(perfil_sur, 
               aes(x = "", 
                   y = dinero.s,
                   fill = money.s)) +
  geom_bar(width = 1, stat = "identity") + 
  scale_fill_manual(values=c("#af59ff", "#fff459")) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(size = 14, color = "black", face = "bold"),
        legend.key.size = unit(2.5, "line"),
        legend.text = element_text(size = 12, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 0.5, color = "black", face = "italic")) +
  labs(fill = "Sur",
       caption = "Fuente: Anuario Estadístico y Geográfico del INEGI (2019)\nElaboración propia") 

# Pie Sur (Gráfica que Segasi odiaría 2.0) 

pie_sur <- psur + coord_polar("y", start=0)


pie_sur + 
  blank_theme +
  theme(axis.text.x=element_blank(),
        legend.key.size = unit(2.5, "line"),
        legend.text = element_text(size = 12, color = "black", face = "plain")) 

# Aprobación en Abril 2020

# ¿Qué gobernadores han capitalizado el momento? 

aprobacion %>% 
  ggplot(aes(x = fct_reorder(Estado, `Aprobacion Abril`),
             y = `Aprobacion Abril`,
             fill = Partido)) +
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = c("#f2a1f7", "#fd8104", "#621131", "#007abc", "#7704bd", "#ffde00", "#15a152" )) +
  theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 14, color = "black", face = "bold"),
        legend.title = element_text(size = 11, color = "black", face = "bold"),
        legend.key.size = unit(1.5, "line"),
        legend.text = element_text(size = 9, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 1, color = "black", face = "italic"),
        plot.subtitle = element_text(size = 11, color = "black", face = "plain")) +
  labs(title = "Aprobación de gobernadores",
       subtitle = "Por entidad y partido, abril 2020",
       caption = "Fuente: Ranking de Mitofsky de desempeño de las y los gobernadores\nElaboración propia") +
  scale_y_continuous(labels = c("0%", "20%", "40%", "60%"))

# Incremento por región 

na.n <- aprobacion %>% 
  arrange(-Incremento) %>% 
  print(n = 5) %>% 
  mutate(exit= ifelse(Estado %in% c("Aguascalientes", "Baja California", "Baja California Sur", "Chihuahua", "Coahuila", "Durango", "Guanajuato", "Jalisco", "Nayarit", "Nuevo Leon", "Querétaro", "San Luis Potosí", "Sinaloa", "Sonora", "Tamaulipas", "Zacatecas"), "Norte", "Sur"))


na.n %>% 
  ggplot(aes(x = fct_reorder(Estado, Incremento), 
             y = Incremento, 
             fill = exit)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=c("#af59ff", "#fff459")) + theme_minimal() +
  theme(axis.text.y = element_text(color = "black"),
        axis.title = element_blank(),
        plot.title = element_text(size = 14, color = "black", face = "bold"),
        legend.title = element_text(size = 11, color = "black", face = "bold"),
        legend.key.size = unit(2, "line"),
        legend.text = element_text(size = 10, color = "black", face = "plain"),
        plot.caption = element_text(size = 10, hjust = 1, color = "black", face = "italic"),
        plot.subtitle = element_text(size = 11, color = "black", face = "plain")) +
  labs(title = "Incremento en la aprobación de gobernadores",
       fill = "Región", 
       subtitle = "Por entidad y región, abril 2020",
       caption = "Fuente: Ranking de Mitofsky de desempeño de las y los gobernadores\nElaboración propia")+
  scale_y_continuous(labels = c("-05%","0%", "20%", "40%", "60%"))

############################
#### RESUMEN RESULTADOS ####
############################

# Ingresos (INEGI, 2019)

# Norte - 1,090,090 millones de pesos, un 27.9828 % del total
# Sur - 2,805,481 millones de pesos, un 72.01718 % del total
# Nacional - 3,895,572 millones de pesos 

# Transferencias federales (INEGI, 2019)

# Norte - 626,187.9 millones de pesos, un 39.69609 % del total
# Sur - 951,267.5 millones de pesos, un 60.30394 % del total
# Nacional - 1,577,455 millones de pesos 

# Contagios (corte al 9 de Junio del 2020)

# Acumulados:
# Norte - 34,979, un 28.14056 % del total
# Sur - 89,322, un 71.85944 % del total
# Nacional - 124,301

# Tasa:
# Norte - 67.52
# Sur - 121.17
# Nacional - 99.18 (comparada con la de la Dirección General de Epidemiología de 99.27)

# GRACIAS POR LEER <3
