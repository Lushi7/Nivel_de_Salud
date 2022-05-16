###########################################################################
# LIBRERIAS A USAR --------------------------------------------------------


library(datos)
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(reshape2)
library(stringr)
library(tidyverse)
library(tidyr)
library(writexl)

###########################################################################
# COPIA DE DATOS DESDE OLAP -----------------------------------------------

# ABRO EL ARCHIVO DE OLAP "//10.21.11.5/4-Servicios de Salud/conexion rend olap.xlsx"
# Contraseña y usuario OLAP Rendimientos Hospitalarios:

# Usuario: dis.msal@markeycomar.onmicrosoft.com
# CLAVE: DIS-2022

# EN LA TABLA DINAMICA - no olvidar actualizar -
# SELECCIONO PARA OBTENER LAS COLUMNAS:
# FILTRO: Año = 2022 o 2021, Dependencia = Todas?
# FILAS: Region	Partido	Establecimiento Mes
# (Las agrego desde el campo PBI_Rendimiento -> Mas campos)
# VALORES: Egresos_M (esta en el campo SIGMA PBI_Rendimiento, da el Total)

# COPIO (Region	Partido	Establecimiento Mes Total) Y PEGO COMO DATOS EN: 
# "//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Descarga_de_OLAP.xlsx"

# ABRO BASE QUE COPIAMOS DESDE OLAP ---------------------------------------

# Descarga_de_OLAP_2021 <- read_excel("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Descarga_de_OLAP_2021.xlsx", col_types = c("text", "text", "text", "text", "numeric"), skip = 2)
Descarga_de_OLAP_2021 <- read.csv("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Descarga_de_OLAP_2021.csv", encoding="UTF-8")

# Descarga_de_OLAP_2022 <- read_excel("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Descarga_de_OLAP_2022.xlsx", col_types = c("text", "text", "text", "text", "numeric"), skip = 2)
Descarga_de_OLAP_2022 <- read.csv("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Descarga_de_OLAP_2022.csv", encoding="UTF-8")

# ABRO F10 ----------------------------------------------------------------

F102021 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2021_total 05-2022/2021_F10.TXT", quote = "")
F102022 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2022_total 05-2022/2022_F10.TXT", quote = "")

# AL USAR -> quote = "" <- EVITO EL PROBLEMA DEL CARACTER OCULTO Y ABRE TODO

###########################################################################
# CAMBIO NOMBRE BASES ------------------------------------------------------

# SRH <- Descarga_de_OLAP_2021
# F <- F102021

SRH <- Descarga_de_OLAP_2022
F <- F102022

###########################################################################
###########################################################################
# LIMPIEZA BASE -----------------------------------------------------------

# ELIMINO COLUMNAS QUE NO QUIERO

SRH$X.U.FEFF.Region <- NULL
SRH$Partido <- NULL

# SEPARO CODIGO DE ESTABLECIMIENTO DEL NOMBRE
SRH$Establecimiento = substr(SRH$Establecimiento, 1, 8)

SRH$Establecimiento = as.character(SRH$Establecimiento)

# CORROBORO QUE MIS TOTALES SEAN DE TIPO NUMERICO:
SRH$Egresos = as.integer(SRH$Egresos)

# CAMBIO NOMBRE estable_id POR Establecimiento PARA QUE QUEDEN IGUAL
SRH <- SRH %>% rename(SRH = Egresos)

###########################################################################
# SALIDA ------------------------------------------------------------------

# write_xlsx(SRH.MESES.TOTALES, "//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Egresos_SRH.xlsx")

###########################################################################
# TRABAJO CON F10 ---------------------------------------------------------


# ELIMINO FILAS QUE NO TIENEN NADA EN sexo (QUITO LA FILA QUE TIENE EL TOTAL)
F <- F[!is.na(F$sexo),]

# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------
f1 <- F

DUP <- f1[duplicated(f1$form10_id) | duplicated(f1$form10_id, fromLast=TRUE), ]

# 
# f1$DUP <- duplicated(f1[,c("form10_id")],)
# 
# F <- filter(f1, DUP == FALSE)
# 
# F$DUP <- NULL

###########################################################################
# GENERO COLUMNAS MES Y TOTAL SEGUN ESTABLECIMIENTO -----------------------


F10 <- F %>% dplyr::select(estable_id, mes)

# F10$mes <- as.numeric(F10$mes)
F10$estable_id <- stringr::str_pad(F10$estable_id, 8, side = "left", pad = 0)

F10. <- F10 %>% 
  group_by(estable_id, mes) %>% dplyr::summarise(CIPRES = n())

# CORROBORO QUE MIS TOTALES SEAN DE TIPO NUMERICO:
F10.$CIPRES = as.integer(F10.$CIPRES)

# CAMBIO NOMBRE estable_id POR Establecimiento PARA QUE QUEDEN IGUAL
F10. <- F10. %>% rename(Establecimiento = estable_id,
                        Mes = mes)

# ELIMINA FILAS QUE CONTIENEN NA
F10. <- na.omit(F10.)

###########################################################################
# SALIDA ------------------------------------------------------------------

# write_xlsx(F10.MESES.TOTALES, "//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Egresos_F10.xlsx")

###########################################################################
###########################################################################
###########################################################################
# UNO EGRESOS DE SRH CON LOS DE CIPRES ------------------------------------

EGRESOS <- merge(SRH, F10., all = TRUE)


EGRESOS$Mes <- stringr::str_pad(EGRESOS$Mes, 2, side = "left", pad = 0)

###########################################################################
###########################################################################
###########################################################################
# ABRO GUIA ESTABLECIMIENTOS ----------------------------------------------

GUIA.EST <- read_excel("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Guia-R.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", "text"))

# SELECCIONO SOLO LAS COLUMNAS QUE QUIERO DE LA GUIA
Guia <- GUIA.EST %>% dplyr::select(REGION, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO)

# CAMBIO NOMBRE COLUMNA PARA PODER UNIRLAS
EGRESOS <- EGRESOS %>% rename(COD.EST = Establecimiento)

# CHEQUEO QUE TENGA EL FORMATO QUE QUIERO
EGRESOS$COD.EST <- stringr::str_pad(EGRESOS$COD.EST, 8, side = "left", pad = 0)

EGRESOS.FINAL <- merge(Guia, EGRESOS, by = "COD.EST", all.y = TRUE)


# CALCULO % COBERTURA -----------------------------------------------------

EGRESOS.FINAL$SRH <- ifelse(EGRESOS.FINAL$SRH == 0, NA, EGRESOS.FINAL$SRH)
EGRESOS.FINAL$CIPRES <- ifelse(EGRESOS.FINAL$CIPRES == 0, NA, EGRESOS.FINAL$CIPRES)

EGRESOS.FINAL$`% COBERTURA` <- (EGRESOS.FINAL$CIPRES / EGRESOS.FINAL$SRH)

EGRESOS.FINAL$`% COBERTURA` <- format(round(EGRESOS.FINAL$`% COBERTURA`, 2))

EGRESOS.FINAL$`% COBERTURA` <- as.numeric(EGRESOS.FINAL$`% COBERTURA`)


# CHEQUEO DONDE SE REGISTRA -----------------------------------------------

EGRESOS.FINAL.FINAL <- EGRESOS.FINAL

EGRESOS.FINAL.FINAL$REGISTRA <- ifelse(!is.na(EGRESOS.FINAL.FINAL$SRH) & is.na(EGRESOS.FINAL.FINAL$CIPRES), "Solo en SRH", NA)
EGRESOS.FINAL.FINAL$REGISTRA <- ifelse(is.na(EGRESOS.FINAL.FINAL$SRH) & !is.na(EGRESOS.FINAL.FINAL$CIPRES), "Solo en CIPRES", EGRESOS.FINAL.FINAL$REGISTRA)
EGRESOS.FINAL.FINAL$REGISTRA <- ifelse(!is.na(EGRESOS.FINAL.FINAL$SRH) & !is.na(EGRESOS.FINAL.FINAL$CIPRES), "SRH y CIPRES", EGRESOS.FINAL.FINAL$REGISTRA)
EGRESOS.FINAL.FINAL$REGISTRA <- ifelse(is.na(EGRESOS.FINAL.FINAL$SRH) & is.na(EGRESOS.FINAL.FINAL$CIPRES), "NO REGISTRA", EGRESOS.FINAL.FINAL$REGISTRA)


# REORDENO COLUMNAS DE TABLA FINAL ----------------------------------------

col_order <- c("REGION", "PARTIDO", "DEPENDENCIA", "COD.EST", "ESTABLECIMIENTO", "Mes", "SRH", "CIPRES", "% COBERTURA", "REGISTRA")

EGRESOS.FINAL.FINAL <-  dplyr::select(EGRESOS.FINAL.FINAL, one_of(col_order))


###########################################################################
# SALIDA ------------------------------------------------------------------

# 2021
# write_xlsx(EGRESOS.FINAL.FINAL, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2021_SRH-CIPRES_%Y-%m-%d.xlsx"),sep ="-"))))

# 2022
write_xlsx(EGRESOS.FINAL.FINAL, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022_SRH-CIPRES_%Y-%m-%d.xlsx"),sep ="-"))))

# VOY A LA RUTA DE ARCHIVO Y AGREGO AÑO A MANO DESPUES DE "Egresos-"

###########################################################################