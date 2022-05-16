###########################################################################
# LIBRERIAS ---------------------------------------------------------------


library(data.table)
library(datos)
library(dplyr)
library(ggplot2)
library(labelled)
library(lubridate)
library(magrittr)
library(openxlsx)
library(readr)
library(readxl)
library(stringr)
library(tidyverse)
library(writexl)
library(zoo)


###########################################################################
# ABRO SERVICIOS -----------------------------------------

SERVICIOS2021 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2021_total 05-2022/2021_SERVICIOS.TXT")
SERVICIOS2022 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2022_total 05-2022/2022_SERVICIOS.TXT")


# RENOMBRO BASE PARA NO VOLVER A ABRIRLA ----------------------------------

SERV <- SERVICIOS2021
# SERV <- SERVICIOS2022


# CAMBIO FORMATO Y NOMBRE DE LAS VARIABLES --------------------------------


SERV <- SERV %>% rename(Form10_id = form10id)
SERV <- SERV %>% rename(Apellido.Nombre = apellido_nombre)
SERV <- SERV %>% rename(Tipo.Doc = tipo_doc)
SERV <- SERV %>% rename(Descr.Doc = tipo_doc_descrip)
SERV <- SERV %>% rename(Nro.Doc = nro_doc)
SERV <- SERV %>% rename(Fecha.Nacimiento = fecha_nac)
SERV <- SERV %>% rename(Fecha.Ingreso.Serv = fecha_ingreso_serv)
SERV <- SERV %>% rename(Dias.Estada.Servicio = dias_estada)
SERV <- SERV %>% rename(orden.Servicio = orden)


# F10 <- F10 %>% rename(Servicio.Ingreso = serv_ingreso)
# F10 <- F10 %>% rename(Descr.Serv.Ingreso = serv_ingreso_descrip)


# ELIMINO COLUMNA X
SERV$X <- NULL


# FORMATO DE VARIABLES COMO TEXTO
SERV <- SERV %>%
  mutate(Form10_id= as.character(Form10_id), Apellido.Nombre= as.character(Apellido.Nombre), 
         Fecha.Nacimiento = as.character(Fecha.Nacimiento), Fecha.Ingreso.Serv = as.character(Fecha.Ingreso.Serv),
         serv_id = as.character(serv_id), Descr.Doc = as.character(Descr.Doc))

# FORMATO DE VARIABLES COMO FECHA
SERV <- SERV %>%
  mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento), Fecha.Ingreso.Serv = lubridate::dmy(Fecha.Ingreso.Serv))
  
# FORMATO DE VARIABLES DE id
SERV$Form10_id <- stringr::str_pad(SERV$Form10_id, 7, side = "left", pad = 0)
SERV$serv_id <- stringr::str_pad(SERV$serv_id, 3, side = "left", pad = 0)

# FORMATO DE VARIABLES COMO NUMERO ENTERO
SERV <- SERV %>%
  mutate(orden.Servicio = as.integer(orden.Servicio), Dias.Estada.Servicio = as.integer(Dias.Estada.Servicio))


# GENERO ID_CONTROL: FORM10_ID + COD.SERV ---------------------------------

SERV$id.Control <- paste(SERV$Form10_id, SERV$serv_id, sep = "|")


SERV <- SERV %>%  mutate(Dias.Estada.Servicio = as.integer(Dias.Estada.Servicio))

SERV$VAL.SERV <- ifelse(
  (SERV$serv_id == "035" | SERV$serv_id == "135" | SERV$serv_id == "436"
   | SERV$serv_id == "437" | SERV$serv_id == "438")
  & (SERV$Dias.Estada.Servicio > 2),
  "ERROR SERV. GUARDIA SEGUN ESTADA", NA)

# 035
# 135
# 436
# 437
# 438

SERV$VAL.SERV <- ifelse(
  (SERV$Dias.Estada.Servicio < 0),
  "ERROR SERV. DIAS ESTADA", SERV$VAL.SERV)

###########################################################################
# NECESITO TENER CARGADO F10.DIAG.REP ----------------------------------------------
###########################################################################
# EJECUTA SCRIPT DIAGNOSTICOS.2021 ----------------------------------------
# 
# # DIAGNOSTICOS.R: 
# 
# source("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/RLu/F10/DIAGNOSTICOS.R")
# 
# UNO CON F10.DIAG.REP ----------------------------------------------------
# F10.DIAG.REP$id.Control <- paste(F10.DIAG.REP$Form10_id, F10.DIAG.REP$Servicio.Egreso, sep = "|")
#   
# F10.DIAG.REP.SERV <- merge(F10.DIAG.REP, SERV, all.x = TRUE)
# 
# F10.DIAG.REP.SERV <- F10.DIAG.REP.SERV %>% 
#   select(Form10_id, RS, Partido, Cod.Est, Descr.Establecimiento, Sector, Nro.Informe, 
#          Apellido.Nombre, Tipo.Doc, Descr.Doc, Nro.Doc, Sexo, Fecha.Nacimiento,
#          Fecha.Ingreso, Servicio.Ingreso, Descr.Serv.Ingreso, 
#          Fecha.Egreso, Servicio.Egreso, Descr.Serv.Egreso, Tipo.Egreso, Dias.Estada,
#          id.Control, Fecha.Ingreso.Serv, serv_id, Dias.Estada.Servicio, orden.Servicio, 
#          orden.Diagnostico, Cod.Diag.Ppal, Descr.Diag.Ppal, Cod.Causas.Ext, Descr.Causas.Ext, 
#          Tipo.Lesion, Lugar.Lesion, id.Diagnostico, Tipo.Diag)


# SALIDA ------------------------------------------------------------------

# write.xlsx(F10.DIAG.REP.SERV, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validados/F10-2021_FORM10_ID-REPETIDOS-SERVICIOS.xlsx", overwrite = TRUE)


