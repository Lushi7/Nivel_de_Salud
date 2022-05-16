###########################################################################
# LIBRERIAS ---------------------------------------------------------------


library(data.table)
library(datos)
library(dplyr)
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
# TENGO QUE TENER F10 CARGADO ---------------------------------------------

F10REP <- F10.2021 %>% 
  select(Form10.id, Cod.Est, Sector, Nro.Informe, Apellido.Nombre, Tipo.Doc, Nro.Doc,
         Sexo, Fecha.Nacimiento, Fecha.Ingreso, Servicio.Ingreso, Descr.Serv.Ingreso,
         Fecha.Egreso, Servicio.Egreso, Descr.Serv.Egreso, Tipo.Egreso, Dias.Estada,
         Cod.Diag.Ppal, Descr.Diag.Ppal, Cod.Causas.Ext, Descr.Causas.Ext)


# # FORMATO DE VARIABLES COMO TEXTO
# F10REP <- F10REP %>%
#   mutate(Form10.id= as.character(Form10.id),
#          Apellido.Nombre= as.character(Apellido.Nombre),
#          Fecha.Nacimiento = as.character(Fecha.Nacimiento),
#          Fecha.Ingreso = as.character(Fecha.Ingreso),
#          Servicio.Egreso = as.character(Servicio.Egreso))
# 
# # FORMATO DE VARIABLES COMO FECHA
# F10REP <- F10REP %>%
#   mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento),
#          Fecha.Ingreso = lubridate::dmy(Fecha.Ingreso))
# 
# # FORMATO DE VARIABLES DE id
# F10REP$Form10.id <- stringr::str_pad(F10REP$Form10.id, 7, side = "left", pad = 0)
# F10REP$Servicio.Egreso <- stringr::str_pad(F10REP$Servicio.Egreso, 3, side = "left", pad = 0)
# 
# # FORMATO DE VARIABLES COMO NUMERO ENTERO
# F10REP <- F10REP %>%
#   mutate(Dias.Estada = as.integer(Dias.Estada))

###########################################################################

f1 <- F10REP

# NROS DE DOCUMENTOS REPETIDOS
DUPdni <- f1[duplicated(f1$Nro.Doc) | duplicated(f1$Nro.Doc, fromLast=TRUE), ]

DUPdni$Repetido.DOC <- "Nro.Doc"

## F10.REP <- merge(F10.2021, DUPdni, by.all="name_of_column_in_common", all.x=T)
F10.DNI <- left_join(F10.2021, unique(DUPdni))

ORDEN.FINAL <- c("Form10.id", "Region.Sanitaria", "Partido", "Cod.Est", "Establecimiento", "Sector",
                 "Nro.Informe", "Mes", "Año", 
                 "Codigo.Residencia", "Reside.Pais", "Reside.Prov", "Reside.Partido", "Reside.Localidad",
                 "Apellido.Nombre", "Tipo.Doc", "Descr.Doc", "Nro.Doc", "Fecha.Nacimiento", "Sexo", "Descr.Sexo",
                 "Edad.id", "Descr.Edad.id", "Cant.Edad", "Grupo.Etario",
                 "Fecha.Ingreso", "Servicio.Ingreso", "Descr.Serv.Ingreso", 
                 "Fecha.Egreso", "Servicio.Egreso", "Descr.Serv.Egreso",
                 "Tipo.Egreso", "Descr.Tipo.Egreso", "Dias.Estada",
                 "Cap.CIE10", "Descr.Cap.CIE10", "Grupo.CIE10", "Descr.Grp.CIE10",
                 "Cod.Diag.Ppal", "Descr.Diag.Ppal", "Cod.Causas.Ext", "Descr.Causas.Ext", 
                 "Tipo.Lesion", "Descr.Lesion.Tipo", "Lugar.Lesion", "Descr.Lesion.Lugar",
                 "Dias.ARM", "Obra.Social", "Descr.Obra.Social",
                 "Cod.Int.Quir1", "Descr.Int.Quir1", "Cod.Int.Quir2", "Descr.Int.Quir2", "Cod.Int.Quir3", "Descr.Int.Quir3",
                 "Peso.gr.nacim", "Nivel.Instruccion", "Situacion.Laboral", "Ocupacion.Actual",
                 "Repetido.ID", "Repetido.DOC")



F10DNI <- dplyr::select(F10.DNI, any_of(ORDEN.FINAL))

# write.xlsx(F10DNI, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/DNI_REPETIDOS.xlsx", overwrite = TRUE)

###########################################################################
# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------
f1 <- F10REP

# FORM10_ID REPETIDOS
DUPid <- f1[duplicated(f1$Form10.id) | duplicated(f1$Form10.id, fromLast=TRUE), ]

DUPid$Repetido.ID <- "Form10.id"

# write.xlsx(DUPid, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/Form10id_REPETIDOS.xlsx", overwrite = TRUE)


## F10.REP <- merge(F10.2021, DUPdni, by.all="name_of_column_in_common", all.x=T)
F10.ID <- left_join(F10.2021, unique(DUPid))

ORDEN.FINAL <- c("Form10.id", "Region.Sanitaria", "Partido", "Cod.Est", "Establecimiento", "Sector",
                 "Nro.Informe", "Mes", "Año", 
                 "Codigo.Residencia", "Reside.Pais", "Reside.Prov", "Reside.Partido", "Reside.Localidad",
                 "Apellido.Nombre", "Tipo.Doc", "Descr.Doc", "Nro.Doc", "Fecha.Nacimiento", "Sexo", "Descr.Sexo",
                 "Edad.id", "Descr.Edad.id", "Cant.Edad", "Grupo.Etario",
                 "Fecha.Ingreso", "Servicio.Ingreso", "Descr.Serv.Ingreso", 
                 "Fecha.Egreso", "Servicio.Egreso", "Descr.Serv.Egreso",
                 "Tipo.Egreso", "Descr.Tipo.Egreso", "Dias.Estada",
                 "Cap.CIE10", "Descr.Cap.CIE10", "Grupo.CIE10", "Descr.Grp.CIE10",
                 "Cod.Diag.Ppal", "Descr.Diag.Ppal", "Cod.Causas.Ext", "Descr.Causas.Ext", 
                 "Tipo.Lesion", "Descr.Lesion.Tipo", "Lugar.Lesion", "Descr.Lesion.Lugar",
                 "Dias.ARM", "Obra.Social", "Descr.Obra.Social",
                 "Cod.Int.Quir1", "Descr.Int.Quir1", "Cod.Int.Quir2", "Descr.Int.Quir2", "Cod.Int.Quir3", "Descr.Int.Quir3",
                 "Peso.gr.nacim", "Nivel.Instruccion", "Situacion.Laboral", "Ocupacion.Actual",
                 "Repetido.ID", "Repetido.DOC")



F10ID <- dplyr::select(F10.ID, any_of(ORDEN.FINAL))

# write.xlsx(F10ID, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/Form10id_REPETIDOS.xlsx", overwrite = TRUE)

###########################################################################
# UNO F10 CON COLUMNAS DE DUPLICADOS --------------------------------------

DUP <- merge(DUPid, DUPdni, all = TRUE)

F10.DUP <- left_join(F10.2021, unique(DUP))

ORDEN.FINAL <- c("Form10.id", "Region.Sanitaria", "Partido", "Cod.Est", "Establecimiento", "Sector",
                 "Nro.Informe", "Mes", "Año", 
                 "Codigo.Residencia", "Reside.Pais", "Reside.Prov", "Reside.Partido", "Reside.Localidad",
                 "Apellido.Nombre", "Tipo.Doc", "Descr.Doc", "Nro.Doc", "Fecha.Nacimiento", "Sexo", "Descr.Sexo",
                 "Edad.id", "Descr.Edad.id", "Cant.Edad", "Grupo.Etario",
                 "Fecha.Ingreso", "Servicio.Ingreso", "Descr.Serv.Ingreso", 
                 "Fecha.Egreso", "Servicio.Egreso", "Descr.Serv.Egreso",
                 "Tipo.Egreso", "Descr.Tipo.Egreso", "Dias.Estada",
                 "Cap.CIE10", "Descr.Cap.CIE10", "Grupo.CIE10", "Descr.Grp.CIE10",
                 "Cod.Diag.Ppal", "Descr.Diag.Ppal", "Cod.Causas.Ext", "Descr.Causas.Ext", 
                 "Tipo.Lesion", "Descr.Lesion.Tipo", "Lugar.Lesion", "Descr.Lesion.Lugar",
                 "Dias.ARM", "Obra.Social", "Descr.Obra.Social",
                 "Cod.Int.Quir1", "Descr.Int.Quir1", "Cod.Int.Quir2", "Descr.Int.Quir2", "Cod.Int.Quir3", "Descr.Int.Quir3",
                 "Peso.gr.nacim", "Nivel.Instruccion", "Situacion.Laboral", "Ocupacion.Actual",
                 "Repetido.ID", "Repetido.DOC")


F10.REP <- dplyr::select(F10.DUP, any_of(ORDEN.FINAL))


write.xlsx(F10.REP, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/Form10id-y-DNI_REPETIDOS.xlsx", overwrite = TRUE)


###########################################################################

# f1$DUPid <- duplicated(f1[,c("Form10.id")],)
# f1$DUPdni <- duplicated(f1[,c("Nro.Doc")],)
# 
# f1$DUPid <- ifelse()

# F <- filter(f1, DUP == FALSE)
# 
# F$DUP <- NULL

dni <- DUPdni %>% 
  select(Nro.Doc)

# VALORES UNICOS
dni <- dni %>% 
  distinct()

# write.xlsx(dni, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/DNIs_UNICOS.xlsx", overwrite = TRUE)
# write.xlsx(DUPdni, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/DNIs_REPETIDOS.xlsx", overwrite = TRUE)


DUPdni$Fecha.Ingreso <- as.Date(DUPdni$Fecha.Ingreso, format = "%d/%m/%Y")
DUPdni$Fecha.Egreso <- as.Date(DUPdni$Fecha.Egreso, format = "%d/%m/%Y")

###########################################################################
###########################################################################
# ABRO SERVICIOS -----------------------------------------

SERVICIOS2021 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2021_total 05-2022/2021_SERVICIOS.TXT")
# SERVICIOS2022 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2022_total 04-2022/2022_SERVICIOS.TXT")

# RENOMBRO BASE PARA NO VOLVER A ABRIRLA ----------------------------------

SERV <- SERVICIOS2021
# SERV <- SERVICIOS2022

f1 <- F10REP

# FORM10_ID REPETIDOS
DUPid <- f1[duplicated(f1$Form10.id) | duplicated(f1$Form10.id, fromLast=TRUE), ]

# CAMBIO FORMATO Y NOMBRE DE LAS VARIABLES --------------------------------


SERV <- SERV %>% rename(Form10.id = form10id)
SERV <- SERV %>% rename(Apellido.Nombre = apellido_nombre)
SERV <- SERV %>% rename(Tipo.Doc = tipo_doc)
SERV <- SERV %>% rename(Descr.Doc = tipo_doc_descrip)
SERV <- SERV %>% rename(Nro.Doc = nro_doc)
SERV <- SERV %>% rename(Fecha.Nacimiento = fecha_nac)
SERV <- SERV %>% rename(Fecha.Ingreso.Serv = fecha_ingreso_serv)
SERV <- SERV %>% rename(Dias.Estada.Servicio = dias_estada)
SERV <- SERV %>% rename(orden.Servicio = orden)
SERV <- SERV %>% rename(Servicio.Egreso = serv_id)


# F10 <- F10 %>% rename(Servicio.Ingreso = serv_ingreso)
# F10 <- F10 %>% rename(Descr.Serv.Ingreso = serv_ingreso_descrip)


# ELIMINO COLUMNA X
SERV$X <- NULL


# FORMATO DE VARIABLES COMO TEXTO
SERV <- SERV %>%
  mutate(Form10.id= as.character(Form10.id), 
         Apellido.Nombre= as.character(Apellido.Nombre), 
         Fecha.Ingreso.Serv = as.character(Fecha.Ingreso.Serv),
         Servicio.Egreso = as.character(Servicio.Egreso), 
         Tipo.Doc = as.character(Tipo.Doc),
         Descr.Doc = as.character(Descr.Doc),
         Nro.Doc = as.character(Nro.Doc),
         Fecha.Nacimiento = as.character(Fecha.Nacimiento),)

# FORMATO DE VARIABLES COMO FECHA
SERV <- SERV %>%
  mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento), 
         Fecha.Ingreso.Serv = lubridate::dmy(Fecha.Ingreso.Serv))

# FORMATO DE VARIABLES DE id
SERV$Form10.id <- stringr::str_pad(SERV$Form10.id, 7, side = "left", pad = 0)
SERV$Servicio.Egreso <- stringr::str_pad(SERV$Servicio.Egreso, 3, side = "left", pad = 0)

# FORMATO DE VARIABLES COMO NUMERO ENTERO
SERV <- SERV %>%
  mutate(orden.Servicio = as.integer(orden.Servicio), 
         Dias.Estada.Servicio = as.integer(Dias.Estada.Servicio))

#### DUPid
# FORMATO DE VARIABLES COMO TEXTO
DUPid <- DUPid %>%
  mutate(Form10.id= as.character(Form10.id), 
         Apellido.Nombre= as.character(Apellido.Nombre), 
         Fecha.Nacimiento = as.character(Fecha.Nacimiento), 
         Fecha.Ingreso = as.character(Fecha.Ingreso),
         Servicio.Egreso = as.character(Servicio.Egreso),
         Tipo.Doc = as.character(Tipo.Doc),
         Nro.Doc = as.character(Nro.Doc))

# FORMATO DE VARIABLES COMO FECHA
DUPid <- DUPid %>%
  mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento), 
         Fecha.Ingreso = lubridate::dmy(Fecha.Ingreso))

# FORMATO DE VARIABLES DE id
DUPid$Form10.id <- stringr::str_pad(DUPid$Form10.id, 7, side = "left", pad = 0)
DUPid$Servicio.Egreso <- stringr::str_pad(DUPid$Servicio.Egreso, 3, side = "left", pad = 0)

# FORMATO DE VARIABLES COMO NUMERO ENTERO
DUPid <- DUPid %>%
  mutate(Dias.Estada = as.integer(Dias.Estada))

REP.FORM.SERV <- merge(DUPid, SERV, all.x =TRUE)
