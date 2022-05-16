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
# ABRO INTERVENCIONES QUIRURGICAS Y TABLA PROCED.QUIRURGICOS --------------

# INT.QUIR2021 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2021_total 04-2022/2021_INT_QUIRURGICAS.TXT")

PROC_QUIR <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Procedimientos Quirurgicos.xlsx")

# ABRO PRODUCTOS OBSTETRICOS Y VALIDACIONES -------------------------------

PROD.OBST2021 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2021_total 05-2022/2021_PRODOBSTETRICO.TXT")

VALIDACIONES <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/validaciones-Ale.xlsx",
                           sheet = "Hoja1",
                           col_types = c("skip", "text", "text",
                                         "skip", "skip", "skip", "text",
                                         "skip", "text", "text", "text",
                                         "text", "text", "text", "text", "text",
                                         "text", "text", "text", "text", "text"))

###########################################################################
# RENOMBRO BASES -----------------------------------------------------------

F10PO <- F10.2021 %>% 
  select(Form10.id, Cod.Est, Sector, Nro.Informe, Apellido.Nombre, Tipo.Doc, Nro.Doc,
         Sexo, Fecha.Nacimiento, Fecha.Ingreso, Servicio.Ingreso, Descr.Serv.Ingreso,
         Fecha.Egreso, Servicio.Egreso, Descr.Serv.Egreso, Tipo.Egreso, Dias.Estada,
         Cod.Diag.Ppal, Descr.Diag.Ppal, Cod.Causas.Ext, Descr.Causas.Ext, 
         Cod.Int.Quir1, Descr.Int.Quir1, Cod.Int.Quir2, Descr.Int.Quir2,
         Cod.Int.Quir3, Descr.Int.Quir3, Dias.ARM, Peso.gr.nacim)


PROB <- PROD.OBST2021
PROB$X <- NULL

F10PO <- F10PO %>% mutate(Form10.id = as.character(Form10.id),
                          Cod.Diag.Ppal = as.character(Cod.Diag.Ppal))

VALI <- VALIDACIONES %>% select(categoria4_id, INTQ1, INTQ2)

###########################################################################
# RENOMBRO VARIABLES ------------------------------------------------------


PROB <- PROB %>% rename(Form10.id = form10id,
                        Apellido.Nombre = apellido_nombre,
                        Tipo.Doc = tipo_doc,
                        Descr.Doc = tipo_doc_descrip)
PROB <- PROB %>% rename(Nro.Doc = nro_doc,
                        Fecha.Nacimiento = fecha_nac)

PROB <- PROB %>% rename(id.Prod.Obst = idprobos,
                        Peso.al.nacer = peso,
                        Condicion = condicion,
                        Terminacion = terminacion,
                        Sexo.al.nacer = sexo)

VALI <- VALI %>% rename(Cod.Diag.Ppal = categoria4_id)

# CAMBIO FORMATO VARIABLES PROB -------------------------------------------

PROB <- PROB %>% 
  mutate(Form10.id= as.character(Form10.id), 
         Apellido.Nombre = as.character(Apellido.Nombre), 
         Descr.Doc = as.character(Descr.Doc), 
         Nro.Doc = as.character(Nro.Doc), 
         Fecha.Nacimiento = as.character(Fecha.Nacimiento),
         
         id.Prod.Obst = as.character(id.Prod.Obst))


PROB <- PROB %>%
  mutate(Peso.al.nacer = as.numeric(Peso.al.nacer),
         Condicion = as.numeric(Condicion),
         Terminacion = as.numeric(Terminacion))


PROB <- PROB %>%
  mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento))
F10PO <- F10PO %>%
  mutate(Fecha.Nacimiento = lubridate::dmy(Fecha.Nacimiento))


PROB$Form10.id <- stringr::str_pad(PROB$Form10.id, 7, side = "left", pad = 0)
PROB$id.Prod.Obst <- stringr::str_pad(PROB$id.Prod.Obst, 6, side = "left", pad = 0)

PROB <- PROB %>% mutate(Form10.id = as.character(Form10.id))

f1 <- PROB
DUPidform <- f1[duplicated(f1$Form10.id) | duplicated(f1$Form10.id, fromLast=TRUE), ]
DUPidprob <- f1[duplicated(f1$id.Prod.Obst) | duplicated(f1$id.Prod.Obst, fromLast=TRUE), ]

DUPidform$Repetido.ID <- "Form10.id"

DUPprob <- merge(F10PO, DUPidform, all.y = TRUE)

###########################################################################
# UNO F10 CON PROB PARA VER FALTAS EN UN REGISTRO U OTRO ------------------

F10.PO <- merge(F10PO, PROB, all = TRUE)

# FILTRO F10 POR DIAGNOSTICOS -> O ----------------------------------------


F10.PO$Prod.Obst <- ifelse( 
  (substr(F10.PO$Cod.Diag.Ppal, 1, 1) == "O"), "Diag.Ppal es O", NA)

F10.PO$Tabla.anexa <- ifelse(!is.na(F10.PO$id.Prod.Obst), "Esta en tabla anexa", NA)

F10.PO. <- F10.PO[(!is.na(F10.PO$Prod.Obst) | !is.na(F10.PO$Tabla.anexa)) ,]


# # VEO Form10.id QUE NO ESTA EN PROB -----------------------------------
# 
# F10.PO.$NO.F10.en.PROB <- ifelse(is.na(F10.PO.$id.Prod.Obst), "No tiene PRODUCTO OBSTETRICO", NA)
# 
# ###########################################################################
# # UNO A VALIDACIONES PARA VER INTERVENCIONES QUIR -------------------------
# 
# F10POIQ <- merge(F10.PO., VALI, all.x = TRUE)
# 
# # VALORES UNICOS DE DIAGNOSTICOS
# 
# diagnosticos <- F10POIQ %>% 
#   select(Cod.Diag.Ppal)
# 
# diagnosticos <- diagnosticos %>% 
#   distinct()



# F10IQPO <- merge(x = F10Intq, y = F10Prob, all = TRUE)
# F10IQPO$Solo.en.F10 <- ifelse(!is.na(F10IQPO$NO.F10.en.INTQ) & !is.na(F10IQPO$NO.F10.en.PROB), "Solo esta en F10", NA)

write.xlsx(F10.PO, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2021_PROB.xlsx", overwrite = TRUE)


# 2021
write_xlsx(F10.PO, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10//Validados/F10-2021_PROB.%Y.%m.%d.xlsx"),sep ="."))))