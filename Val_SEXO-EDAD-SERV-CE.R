###########################################################################
# Script que contiene reglas de validación según SEXO, EDAD, SERVICIO,
# CAUSAS EXTERNAS y TIPO DE EGRESO para generar reparos de F10 (CIPRES)
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
# ABRO VALIDACIONES.XLSX --------------------------------------------------

VALIDACIONES <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/validaciones-Ale.xlsx", 
                           col_types = c("skip", "text", "text", 
                                         "skip", "skip", "skip", "text", 
                                         "skip", "text", "text", "text", 
                                         "text", "text", "text", "text", "text",
                                         "text", "text", "text", "text", "text"))

# RENOMBRO BASES PARA NO VOLVER A ABRIRLAS --------------------------------

VALI <- VALIDACIONES %>% 
  select(categoria4_id, Nombre, `Limitada a un sexo`, LI, LS, LIM_INF, LIM_SUP, `TIPO EGRESO`)


# FORMATO Y NOMBRE VARIABLES EN VALIDACIONES ------------------------------

VALI <- VALI %>% 
  mutate(`Limitada a un sexo` = as.numeric(`Limitada a un sexo`),
         LIM_INF = as.numeric(LIM_INF), LIM_SUP = as.numeric(LIM_SUP))

VALI <- VALI %>% rename(Cod.Diag.Ppal = categoria4_id,
                        TIPO.EGRESO = `TIPO EGRESO`)

###########################################################################
# NECESITO TENER CARGADO F10 ----------------------------------------------

# F10-2021: 

# source("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/RLu/F10-2022/F10 - 2010-2021/F10-2010.R")

# F10 <- F10.2021
F10 <- F10.2022

###########################################################################
###########################################################################
# SELECCIONO COLUMNAS DE F10 QUE NECESITO ---------------------------------



F10V <- F10 %>%
  select(Form10.id, Region.Sanitaria, Partido, Cod.Est, Establecimiento, Sector, Nro.Informe, Mes,
         Apellido.Nombre, Tipo.Doc, Descr.Doc, Nro.Doc, Sexo, Descr.Sexo, Fecha.Nacimiento,
         Edad.id, Descr.Edad.id, Cant.Edad, Grupo.Etario,
         Fecha.Ingreso, Servicio.Ingreso, Descr.Serv.Ingreso, 
         Fecha.Egreso, Servicio.Egreso, Descr.Serv.Egreso, Tipo.Egreso, Descr.Tipo.Egreso, Dias.Estada,
         Cap.CIE10, Grupo.CIE10,
         Cod.Diag.Ppal, Descr.Diag.Ppal, Cod.Causas.Ext, Descr.Causas.Ext, 
         Tipo.Lesion, Descr.Lesion.Tipo, Lugar.Lesion, Descr.Lesion.Lugar)


# UNO F10 CON VALIDACIONES ------------------------------------------------

F10.VAL <- merge(F10V, VALI, by = "Cod.Diag.Ppal", all.x = TRUE)


###########################################################################
###########################################################################
# #### VALIDACION POR SEXO ####  ------------------------------------------

# DIAGNOSTICOS QUE NO SE CORRESPONDEN CON SEXO DECLARADO ---------------- -

F10.VAL$VAL.SEXO <- ifelse (F10.VAL$Sexo == 1 & F10.VAL$`Limitada a un sexo` == 2, 
                            "ERROR SEXO DIAG.F", NA)

F10.VAL$VAL.SEXO <- ifelse (F10.VAL$Sexo == 2 & F10.VAL$`Limitada a un sexo` == 1, 
                            "ERROR SEXO DIAG.M", F10.VAL$VAL.SEXO)

F10.VAL$VAL.SEXO <- ifelse (!(F10.VAL$Sexo == 1 | F10.VAL$Sexo == 2) | is.na(F10.VAL$Sexo), 
                            "ERROR SEXO", F10.VAL$VAL.SEXO)

###########################################################################
###########################################################################
# #### VALIDACION POR EDAD #### -------------------------------------------
# CAMBIO NOMBRES INTERVALOS EDAD.ID ---------------------


F10.VAL$EDAD.id <- ifelse(F10.VAL$Edad.id ==  1, "A", NA)
F10.VAL$EDAD.id <- ifelse(F10.VAL$Edad.id ==  2, "M", F10.VAL$EDAD.id)
F10.VAL$EDAD.id <- ifelse(F10.VAL$Edad.id ==  3, "D", F10.VAL$EDAD.id)

F10.VAL$EDAD.id <- ifelse(F10.VAL$EDAD.id ==  "A", 3, F10.VAL$EDAD.id)
F10.VAL$EDAD.id <- ifelse(F10.VAL$EDAD.id ==  "M", 2, F10.VAL$EDAD.id)
F10.VAL$EDAD.id <- ifelse(F10.VAL$EDAD.id ==  "D", 1, F10.VAL$EDAD.id)

F10.VAL$Cant.Edad. <- stringr::str_pad(F10.VAL$Cant.Edad, 3, side = "left", pad = 0)

F10.VAL$ID.EDAD <- paste(F10.VAL$EDAD.id, F10.VAL$Cant.Edad., sep = "")


# FORMATO DE EDADES Y LIMITES INF/SUP -------------------------------------

F10.VAL <- F10.VAL %>% 
  mutate(LI = as.character(LI),
         LS = as.character(LS))


F10.VAL$LI. <- substr(F10.VAL$LI, 4,5)
F10.VAL$LS. <- substr(F10.VAL$LS, 4,5)

F10.VAL$.LI. <- substr(F10.VAL$LI, 1,3)
F10.VAL$.LS. <- substr(F10.VAL$LS, 1,3)

F10.VAL$LI. <- ifelse(F10.VAL$LI. ==  "H", 0, F10.VAL$LI.)
F10.VAL$LI. <- ifelse(F10.VAL$LI. ==  "A", 3, F10.VAL$LI.)
F10.VAL$LI. <- ifelse(F10.VAL$LI. ==  "M", 2, F10.VAL$LI.)
F10.VAL$LI. <- ifelse(F10.VAL$LI. ==  "D", 1, F10.VAL$LI.)

F10.VAL$LS. <- ifelse(F10.VAL$LS. ==  "H", 0, F10.VAL$LS.)
F10.VAL$LS. <- ifelse(F10.VAL$LS. ==  "A", 3, F10.VAL$LS.)
F10.VAL$LS. <- ifelse(F10.VAL$LS. ==  "M", 2, F10.VAL$LS.)
F10.VAL$LS. <- ifelse(F10.VAL$LS. ==  "D", 1, F10.VAL$LS.)

F10.VAL$LIM_INF. <- paste(F10.VAL$LI., F10.VAL$.LI., sep = "")
F10.VAL$LIM_SUP. <- paste(F10.VAL$LS., F10.VAL$.LS., sep = "")

F10.VAL$LIM_INF. <- ifelse(F10.VAL$LIM_INF. == "NANA", NA, F10.VAL$LIM_INF.)
F10.VAL$LIM_SUP. <- ifelse(F10.VAL$LIM_SUP. == "NANA", NA, F10.VAL$LIM_SUP.)

F10.VAL$LI. <- NULL
F10.VAL$LS. <- NULL
F10.VAL$.LI. <- NULL
F10.VAL$.LS. <- NULL

# DIAGNOSTICOS FUERA DE RANGOS ETARIOS DEFINIDOS --------------------------

# DEFINO FORMATO VARIABLES PARA PODER COMPARAR 
F10.VAL <- F10.VAL %>% 
  mutate(LIM_INF. = as.numeric(LIM_INF.), LIM_SUP. = as.numeric(LIM_SUP.))


F10.VAL$VAL.EDAD <- ifelse(F10.VAL$ID.EDAD < F10.VAL$LIM_INF. | 
    F10.VAL$ID.EDAD > F10.VAL$LIM_SUP., "ERROR DIAG.EDAD", NA)
F10.VAL$VAL.EDAD <- ifelse(F10.VAL$Cant.Edad < 0, "ERROR EDAD", F10.VAL$VAL.EDAD)
F10.VAL$VAL.EDAD <- ifelse(F10.VAL$Cant.Edad >= 120, "ERROR EDAD", F10.VAL$VAL.EDAD)


# BORRO COLUMNAS DE VALIDACIONES

F10.VAL$LI <- NULL
F10.VAL$LS <- NULL
F10.VAL$LIM_INF <- NULL
F10.VAL$LIM_SUP <- NULL
F10.VAL$LIM_INF. <- NULL
F10.VAL$LIM_SUP. <- NULL

############################################################################
############################################################################
# #### VALIDACION POR SERVICIO #### ----------------------------------------

F10.VAL$VAL.SERV <- ifelse(
      (F10.VAL$Servicio.Ingreso == "132"  | F10.VAL$Servicio.Egreso == "132") 
  & !( F10.VAL$ID.EDAD >= 0 & F10.VAL$ID.EDAD <= (1028)),
  "ERROR SERV. NEONATOLOGIA", NA)

F10.VAL$VAL.SERV <- ifelse(
  (F10.VAL$Servicio.Ingreso == "042"  | F10.VAL$Servicio.Egreso == "042") 
  & !( F10.VAL$ID.EDAD >= 3010 & F10.VAL$ID.EDAD <= 3050 ),
  "ERROR SERV. OBSTETRICIA MATERNIDAD", F10.VAL$VAL.SERV)

F10.VAL$VAL.SERV <- ifelse(
  (F10.VAL$Servicio.Ingreso == "199"  | F10.VAL$Servicio.Egreso == "199") 
  & !( F10.VAL$ID.EDAD >= 0 & F10.VAL$ID.EDAD <= 3016 ),
  "ERROR SERV. PEDIATRIA-LACTANTES", F10.VAL$VAL.SERV)


# ERRORES POR SERVICIO - TIPO EGRESO --------------------------------------

servicios <- F10.VAL %>% 
  select(Servicio.Egreso, Descr.Serv.Egreso)

# VALORES UNICOS
servicios <- servicios %>% 
  distinct()

# write.xlsx(servicios, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Servicios_en_F10.xlsx", overwrite = TRUE)


# 073 TERAPIA INTENSIVA
# 173	TERAPIA INTENSIVA INFANTIL
# 183	CUIDADOS  INTENSIVOS NEONATOLOGICOS
# 184	CUIDADOS INTENSIVOS  PEDIATRICOS
# 573	TERAPIA INTENSIVA NEONATOLOGICA
# 673	CUIDADOS INTENSIVOS - PROGRESIVOS

F10.VAL$VAL.SERV <- ifelse(F10.VAL$Tipo.Egreso == "1"
                           & (F10.VAL$Servicio.Egreso == "073"
                              | F10.VAL$Servicio.Egreso == "173"
                              | F10.VAL$Servicio.Egreso == "183"
                              | F10.VAL$Servicio.Egreso == "184"
                              | F10.VAL$Servicio.Egreso == "573"
                              | F10.VAL$Servicio.Egreso == "673"
                              | F10.VAL$Servicio.Egreso == "773"
                              | F10.VAL$Servicio.Egreso == "873"),
                           "ERROR SERV. Egreso Alta definitiva", F10.VAL$VAL.SERV)

F10.VAL$VAL.SERV <- ifelse(F10.VAL$Tipo.Egreso == "5"
                           & (F10.VAL$Servicio.Egreso == "073"
                              | F10.VAL$Servicio.Egreso == "173"
                              | F10.VAL$Servicio.Egreso == "183"
                              | F10.VAL$Servicio.Egreso == "184"
                              | F10.VAL$Servicio.Egreso == "573"
                              | F10.VAL$Servicio.Egreso == "673"
                              | F10.VAL$Servicio.Egreso == "773"
                              | F10.VAL$Servicio.Egreso == "873"),
                           "ERROR SERV. Egreso Retiro voluntario", F10.VAL$VAL.SERV)


# servvvv <- unique(servicios$Servicio.Egreso, servicios$Descr.Serv.Egreso)

# F10.VAL <- F10.VAL %>%  mutate(Dias.Estada = as.integer(Dias.Estada))
# 
# F10.VAL$VAL.SERV <- ifelse(
#   (F10.VAL$Servicio.Egreso == "035")
#   & (F10.VAL$Dias.Estada > 2),
#   "ERROR SERV. GUARDIA SEGUN ESTADA", F10.VAL$VAL.SERV)

# 035
# 135
# 436
# 437
# 438


###########################################################################
###########################################################################
# #### CAUSAS EXTERNAS #### -----------------------------------------------

# Cod.Diag.Ppal [S-T] NO ESTA EN CAP.XIX ------------------------------------ -

F10.VAL$Errores.diag.CE <- ifelse(
  ( (substr(F10.VAL$Cod.Diag.Ppal,1,1) == "S" | substr(F10.VAL$Cod.Diag.Ppal, 1, 1) == "T") 
    & F10.VAL$Cap.CIE10 != 19), "C.EXT Cod.Diag.Ppal [S-T] NO EN CAP XIX", NA)


# Cod.Diag.Ppal [V-Y] NO ESTA EN CAP.XX ------------------------------------- -

F10.VAL$Errores.diag.CE <- ifelse(
  (substr(F10.VAL$Cod.Diag.Ppal,1,1) == "V" | substr(F10.VAL$Cod.Diag.Ppal,1,1) == "W" |
     substr(F10.VAL$Cod.Diag.Ppal,1,1) == "X" | substr(F10.VAL$Cod.Diag.Ppal,1,1) == "Y") 
  & (F10.VAL$Cap.CIE10 != 20), 
  "C.EXT Cod.Diag.Ppal [V-Y] NO EN CAP XX", F10.VAL$Errores.diag.CE)


# Cod.Diag.Ppal ES [S-T] PERO Cod.Causas.Ext NO ES [V-Y] -------------------- -

F10.VAL$Errores.diag.CE <- ifelse(
  ((substr(F10.VAL$Cod.Diag.Ppal,1,1) == "S" | substr(F10.VAL$Cod.Diag.Ppal, 1, 1) == "T") # Cod.Diag.Ppal EST? EN S o T
   & !(substr(F10.VAL$Cod.Causas.Ext,1,1) == "V" |       # NO ESTA ENTRE [V y T]
         substr(F10.VAL$Cod.Causas.Ext,1,1) == "W" |
         substr(F10.VAL$Cod.Causas.Ext,1,1) == "X" |
         substr(F10.VAL$Cod.Causas.Ext,1,1) == "Y"))
  , "C.EXT Cod.Diag.Ppal [S-T] PERO Cod.Causas.Ext NO ES [V-Y]", F10.VAL$Errores.diag.CE)


# Cod.Causas.Ext ES [V-Y] PERO EN Cod.Diag.Ppal NO ES [S-T] ----------------- -

F10.VAL$Errores.diag.CE <- ifelse(
  (substr(F10.VAL$Cod.Causas.Ext,1,1) == "V" | substr(F10.VAL$Cod.Causas.Ext,1,1) == "W" |
     substr(F10.VAL$Cod.Causas.Ext,1,1) == "X" |substr(F10.VAL$Cod.Causas.Ext,1,1) == "Y") 
  & !(substr(F10.VAL$Cod.Diag.Ppal,1,1) == "S" | substr(F10.VAL$Cod.Diag.Ppal,1,1) == "T")
  , "C.EXT Cod.Causas.Ext [V-T] PERO Cod.Diag.Ppal NO ES [S-T]", F10.VAL$Errores.diag.CE)

# VERIFICO TIPO DE LESION POR CAUSA EXTERNA (ACCIDENTE...) ----------------

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Tipo.Lesion == 1 & !((F10.VAL$Cod.Diag.Ppal >= "V01" & F10.VAL$Cod.Diag.Ppal <= "X60") |
                                    (F10.VAL$Cod.Causas.Ext >= "V01" & F10.VAL$Cod.Causas.Ext <= "X60")))
  , "C.EXT TIPO LESION - 1", NA)

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Tipo.Lesion == 2 & !((F10.VAL$Cod.Diag.Ppal >= "X60" & F10.VAL$Cod.Diag.Ppal <= "X85") |
                                    (F10.VAL$Cod.Causas.Ext >= "X60" & F10.VAL$Cod.Causas.Ext <= "X85")))
  , "C.EXT TIPO LESION - 2", F10.VAL$Errores.tipo.y.lugar.lesion)


F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Tipo.Lesion == 3 & !((F10.VAL$Cod.Diag.Ppal >= "X85" & F10.VAL$Cod.Diag.Ppal <= "Y10") |
                                    (F10.VAL$Cod.Causas.Ext >= "X85" & F10.VAL$Cod.Causas.Ext <= "Y10")))
  , "C.EXT TIPO LESION - 3", F10.VAL$Errores.tipo.y.lugar.lesion)

# ACA DEBERÍA SER HASTA Y34 INCLUSIVE, PERO FALTA UN 5to CARACTER DESDE SISTEMA
F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Tipo.Lesion == 4 & !((F10.VAL$Cod.Diag.Ppal >= "Y10" & F10.VAL$Cod.Diag.Ppal <= "Y99") |
                                    (F10.VAL$Cod.Causas.Ext >= "Y10" & F10.VAL$Cod.Causas.Ext <= "Y99")))
  , "C.EXT TIPO LESION - 4", F10.VAL$Errores.tipo.y.lugar.lesion)


# VERIFICO LUGAR DE LESION POR CAUSA EXTERNA (DOM.PART, TRABAJO...) --------

F10.VAL$diagcausa <- substr(F10.VAL$Cod.Causas.Ext, 4, 4)

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Cod.Causas.Ext >= "W00" & F10.VAL$Cod.Causas.Ext <= "Y34") &
    !(substr(F10.VAL$Cod.Causas.Ext,1,3) == "X59" |
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y06" | 
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y07")
  & (F10.VAL$Lugar.Lesion == 1 & !(F10.VAL$diagcausa == 0))
  , "C.EXT LUGAR LESION - 1", F10.VAL$Errores.tipo.y.lugar.lesion)

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Cod.Causas.Ext >= "W00" & F10.VAL$Cod.Causas.Ext <= "Y34") &
    !(substr(F10.VAL$Cod.Causas.Ext,1,3) == "X59" |
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y06" | 
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y07")
  & (F10.VAL$Lugar.Lesion == 2 & !(F10.VAL$diagcausa == 4))
  , "C.EXT LUGAR LESION - 2", F10.VAL$Errores.tipo.y.lugar.lesion)

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Cod.Causas.Ext >= "W00" & F10.VAL$Cod.Causas.Ext <= "Y34") &
    !(substr(F10.VAL$Cod.Causas.Ext,1,3) == "X59" |
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y06" | 
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y07")
  & (F10.VAL$Lugar.Lesion == 3 & (F10.VAL$diagcausa == 0 | F10.VAL$diagcausa == 9))
  , "C.EXT LUGAR LESION - 3", F10.VAL$Errores.tipo.y.lugar.lesion)

F10.VAL$Errores.tipo.y.lugar.lesion <- ifelse(
  (F10.VAL$Cod.Causas.Ext >= "W00" & F10.VAL$Cod.Causas.Ext <= "Y34") &
    !(substr(F10.VAL$Cod.Causas.Ext,1,3) == "X59" |
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y06" | 
        substr(F10.VAL$Cod.Causas.Ext,1,3) == "Y07")
  & (F10.VAL$Lugar.Lesion == 4 & (F10.VAL$diagcausa == 0 | F10.VAL$diagcausa == 4))
  , "C.EXT LUGAR LESION - 4", F10.VAL$Errores.tipo.y.lugar.lesion)

F10.VAL$diagcausa <- NULL

###########################################################################
# #### FALTA DIAG PPAL #### -----------------------------------------------

F10.VAL$Falta.Diag.Ppal <- ifelse(is.na(F10.VAL$Cod.Diag.Ppal), "Falta Diagnostico Principal", NA)

###########################################################################
###########################################################################
# #### DIAGNOSTICO SEGUN TIPO DE EGRESO #### ------------------------------
# F$tipoegreso == "1", "Alta definitiva", NA)
# F$tipoegreso == "2", "Alta transitoria", F$Descr_Tipo_Egreso)
# F$tipoegreso == "3", "Traslado a otro establecimiento", F$Descr_Tipo_Egreso)
# F$tipoegreso == "4", "Defuncion", F$Descr_Tipo_Egreso)
# F$tipoegreso == "5", "Retiro voluntario", F$Descr_Tipo_Egreso)
# F$tipoegreso == "6", "Otro", F$Descr_Tipo_Egreso)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "1" 
                                      & F10.VAL$TIPO.EGRESO == "NO ALTA DEFINITIVA", 
                                      "Error Egreso Alta definitiva", NA)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "1" 
                                      & F10.VAL$TIPO.EGRESO == "ALTA DEFINITIVA", 
                                      "Error Egreso Alta definitiva", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "2" 
                                      & F10.VAL$TIPO.EGRESO == "NO ALTA TRANSITORIA", 
                                      "Error Egreso Alta transitoria", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "2" 
                                      & F10.VAL$TIPO.EGRESO == "ALTA TRANSITORIA", 
                                      "Error Egreso Alta transitoria", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "3" 
                                      & F10.VAL$TIPO.EGRESO == "NO TRASLADO", 
                                      "Error Egreso Alta traslado", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "3" 
                                      & F10.VAL$TIPO.EGRESO == "TRASLADO", 
                                      "Error Egreso Alta traslado", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "4" 
                                      & F10.VAL$TIPO.EGRESO == "NO DEFUNCION", 
                                      "Error Egreso Defuncion", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "4" 
                                      & F10.VAL$TIPO.EGRESO == "DEFUNCION", 
                                      "Error Egreso Defuncion", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "5" 
                                      & F10.VAL$TIPO.EGRESO == "NO RETIRO VOLUNTARIO", 
                                      "Error Egreso Alta transitoria", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "5" 
                                      & F10.VAL$TIPO.EGRESO == "RETIRO VOLUNTARIO", 
                                      "Error Egreso Retiro voluntario", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso == "6" 
                                      & F10.VAL$TIPO.EGRESO == "NO OTRO", 
                                      "Error Egreso Otro", F10.VAL$VAL.EGRESO)

F10.VAL$VAL.EGRESO <- ifelse(F10.VAL$Tipo.Egreso != "6" 
                                      & F10.VAL$TIPO.EGRESO == "OTRO", 
                                      "Error Egreso Otro", F10.VAL$VAL.EGRESO)


###########################################################################
###########################################################################
###########################################################################
# SALIDA ------------------------------------------------------------------

# RENOMBRO COLUMNAS Y REORDENO PARA SALIDA FINAL

F10.VAL <- F10.VAL %>% rename(Errores.en.Sexo = VAL.SEXO,
                              Edad.Id = ID.EDAD,
                              Errores.en.Edad = VAL.EDAD,
                              Errores.en.Servicio = VAL.SERV,
                              Errores.en.Tipo.egreso = VAL.EGRESO)

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
                 "Errores.en.Sexo", "Errores.en.Edad", "Errores.en.Servicio",
                 "Errores.diag.CE", "Errores.tipo.y.lugar.lesion", "Errores.en.Tipo.egreso")



F10.VAL.FINAL <- dplyr::select(F10.VAL, any_of(ORDEN.FINAL))

# F10.VAL <- F10.VAL %>% 
#   select(Form10.id, Region.Sanitaria, Partido, Cod.Est, Establecimiento, Nro.Informe, 
#          Apellido.Nombre, Tipo.Doc, Descr.Doc, Nro.Doc, Sexo, Fecha.Nacimiento,
#          Edad.id, Cant.Edad, Fecha.Ingreso, Servicio.Ingreso, Descr.Serv.Ingreso, 
#          Fecha.Egreso, Servicio.Egreso, Descr.Serv.Egreso, Tipo.Egreso, Dias.Estada,
#          Cod.Diag.Ppal, Descr.Diag.Ppal, Cod.Causas.Ext, Descr.Causas.Ext,
#          Tipo.Lesion, Lugar.Lesion, Errores.en.Sexo, Errores.en.Edad, Errores.en.Servicio)


# GENERO TABLA QUE SOLO CONTIENE ERRORES
F10.EDAD.SEXO <- F10.VAL.FINAL[(!is.na(F10.VAL.FINAL$Errores.en.Edad) 
                          | !is.na(F10.VAL.FINAL$Errores.en.Sexo) 
                          | !is.na(F10.VAL.FINAL$Errores.en.Servicio)
                          | !is.na(F10.VAL.FINAL$Errores.diag.CE)
                          | !is.na(F10.VAL.FINAL$Errores.tipo.y.lugar.lesion)
                          ),]



# GENERO ARCHIVOS SALIDA
# write.xlsx(F10.VAL.FINAL, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2021_SEXO-EDAD-SERV-todos.xlsx", overwrite = TRUE)
# write.xlsx(F10.EDAD.SEXO, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2021_SEXO-EDAD-SERV.xlsx", overwrite = TRUE)

# 2021
# write_xlsx(F10.VAL.FINAL, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2021_SEXO-EDAD-SERV-CE-todos.%Y.%m.%d.xlsx"),sep ="."))))
# write_xlsx(F10.EDAD.SEXO, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2021_SEXO-EDAD-SERV-CE.%Y.%m.%d.xlsx"),sep ="."))))

# 2022
write_xlsx(F10.VAL.FINAL, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2022_SEXO-EDAD-SERV-CE-todos.%Y.%m.%d.xlsx"),sep ="."))))
write_xlsx(F10.EDAD.SEXO, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/Validaciones-2022/F10-2022_SEXO-EDAD-SERV-CE.%Y.%m.%d.xlsx"),sep ="."))))

# ELIMINO TABLAS QUE NO VUELVO A USAR
# rm(VALI)
# rm(F10V)
# , overwrite = TRUE

###########################################################################
###########################################################################