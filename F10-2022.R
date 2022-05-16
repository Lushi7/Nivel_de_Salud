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
# ABRO F10 ----------------------------------------------------------------

F102022 <- read.delim("//10.21.11.5/5-Nivel de Salud/Descargas FTP/F10_2022_total 05-2022/2022_F10.TXT", quote = "")
# AL USAR -> quote = "" <- EVITO EL PROBLEMA DEL CARACTER OCULTO Y ABRE TODO

###########################################################################
# RENOMBRO BASES PARA NO VOLVER A CARGARLAS -------------------------------

F22 <- F102022

# CAMBIO AÑO DE F10 PARA AGREGAR TODAS LAS COLUMNAS QUE FALTAN ------------

F <- NULL

F <- F22


# ELIMINO FILAS QUE NO TIENEN NADA EN sexo (QUITO LA FILA QUE TIENE EL TOTAL)
F <- F[!is.na(F$sexo),]

f10 <- F 
# DESPLAZO COLUMNAS SI TIENEN ERROR ---------------------------------------

# Identifying which rows to shift
rows_to_change <- !f10$sexo %in% c("0", "1", "2", "3")

# Moving columns one space to the right
f10[rows_to_change,2:(ncol(f10)-1)] <- f10[rows_to_change,3:ncol(f10)] 

# Deleting wrong values
f10[rows_to_change,2] <- 1

F <- f10

# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------
f1 <- F

# FORM10_ID REPETIDOS
DUP <- f1[duplicated(f1$form10_id) | duplicated(f1$form10_id, fromLast=TRUE), ]

# NROS DE DOCUMENTOS REPETIDOS
DUP <- f1[duplicated(f1$nro_doc) | duplicated(f1$nro_doc, fromLast=TRUE), ]

# write.xlsx(DUP, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Form10id_REPETIDOS.xlsx", overwrite = TRUE)

# f1$DUP <- duplicated(f1[,c("form10_id")],)
# 
# F <- filter(f1, DUP == FALSE)
# 
# F$DUP <- NULL

###############################################################################
# GENERO ID RESIDENCIA ----------------------------------------------------

F$pais_id <- stringr::str_pad(F$pais_id, 3, side = "left", pad = 0)
F$prov_id <- stringr::str_pad(F$prov_id, 2, side = "left", pad = 0)
F$partido_id <- stringr::str_pad(F$partido_id, 3, side = "left", pad = 0)
F$locali_id <- stringr::str_pad(F$locali_id, 2, side = "left", pad = 0)

F$pais_id <- ifelse(is.na(F$pais_id), 999, F$pais_id)
F$prov_id <- ifelse(is.na(F$prov_id), 99, F$prov_id)
F$partido_id <- ifelse(is.na(F$partido_id), 999, F$partido_id)
F$locali_id <- ifelse(is.na(F$locali_id), 99, F$locali_id)

F$Codigo_Resi <- paste(F$pais_id, F$prov_id, F$partido_id, F$locali_id, sep = "")

# ABRO TABLA CON ID RESIDENCIA --------------------------------------------

RESIDENCIA <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Pais-Pcia-Partido-Localidades.xlsx", 
                         sheet = "Hoja2", col_types = c("text", 
                                                        "skip", "text", "skip", "text", "skip", 
                                                        "text", "skip", "text"))

RESI <- RESIDENCIA

F$reside_pais <- NULL
F$reside_prov <- NULL
F$reside_partido <- NULL
F$reside_locali <- NULL

# AGREGO NOMBRES DE RESIDENCIA A F10 --------------------------------------

F <- merge(F, RESI, all.x = TRUE, by = "Codigo_Resi")

# GUARDO codigos de resi para chequear los faltantes .xlsx -------------- -

Res <- F %>% 
  select(Codigo_Resi, pais_id, prov_id, partido_id, locali_id, Reside_Localidad)

Rr <- Res[is.na(Res$Reside_Localidad),]

# BUSCA Y ELIMINA DUPLICADOS -------------------------------------------- -

DUP <- Rr[duplicated(Rr$Codigo_Resi) | duplicated(Rr$Codigo_Resi, fromLast=TRUE), ]


Rr$DUP <- duplicated(Rr[,c("Codigo_Resi")],)

Res <- filter(Rr, DUP == FALSE)

Res$DUP <- NULL

write.xlsx(Res, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Codigos_pueden_faltar.xlsx", overwrite = TRUE)


F$pais_id <- NULL
F$prov_id <- NULL
F$partido_id <- NULL
F$locali_id <- NULL

# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------
# f1 <- F
# 
# DUP <- f1[duplicated(f1$form10_id) | duplicated(f1$form10_id, fromLast=TRUE), ]
# 
# 
# f1$DUP <- duplicated(f1[,c("form10_id")],)
# 
# F <- filter(f1, DUP == FALSE)
# 
# F$DUP <- NULL


###############################################################################
# FORMATO estable_id ------------------------------------------------------

# FORMATO CODIGO DE ESTABLECIMIENTO EN F10
# GUIA$estable_id <- stringr::str_pad(GUIA$COD.EST, 8, side = "left", pad = 0)
F$estable_id <- stringr::str_pad(F$estable_id, 8, side = "left", pad = 0)


# ABRO GUIA ESTABLECIMIENTOS ----------------------------------------------

GUIA.EST <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/RLu/Varios/Guia-R.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", "skip"))

# CAMBIO NOMBRE BASE SELECCIONANDO SOLO COLUMNAS QUE USO
GUIA <- GUIA.EST %>% 
  select(REGION, PARTIDO, COD.EST, ESTABLECIMIENTO, DEPENDENCIA)

# FORMATO CODIGO DE ESTABLECIMIENTO EN GUIA-ESTABLECIMIENTOS
GUIA$COD.EST <- stringr::str_pad(GUIA$COD.EST, 8, side = "left", pad = 0)

# CAMBIO NOMBRE COLUMNA COD.EST
GUIA <- GUIA %>% rename(estable_id = COD.EST)


# REEMPLAZO M = MUNICIPAL, C = PRIVADO, P = PROVINCIAL
GUIA$DEPENDENCIA <- ifelse(
  substr(GUIA$DEPENDENCIA, 1, 1) == "M", "MUNICIPAL", GUIA$DEPENDENCIA)
GUIA$DEPENDENCIA <- ifelse(
  substr(GUIA$DEPENDENCIA, 1, 1) == "P", "PROVINCIAL", GUIA$DEPENDENCIA)
GUIA$DEPENDENCIA <- ifelse(
  substr(GUIA$DEPENDENCIA, 1, 1) == "C", "PRIVADO", GUIA$DEPENDENCIA)
GUIA$DEPENDENCIA <- ifelse(
  substr(GUIA$DEPENDENCIA, 1, 1) == "N", "NACIONAL", GUIA$DEPENDENCIA)


# UNO GUIA.EST A F10 ------------------------------------------------------

F <- merge(GUIA, F, by = "estable_id", all.y = TRUE)

###############################################################################
# GENERO COLUMNAS DESCRIPCION:  -------------------------------------------

# Tipo Egreso, Edad id, Sexo, Lesion tipo, Lesion lugar, obra social --

F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "1", "Alta definitiva", NA)
F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "2", "Alta transitoria", F$Descr_Tipo_Egreso)
F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "3", "Traslado a otro establecimiento", F$Descr_Tipo_Egreso)
F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "4", "Defuncion", F$Descr_Tipo_Egreso)
F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "5", "Retiro voluntario", F$Descr_Tipo_Egreso)
F$Descr_Tipo_Egreso <- ifelse(F$tipoegreso == "6", "Otro", F$Descr_Tipo_Egreso)

F$Descr_Edad_id <- ifelse(F$edad_id == "1", "Años", NA)
F$Descr_Edad_id <- ifelse(F$edad_id == "2", "Meses", F$Descr_Edad_id)
F$Descr_Edad_id <- ifelse(F$edad_id == "3", "Dias", F$Descr_Edad_id)

F$Descr_Sexo <- ifelse(F$sexo == 1, "Masculino", NA)
F$Descr_Sexo <- ifelse(F$sexo == 2, "Femenino", F$Descr_Sexo)
F$Descr_Sexo <- ifelse( !(F$sexo == 1 | F$sexo == 2), "Indeterminado", F$Descr_Sexo)

F$Descr_Lesion_Tipo <- ifelse(F$lesion_id == "1", "Accidente", NA)
F$Descr_Lesion_Tipo <- ifelse(F$lesion_id == "2", "Lesion autoinflingida", F$Descr_Lesion_Tipo)
F$Descr_Lesion_Tipo <- ifelse(F$lesion_id == "3", "Agresion", F$Descr_Lesion_Tipo)
F$Descr_Lesion_Tipo <- ifelse(F$lesion_id == "4", "Se ignora", F$Descr_Lesion_Tipo)

F$Descr_Lesion_Lugar <- ifelse(F$lesi_lu_id == "1", "Domicilio particular", NA)
F$Descr_Lesion_Lugar <- ifelse(F$lesi_lu_id == "2", "Via publica", F$Descr_Lesion_Lugar)
F$Descr_Lesion_Lugar <- ifelse(F$lesi_lu_id == "3", "Lugar de trabajo", F$Descr_Lesion_Lugar)
F$Descr_Lesion_Lugar <- ifelse(F$lesi_lu_id == "4", "Otro", F$Descr_Lesion_Lugar)

F$Descr_Obra_soc <- ifelse(F$obra_soc == "1", "Obra social", NA)
F$Descr_Obra_soc <- ifelse(F$obra_soc == "2", "Plan de salud privado o mutual", F$Descr_Obra_soc)
F$Descr_Obra_soc <- ifelse(F$obra_soc == "3", "Plan o seguro publico", F$Descr_Obra_soc)
F$Descr_Obra_soc <- ifelse(F$obra_soc == "4", "Mas de uno", F$Descr_Obra_soc)
F$Descr_Obra_soc <- ifelse(F$obra_soc == "5", "Ninguno", F$Descr_Obra_soc)

##########################################################################
# ABRO CIE10 PARA DIAGNOSTICOS --------------------------------------------


CIE10.DIAG <- read_excel("//10.21.11.5/5-Nivel de Salud/MD/CIE10.xlsx", 
                         sheet = "Diag", col_types = c("skip", 
                                                       "skip", "skip", "text", "text", "skip"))


# GENERO cod_diag = catego_id + diag_id -----------------------------------

F <- F %>% 
  add_column(cod_diag = paste(F$catego_id, F$diag_id, sep = ""))

F$catego_id <- NULL
F$diag_id <- NULL


# AGREGO desc_4diag -------------------------------------------------------
# UNO GUIA.SERV A F10 

F <- merge(CIE10.DIAG, F, by = "cod_diag", all.y = TRUE)

###############################################################################
# ABRO CIE10 PARA CAUSAS EXTERNAS -----------------------------------------

CIE10.CE <- read_excel("//10.21.11.5/5-Nivel de Salud/MD/CIE10.xlsx", 
                       sheet = "CE", col_types = c("skip", "skip", 
                                                   "text", "text"))


# GENERO cod CE = categocausa + diagcausa ---------------------------------

F <- F %>% 
  add_column(`cod CE` = paste(F$categocausa, F$diagcausa, sep = ""))

F$categocausa <- NULL
F$diagcausa <- NULL


# AGREGO desc CE -------------------------------------------------------
# UNO GUIA.SERV A F10

F <- merge(CIE10.CE, F, by = "cod CE", all.y = TRUE)

###############################################################################
# VINCULO F10 A GUIA SERVICIOS -------------------------------------------
CIE10.SERV <- read_excel("//10.21.11.5/5-Nivel de Salud/MD/CIE10.xlsx", 
                         sheet = "servicios", col_types = c("numeric", 
                                                            "text", "skip"))


# RENOMBRO COLUMNAS DE CIE10 ----------------------------------------------

GSER <- CIE10.SERV %>% rename(servegreso_id = `cod ss`,
                              descr_servegreso = `desc ss`)



# FORMATO CODIGO DE SERVICIO EN F10 Y GUIA-SERVICIOS
GSER$servegreso_id <- stringr::str_pad(GSER$servegreso_id, 3, side = "left", pad = 0)

F$servegreso_id <- stringr::str_pad(F$servegreso_id, 3, side = "left", pad = 0)
F$serv_ingreso <- stringr::str_pad(F$serv_ingreso, 3, side = "left", pad = 0)


# UNO GUIA.SERV A F10 -----------------------------------------------------

F <- merge(GSER, F, by = "servegreso_id", all.y = TRUE)

F$descr_servegreso.y <- NULL

F <- rename(F, descr_servegreso = descr_servegreso.x)

# GENERO COLUMNAS COD.SERV Y DESCR.SERV

GSER <- rename(GSER, serv_ingreso = servegreso_id,
               descr_servingreso = descr_servegreso)


F <- merge(GSER, F, by = "serv_ingreso", all.y = TRUE)

###############################################################################
# CIE-10 Int quir ---------------------------------------------------------

CIE10.INTQ <- read_excel("//10.21.11.5/5-Nivel de Salud/MD/CIE10.xlsx", 
                         sheet = "Int quir", col_types = c("skip", 
                                                           "skip", "text", "text"))

# RENOMBRO COLUMNAS DE CIE10 ----------------------------------------------

INTQ <- CIE10.INTQ %>% rename(cod_IQ1 = `cod IQ`,
                              descr_IQ1 = `desc IQ`)

F$descr_intquir1 <- NULL
F$descr_intquir2 <- NULL
F$descr_intquir3 <- NULL

# GENERO CODIGOS DE 4 DIGITOS DE INTQ -------------------------------------
# intq1
F <- F %>% 
  add_column(cod_IQ1 = paste(F$cat_int_id1, F$intquir_id1, sep = ""))

F$cat_int_id1 <- NULL
F$intquir_id1 <- NULL

# intq2
F <- F %>% 
  add_column(cod_IQ2 = paste(F$cat_int_id2, F$intquir_id2, sep = ""))

F$cat_int_id2 <- NULL
F$intquir_id2 <- NULL

# intq3
F <- F %>% 
  add_column(cod_IQ3 = paste(F$cat_int_id3, F$intquir_id3, sep = ""))

F$cat_int_id3 <- NULL
F$intquir_id3 <- NULL

# FORMATO CODIGO DE SERVICIO EN F10 E INTERVENCION QUIRURGICA
INTQ$cod_IQ1 <- stringr::str_pad(INTQ$cod_IQ1, 4, side = "left", pad = 0)
F$cod_IQ1 <- stringr::str_pad(F$cod_IQ1, 4, side = "left", pad = 0)
F$cod_IQ2 <- stringr::str_pad(F$cod_IQ1, 4, side = "left", pad = 0)
F$cod_IQ3 <- stringr::str_pad(F$cod_IQ1, 4, side = "left", pad = 0)

F$cod_IQ1 <- ifelse(F$cod_IQ1 == "NANA", NA, F$cod_IQ1)
F$cod_IQ2 <- ifelse(F$cod_IQ2 == "NANA", NA, F$cod_IQ2)
F$cod_IQ3 <- ifelse(F$cod_IQ3 == "NANA", NA, F$cod_IQ3)

# UNO INTQ A F10 ----------------------------------------------------------

F <- merge(INTQ, F, by = "cod_IQ1", all.y = TRUE)


# CAMBIO NOMBRE COLUMNAS DE INTQ PARA AGREGAR DESCR IQ 2
INTQ <- INTQ %>% rename(cod_IQ2 = cod_IQ1,
                        descr_IQ2 = descr_IQ1)

# UNO GUIA.SERV A F10
F <- merge(INTQ, F, by = "cod_IQ2", all.y = TRUE)

# CAMBIO NOMBRE COLUMNAS DE INTQ PARA AGREGAR DESCR IQ 3
INTQ <- INTQ %>% rename(cod_IQ3 = cod_IQ2,
                        descr_IQ3 = descr_IQ2)

# UNO GUIA.SERV A F10
F <- merge(INTQ, F, by = "cod_IQ3", all.y = TRUE)

###############################################################################
# checkpoint
f10 <- F
F <- f10
###############################################################################
# CAMBIO NOMBRES COLUMNAS Y LAS REORDENO PARA SALIDA FINAL ----------------

# ARMO UNA LISTA QUE ME VA A CAMBIAR LOS NOMBRES DE LAS COLUMAS SI EXISTEN:
NOMBRES.FINAL <- c(Form10.id = "form10_id",
                   Region.Sanitaria = "REGION",
                   Partido = "PARTIDO",
                   Cod.Est = "estable_id",
                   Establecimiento = "ESTABLECIMIENTO",
                   Sector = "DEPENDENCIA",
                   Nro.Informe = "nroinforme",
                   Mes = "mes",
                   `Año` = "anio",
                   Codigo.Residencia = "Codigo_Resi",
                   Reside.Pais = "Reside_Pais",
                   Reside.Prov = "Reside_Prov",
                   Reside.Partido = "Reside_Partido",
                   Reside.Localidad = "Reside_Localidad",
                   Apellido.Nombre = "apellido_nombre",
                   Tipo.Doc = "tipo_doc",
                   Descr.Doc = "tipo_doc_descrip",
                   Nro.Doc = "nro_doc",
                   Fecha.Nacimiento = "fecha_nac",
                   Fecha.Ingreso = "feingser",
                   Sexo = "sexo",
                   Descr.Sexo = "Descr_Sexo",
                   Edad.id = "edad_id",
                   Cant.Edad = "cantedad",
                   Descr.Edad.id = "Descr_Edad_id",
                   Grupo.Etario = "grupo_etareo",
                   Fecha.Egreso = "fe_egreso",
                   Servicio.Ingreso = "serv_ingreso",
                   Descr.Serv.Ingreso = "descr_servingreso",
                   Servicio.Egreso = "servegreso_id",
                   Descr.Serv.Egreso = "descr_servegreso",
                   Tipo.Egreso = "tipoegreso",
                   Descr.Tipo.Egreso = "Descr_Tipo_Egreso",
                   Dias.Estada = "dias_estada",
                   Cod.Diag.Ppal = "cod_diag",
                   Cod.Causas.Ext = "cod CE",
                   Descr.Causas.Ext = "desc CE",
                   Descr.Diag.Ppal = "desc_4diag",
                   Cap.CIE10 = "capitu_id",
                   Grupo.CIE10 = "grp_cap_id",
                   Descr.Cap.CIE10 = "descr_capitu",
                   Descr.Grp.CIE10 = "descr_grp",
                   Tipo.Lesion = "lesion_id",
                   Lugar.Lesion = "lesi_lu_id",
                   Descr.Lesion.Tipo = "Descr_Lesion_Tipo",
                   Descr.Lesion.Lugar = "Descr_Lesion_Lugar",
                   Dias.ARM = "arm",
                   Obra.Social = "obra_soc",
                   Descr.Obra.Social = "Descr_Obra_soc",
                   Peso.gr.nacim = "pesogrsnacer",
                   Fecha.Ingreso = "feingser",
                   Cod.Int.Quir1 = "cod_IQ1",
                   Descr.Int.Quir1 = "descr_IQ1",
                   Cod.Int.Quir2 = "cod_IQ2",
                   Descr.Int.Quir2 = "descr_IQ2",
                   Cod.Int.Quir3 = "cod_IQ3",
                   Descr.Int.Quir3 = "descr_IQ3",
                   Nivel.Instruccion = "nivel_instruccion",
                   Situacion.Laboral = "situacion_laboral",
                   Ocupacion.Actual = "ocupacion_actual")

F.fin <- F %>% rename(any_of(NOMBRES.FINAL))


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
                 "Peso.gr.nacim", "Nivel.Instruccion", "Situacion.Laboral", "Ocupacion.Actual")



F.FINAL <- dplyr::select(F.fin, any_of(ORDEN.FINAL))

###############################################################################
# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------
f1 <- F.FINAL

# FORM10_ID REPETIDOS
DUP <- f1[duplicated(f1$Form10.id) | duplicated(f1$Form10.id, fromLast=TRUE), ]

# NROS DE DOCUMENTOS REPETIDOS
DUP <- f1[duplicated(f1$Nro.Doc) | duplicated(f1$Nro.Doc, fromLast=TRUE), ]

# write.xlsx(DUP, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/Validaciones/Form10id_REPETIDOS.xlsx", overwrite = TRUE)

# f1$DUP <- duplicated(f1[,c("form10_id")],)
# 
# F <- filter(f1, DUP == FALSE)
# 
# F$DUP <- NULL


###############################################################################
###############################################################################
# ACA SELECCIONO FINAL! ---------------------------------------------------

F10.2022 <- F.FINAL

###############################################################################
# GUARDO SALIDA DE F10 COMO .xlsx -----------------------------------------

# write.xlsx(F10.2022, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/F10.2022-2022.05.09.xlsx", overwrite = TRUE)


# 2022
write_xlsx(F10.2022, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/F10/F10-2022/F10.2022-%Y.%m.%d.xlsx"),sep ="."))))

###############################################################################, overwrite = TRUE
###############################################################################
###############################################################################