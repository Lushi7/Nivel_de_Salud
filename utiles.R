###############################################################################
# LIBRERIAS ---------------------------------------------------------------

library(basictabler)
library(data.table)
library(datos)
library(dplyr)
library(flextable)
library(ggplot2)
library(Hmisc)
library(labelled)
library(lubridate)
library(magrittr)
library(MASS)
library(openxlsx)
library(pivottabler)
library(RColorBrewer)
library(readr)
library(readxl)
library(reshape2)
library(stringi)
library(stringr)
library(tibble)
library(tidyverse)
library(writexl)
library(zoo)

###############################################################################
# ABRE / CONSOLIDA ARCHIVOS DE UNA CARPETA ---------------------------------------


F10 <- list.files(path="//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos-F10", full.names = TRUE) %>%
  lapply(read_xlsx, skip = 4) %>%
  bind_rows


OCUP.CONSOLIDADO <- list.files(path="//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/1- Monitor Indicadores (Ex-Informe)/Modelo de datos Seguimiento Camas/Ocupacion/Historico/", full.names = TRUE) %>%
  lapply(read_xlsx, col_types = c("date", "skip", "skip", "skip", "text", "skip",
                                  "skip", "skip", "text", "text", "text", "text", 
                                  "text", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                  "skip", "skip", "skip"), skip = 4) %>% bind_rows


###############################################################################
# CAMBIO VACIOS POR NA ----------------------------------------------------

F102021 <- read.delim("C:/Users/Dpto_Vitales/Desktop/Bases Egresos/F102021.TXT", na.strings="")

# DENTRO DEL PAR?NTESIS DE read_xlsx ASIGNA COMO NA A LOS ESPACIOS VACIOS
na.strings=c(""," ","NA")

dat <- dat %>% mutate_all(na_if,"")

###############################################################################
# EJECUTA UN SCRIPT DESDE SU UBICACION ------------------------------------

# CENSO_CONSOLIDADO.R: 

source("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/RLu/Censos/censo_consolidado.R")

###############################################################################
# VALORES UNICOS DE GUIA --------------------------------------------------

Guia <- Guia %>% 
  distinct()

# VALORES UNICOS DE SERVICIOS

servicios <- F10.VAL %>% 
  select(Servicio.Egreso, Descr.Serv.Egreso)

# VALORES UNICOS
servicios <- servicios %>% 
  distinct()

###############################################################################
# GENERA / CREA COLUMNA NUEVA CON ELEMENTOS / NA --------------------------

F100 <- F100 %>% 
  add_column(A?O.F10 = 2021)

###############################################################################
# CAMBIAR NOMBRE COLUMNAS -------------------------------------------------


names(Guia)[names(Guia) == 'ESTABLECIMIENTO COD'] <- 'COD.EST'

DIAG <- DIAG %>% rename(form10_id = form10id) # NUEVO.NOM = VIEJO.NOM

rename(archivo, nombre_viejo = nombre.nuevo)  # cambia nombre encabezado

# ARMO UNA LISTA QUE ME VA A CAMBIAR LOS NOMBRES DE LAS COLUMAS SI EXISTEN:
BUSCAR <- c( "ENERO" = "1",
             "FEBRERO" = "2",
             "MARZO" = "3",
             "ABRIL" = "4",
             "MAYO" = "5",
             "JUNIO" = "6",
             "JULIO" = "7",
             "AGOSTO" = "8",
             "SEPTIEMBRE" = "9",
             "OCTUBRE" = "10",
             "NOVIEMBRE" = "11",
             "DICIEMBRE" = "12")

SRH.MESES <- SRH.MESES %>% rename(any_of(BUSCAR))

# AGREGO AL NOMBRE SUFIJO "SRH":
SRH.MESES <- SRH.MESES %>% rename_at(vars(-Establecimiento), ~ paste0(., ".SRH"))

###############################################################################
# CAMBIA NOMBRE COLUMNA SI EXISTE -----------------------------------------

# ARMO UNA LISTA QUE ME VA A CAMBIAR LOS NOMBRES DE LAS COLUMAS SI EXISTEN:
BUSCAR <- c( "ENERO" = "1",
             "FEBRERO" = "2",
             "MARZO" = "3",
             "ABRIL" = "4",
             "MAYO" = "5",
             "JUNIO" = "6",
             "JULIO" = "7",
             "AGOSTO" = "8",
             "SEPTIEMBRE" = "9",
             "OCTUBRE" = "10",
             "NOVIEMBRE" = "11",
             "DICIEMBRE" = "12")

SRH.MESES <- F102010 %>% rename(any_of(BUSCAR))

###############################################################################
# ELIMINA COLUMNAS DE UN DF ------------------------------------------------

OPORT$`CENSO 21HS`<- NULL
# ESTO ME ELIMINA LA PENULTIMA COLUMNA (QUE ES MI ULTIMA SEMANA = INCOMPLETA)
d13p <- d13[,-(ncol(d13)-1)]
# and if you want last n columns, try
mydata[,-(ncol(mydata2)-n-1):ncol(mydata2)]

###############################################################################
# ELIMINA FILAS VACIAS O CON ELEMENTOS NA ---------------------------------

# ELIMINA FILAS QUE CONTIENEN NA EN UNA COLUMNA EN PARTICULAR
pacientes <- pacientes[!is.na(pacientes$F.IN),]
# ELIMINA FILAS QUE CONTENGAN NA
RELEVA <- na.omit(RELEV)
# ELIMINA FILAS VACIAS (TODO NA)
RELEVA <- RELEV %>% filter_all(any_vars(!is.na(.)))


###############################################################################
# ELIMINA UN DF ------------------------------------------------

rm(F102019)

# ELIMINA TODO MENOS "SALa"
rm(list=setdiff(ls(), "SALa"))

# ELIMINA TODO MENOS ESTOS AMIGUITXS
rm(list = ls()[! ls() %in% c("EVOL", "SALa", "SALap", "SALA.FAL", "SALAP.FAL")])

###############################################################################
# BUSCA Y ELIMINA DUPLICADOS ----------------------------------------------


DUP <- F102021[duplicated(F102021$form10_id) | duplicated(F102021$form10_id, fromLast=TRUE), ]


F102020$DUP <- duplicated(F102020[,c("form10_id")],)

F102019<-filter(F102020, DUP==F)

###############################################################################
# GENERO NUEVAS COLUMNAS DE A?O = 2001  -------------------------


f10 <- F10 %>% 
  add_column(A?O.F10 = 2021)

###############################################################################
# REORDENO LAS COLUMNAS ---------------------------------------------------

%>%
  select(-año.alt)

CIPRESS <- CIPRES %>% 
  select(COD.EST, AniO.CIPRES, MES.CIPRES, Egreso_Mensual_CIPRES)


RES <- RES %>% 
  dplyr::select(REGION, ID.PARTIDO, PARTIDO, COD.EST, ESTABLECIMIENTO, SECTOR, DEPENDENCIA)


RELEVA = RELEVA[, c("FECHA", "REGION", "ID.PARTIDO", "PARTIDO", "COD.EST", "ESTABLECIMIENTO", 
                    "DEPENDENCIA", "COD.SERV", "SERVICIO", "TIPO.CUIDADO", 
                    "GRUPO", "CAMAS.DISP", "CAMAS.OCUPA", "CAMAS.OCUPA.COVID")]


RES. <- RES[, c(2, 3, 4, 1, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]

###############################################################################
# ORDEN ASCENDENTE - DESCENDENTE ------------------------------------------

C.H <- arrange(CENSOS, desc(FECHA.CENSO),)

###############################################################################
# ME CUENTA CANTIDAD DE VALORES UNICOS DE UNA TABLA ---------------------------

table(F102020$sexo)

###############################################################################
# REEMPLAZO NA/0 por PRIVADO/NA en Jurisdiccion --------------------------------

Censo$JURISDICCION[is.na(Censo$JURISDICCION)] = "PRIVADO"

CENSO$CENSOS.OPORTUNOS[CENSO$CENSOS.OPORTUNOS == 0] <- NA

###############################################################################
# FORMATO FECHA -----------------------------------------------------------

# Formato de FECHA dia/mes/a?o

Ocupacion.Consolidado$FECHA <- as.Date(Ocupacion.Consolidado$FECHA ,format="%d/%m/%Y")

F100 <- F100 %>%
  mutate(fecha_nac = lubridate::dmy(fecha_nac))

# Formato año-semana
CONS <- agrupado
CONS$fecha_atencion <- as.Date(CONS$fecha_atencion, format = "%Y-%m-%d")
CONS$FECHA1 <- strftime(CONS$fecha_atencion, format = "%Y-%U")

###############################################################################
# FORMATO VARIABLES -------------------------------------------------------

F10VAL$EDAD.CALC <- as.integer(F10VAL$EDAD.CALC)



F10 <- F102020 %>% 
  select(apellido_nombre, fecha_nac, form10_id, estable_id, sexo, fe_egreso, 
         tipoegreso, categocausa, diagcausa, catego_id, diag_id, edad_id,
         cantedad, capitu_id, grp_cap_id, servegreso_id, dias_estada, 
         lesion_id, lesi_lu_id, pesogrsnacer, feingser,serv_ingreso)


F10 <- F10 %>%
  mutate(apellido_nombre= as.character(apellido_nombre),
         form10_id= as.character(form10_id), estable_id= as.character(estable_id),
         fecha_nac = as.character(fecha_nac), fe_egreso = as.character(fe_egreso), 
         categocausa = as.character(categocausa), diagcausa = as.character(diagcausa),
         catego_id = as.character(catego_id), diag_id = as.character(diag_id),
         servegreso_id = as.character(servegreso_id), feingser = as.character(feingser), 
         serv_ingreso = as.character(serv_ingreso))

F10 <- F10 %>%
  mutate(fecha_nac = lubridate::dmy(fecha_nac), 
         fe_egreso = lubridate::dmy(fe_egreso),
         feingser = lubridate::dmy(feingser))


F10$form10_id <- stringr::str_pad(F10$form10_id, 7, side = "left", pad = 0)
F10$estable_id <- stringr::str_pad(F10$estable_id, 8, side = "left", pad = 0)
F10$servegreso_id <- stringr::str_pad(F10$servegreso_id, 3, side = "left", pad = 0)
F10$serv_ingreso <- stringr::str_pad(F10$serv_ingreso, 3, side = "left", pad = 0)

# 2020 --------------------------------------------------------------------

f20$Form10.id <- as.character(f20$Form10.id)
f20$Region.Sanitaria <- as.character(f20$Region.Sanitaria)
f20$Partido <- as.character(f20$Partido)
f20$Cod.Est <- as.character(f20$Cod.Est)
f20$Establecimiento <- as.character(f20$Establecimiento)
f20$Sector <- as.character(f20$Sector)
f20$Nro.Informe <- as.character(f20$Nro.Informe)
f20$Mes <- as.character(f20$Mes)
f20$Año <- as.character(f20$Año)
f20$Codigo.Residencia <- as.character(f20$Codigo.Residencia)
f20$Reside.Pais <- as.character(f20$Reside.Pais)
f20$Reside.Prov <- as.character(f20$Reside.Prov)
f20$Reside.Partido <- as.character(f20$Reside.Partido)
f20$Reside.Localidad <- as.character(f20$Reside.Localidad)
f20$Apellido.Nombre <- as.character(f20$Apellido.Nombre)
f20$Tipo.Doc <- as.character(f20$Tipo.Doc)
f20$Descr.Doc <- as.character(f20$Descr.Doc)
f20$Nro.Doc <- as.character(f20$Nro.Doc)
f20$Fecha.Nacimiento <- as.character(f20$Fecha.Nacimiento)
f20$Sexo <- as.character(f20$Sexo)
f20$Descr.Sexo <- as.character(f20$Descr.Sexo)
f20$Edad.id <- as.integer(f20$Edad.id)
f20$Descr.Edad.id <- as.character(f20$Descr.Edad.id)
f20$Cant.Edad <- as.integer(f20$Cant.Edad)
f20$Grupo.Etario <- as.character(f20$Grupo.Etario)
f20$Fecha.Ingreso <- as.character(f20$Fecha.Ingreso)
f20$Servicio.Ingreso <- as.character(f20$Servicio.Ingreso)
f20$Descr.Serv.Ingreso <- as.character(f20$Descr.Serv.Ingreso)
f20$Fecha.Egreso <- as.character(f20$Fecha.Egreso)
f20$Servicio.Egreso <- as.character(f20$Servicio.Egreso)
f20$Descr.Serv.Egreso <- as.character(f20$Descr.Serv.Egreso)
f20$Tipo.Egreso <- as.character(f20$Tipo.Egreso)
f20$Descr.Tipo.Egreso <- as.character(f20$Descr.Tipo.Egreso)
f20$Dias.Estada <- as.integer(f20$Dias.Estada)
f20$Cap.CIE10 <- as.character(f20$Cap.CIE10)
f20$Grupo.CIE10 <- as.character(f20$Grupo.CIE10)
f20$Cod.Diag.Ppal <- as.character(f20$Cod.Diag.Ppal)
f20$Descr.Diag.Ppal <- as.character(f20$Descr.Diag.Ppal)
f20$Cod.Causas.Ext <- as.character(f20$Cod.Causas.Ext)
f20$Descr.Causas.Ext <- as.character(f20$Descr.Causas.Ext)
f20$Tipo.Lesion <- as.character(f20$Tipo.Lesion)
f20$Lugar.Lesion <- as.character(f20$Lugar.Lesion)
f20$Descr.Lesion.Tipo <- as.character(f20$Descr.Lesion.Tipo)
f20$Descr.Lesion.Lugar <- as.character(f20$Descr.Lesion.Lugar)
f20$Dias.ARM <- as.integer(f20$Dias.ARM)
f20$Obra.Social <- as.character(f20$Obra.Social)
f20$Descr.Obra.Social <- as.character(f20$Descr.Obra.Social)
f20$Cod.Int.Quir1 <- as.character(f20$Cod.Int.Quir1)
f20$Descr.Int.Quir1 <- as.character(f20$Descr.Int.Quir1)
f20$Cod.Int.Quir2 <- as.character(f20$Cod.Int.Quir2)
f20$Descr.Int.Quir2 <- as.character(f20$Descr.Int.Quir2)
f20$Cod.Int.Quir3 <- as.character(f20$Cod.Int.Quir3)
f20$Descr.Int.Quir3 <- as.character(f20$Descr.Int.Quir3)
f20$Peso.gr.nacim <- as.character(f20$Peso.gr.nacim)
f20$Nivel.Instruccion <- as.character(f20$Nivel.Instruccion)
f20$Situacion.Laboral <- as.character(f20$Situacion.Laboral)
f20$Ocupacion.Actual <- as.character(f20$Ocupacion.Actual)


###############################################################################
# FORMATO NUMEROS-DECIMALES -----------------------------------------------

# REDONDEA A 4 DECIMALES DESPUES DE LA COMA
format(round(28/365.25, 4))


###############################################################################
# FUNCIONES UTILES --------------------------------------------------------

#Para ver valores variable
glimpse(F102020)

# trae valores ?nicos
unique(F102020$sexo)

# cuenta cu?ntos hay de cada valor ?nico
table(F102020$sexo)

# cuenta Nro columnas
nc<-ncol(F102020)

# Useful functions
# Center: mean(), median()
# 
# Spread: sd(), IQR(), mad()
# 
# Range: min(), max(), quantile()
# 
# Position: first(), last(), nth(),
# 
# Count: n(), n_distinct()
# 
# Logical: any(), all()
 
archivo %>%
   group_by(establecimientos) %>%
   summarise(mean = mean(camas), n = n()) %>%
    summarise(n.var = quantile(camas, c(0.25, 0.75)), prob = c(0.25, 0.75))

INDICADORES <- pacientes %>% 
  group_by(COD.EST, REGION, PARTIDO, LOCALIDAD, ESTABLECIMIENTO) %>% 
  summarise(EGRESOS = sum(EGRESOS), DIAS.PACIENTES = sum(DIAS.ESTADA))


table(var) #me cuenta las que son iguales/diferentes
summary(tabla) # me da toda la estadistica de la tabla - pillo
head(tabla, nro_filas_quiero_ver_sino_son_6)
tail(tabla, nro_filas_quiero_ver_sino_son_6)

###############################################################################
# ELSEIF --------------------------------------------------------
a = c(5,7,2,9)
ifelse(a %% 2 == 0,"even","odd")
# "odd"  "odd"  "even" "odd"

F10.C.EXT$Errores.diag.CE <- ifelse(
  ( (substr(F10.C.EXT$Cod.Diag.Ppal,1,1) == "S" | substr(F10.C.EXT$Cod.Diag.Ppal, 1, 1) == "T") 
    & F10.C.EXT$capitu_id != 19), "C.EXT Cod.Diag.Ppal [S-T] NO EN CAP XIX", NA)


###############################################################################
# FILTRAR POR -------------------------------------------------------------

# Useful filter functions

  ==, >, >= etc

&, |, !, xor()      Y, O, NO, ???
la coma tambien es un Y

is.na()

between(), near()

filter(starwars, hair_color == "none" | eye_color == "black")  # filtra por esto O lo otro

vars <- c("mass", "height")
cond <- c(80, 150)
starwars %>%
  filter(
    .data[[vars[[1]]]] > cond[[1]],
    .data[[vars[[2]]]] > cond[[2]]             # deja solo la masa>80 y altura>150
  )

# ESTA EN
df %>%
  filter(col_name %in% c('value1', 'value2', 'value3', ...))

# NO ESTA EN (cambia ! antes de colname)
df %>%
  filter(!col_name %in% c('value1', 'value2', 'value3', ...))

###############################################################################
# FILTRO POR FECHAS -------------------------------------------------------

# fechas <- unique(Historico$FECHA) # me da valores unicos
# fechas <- fechas[order(fechas, decreasing = TRUE)] # ordena de mayor a menor
# fechas <- fechas[2:25] 

# Cen.8 <- subset(Historico, FECHA >= floor_date(today(), unit = "month") - months(1))
# fec <- unique(Cen.8$FECHA) # me da valores unicos
# fec <- fec[order(fec, decreasing = TRUE)] # ordena de mayor a menor

# SETEO DIAS DE SEMANA CON FECHA CORRESPONDIENTE ------------------------------


hoy <- (Sys.Date())
diasem <- weekdays(hoy)

if (diasem == "lunes") {
  lunes <- hoy
  martes <- hoy - 6
  miercoles <- hoy - 5
  jueves  <- hoy - 4
  viernes <- hoy - 3 
} else if (diasem == "martes"){
  lunes <- hoy - 1
  martes <- hoy
  miercoles <- hoy - 6
  jueves  <- hoy - 5
  viernes <- hoy - 4 
  
} else if (diasem == "mi?rcoles"){
  lunes <- hoy - 2
  martes <- hoy - 1
  miercoles <- hoy 
  jueves  <- hoy - 6
  viernes <- hoy - 5 
  
} else if (diasem == "jueves"){
  lunes <- hoy - 3
  martes <- hoy - 2
  miercoles <- hoy -1
  jueves  <- hoy 
  viernes <- hoy - 6 
} else if (diasem == "viernes"){
  lunes <- hoy - 4
  martes <- hoy - 3
  miercoles <- hoy - 2
  jueves  <- hoy - 1
  viernes <- hoy }


###############################################################################
# AGRUPAR -----------------------------------------------------------------
# grouped arrange ignores groups
by_cyl <- mtcars %>% group_by(cyl)
by_cyl %>% arrange(desc(wt))
# Unless you specifically ask:
by_cyl %>% arrange(desc(wt), .by_group = TRUE)

###############################################################################
# SELECCIONO FILAS QUE CUMPLAN CONDICION ----------------------------------

pacientes <- pacientes[(pacientes$JURISDICCION == "MUNICIPAL"),]


###############################################################################
# Genero IDSERVFECHA -------------------------------

Censo$IDSERVFECHA <- paste(Censo$`FECHA CENSO`,Censo$`ESTABLECIMIENTO COD`,Censo$`SERVICIO COD`,Censo$CUIDADO,Censo$GRUPO,sep = "|")


paste(var1, var2, var3, sep = "_")



###############################################################################
# Separo IDREL en FECHA y COD.EST -> Ejemplo: 44377-21000039 ---------------


diacod = RELEV$IDREL
diacod = strsplit(diacod,"\\-")
diacod = do.call(rbind, diacod)
colnames(diacod) = c("FECHA", "COD.EST")
RELEV = cbind(RELEV,diacod)
RELEV$IDREL = NULL
RELEV$FECHA <- RELEV$`dia`
RELEV$`dia` = NULL

# RENOMBRO FILAS DEJANDO SOLO EL NOMBRE ANTES DEL PUNTO/SIMBOLO

INTENSIVOS$DEPENDENCIA <- sub("\\..*", "", INTENSIVOS$DEPENDENCIA)

# SEPARO POR SALTO DE LINEA /linebreak ------------------------------------

Cg <- C %>% mutate(C1 = strsplit(C1, "\n")) %>% 
  unnest(C1) %>% 
  mutate(row = row_number()) %>% 
  spread(row, C1)
CG <- as.data.frame(t(Cg))
CG2 <- unique(CG)

###############################################################################
# Check for any whitespace in the text #################


x$species <- gsub('\\s+', '', x$species)


###############################################################################
# BUSCA el caracter oculto del CODEST ---------------------------

a = GuiaN$COD.EST    #
 nchar(a)
 any(a!=9)
 which(a!=9)
GuiaN$COD.EST = substr(GuiaN$COD.EST, 2,9)

SGC$COD.EST <- stringr::str_pad(SGC$COD.EST, 8, side = "left", pad = 0)

Censo$COD.SERV <- stringr::str_pad(Censo$COD.SERV, 3, side = "left", pad = 0)

###############################################################################
# CONVIERTO TEXTO A MAYUSCULAS ---------------------------------------------

GuiaN <- mutate_all(Guia, funs(toupper))

# CAMBIO TODO A MAYUSCULAS Y REEMPLAZO TILDES
d <- chartr("ÁÉÍÓÚ", "AEIOU", toupper(C$C1))
e <- toupper(stri_trans_general(C$C1, "Latin-ASCII"))

rename(archivo, nombre_viejo = nombre.nuevo)  # cambia nombre encabezado

names(Guia)[names(Guia) == 'C?digo'] <- 'COD.EST'

rename_with(archivo, toupper)   # pone los encabezados en mayusc



###############################################################################
# ORDEN COLUMNAS F10 ------------------------------------------------------

### ORDEN 
col_order <- c( "apellido_nombre",  "tipo_doc",
                "tipo_doc_descrip", "nro_doc",
                "fecha_nac","form10_id" ,
                "estable_id","sexo", 
                "fe_egreso", "pais_id",
                "prov_id","partido_id",
                "locali_id","tipoegreso",
                "categocausa","diagcausa",
                "catego_id","diag_id",
                "edad_id", "cantedad",
                "capitu_id","grp_cap_id",
                "mes","anio",
                "servegreso_id","dias_estada",
                "nroinforme","grupo_etareo",
                "intquir_id1","cat_int_id1",
                "intquir_id2","cat_int_id2", 
                "intquir_id3" ,"cat_int_id3",
                "arm","obra_soc", "lesion_id",
                "lesi_lu_id","pesogrsnacer",
                "feingser","serv_ingreso")

#names(base_2019)

F10 <- F102010[, col_order]


# ORDEN POR COLUMNA -------------------------------------------------------

table = table[order(table[,1], decreasing = FALSE),]

###############################################################################
# VINCULAR A GUIA ---------------------------------------------------------


Guia <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/RLu/Varios/Guia-R.xlsx", 
                     col_types = c("text", "skip", "text", 
                                   "text", "text", "text", "skip", "text", 
                                   "text"))

SGC$COD.EST <- stringr::str_pad(SGC$COD.EST, 8, side = "left", pad = 0)

SIGC <- merge(Guia, SGC, by = "COD.EST", all.y = TRUE)

###############################################################################
# GENERO COLUMNAS DESCRIPCION QUE FALTAN ----------------------------------

F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "1", "Alta definitiva", NA)
F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "2", "Alta transitoria", F10.20$Descr_Tipo_Egreso)
F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "3", "Traslado a otro establecimiento", F10.20$Descr_Tipo_Egreso)
F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "4", "Defuncion", F10.20$Descr_Tipo_Egreso)
F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "5", "Retiro voluntario", F10.20$Descr_Tipo_Egreso)
F10.20$Descr_Tipo_Egreso <- ifelse(F10.20$tipoegreso == "6", "Otro", F10.20$Descr_Tipo_Egreso)

F10.20$Descr_Edad_id <- ifelse(F10.20$edad_id == "1", "Anios", NA)
F10.20$Descr_Edad_id <- ifelse(F10.20$edad_id == "2", "Meses", F10.20$Descr_Edad_id)
F10.20$Descr_Edad_id <- ifelse(F10.20$edad_id == "3", "Dias", F10.20$Descr_Edad_id)

F10.20$Descr_Sexo <- ifelse(F10.20$sexo == 1, "Masculino", NA)
F10.20$Descr_Sexo <- ifelse(F10.20$sexo == 2, "Femenino", F10.20$Descr_Sexo)
F10.20$Descr_Sexo <- ifelse( !(F10.20$sexo == 1 | F10.20$sexo == 2), "Indeterminado", F10.20$Descr_Sexo)

F10.20$Descr_Lesion_Tipo <- ifelse(F10.20$lesion_id == "1", "Accidente", NA)
F10.20$Descr_Lesion_Tipo <- ifelse(F10.20$lesion_id == "2", "Lesion autoinflingida", F10.20$Descr_Lesion_Tipo)
F10.20$Descr_Lesion_Tipo <- ifelse(F10.20$lesion_id == "3", "Agresion", F10.20$Descr_Lesion_Tipo)
F10.20$Descr_Lesion_Tipo <- ifelse(F10.20$lesion_id == "4", "Se ignora", F10.20$Descr_Lesion_Tipo)

F10.20$Descr_Lesion_Lugar <- ifelse(F10.20$lesi_lu_id == "1", "Domicilio particular", NA)
F10.20$Descr_Lesion_Lugar <- ifelse(F10.20$lesi_lu_id == "2", "Via publica", F10.20$Descr_Lesion_Lugar)
F10.20$Descr_Lesion_Lugar <- ifelse(F10.20$lesi_lu_id == "3", "Lugar de trabajo", F10.20$Descr_Lesion_Lugar)
F10.20$Descr_Lesion_Lugar <- ifelse(F10.20$lesi_lu_id == "4", "Otro", F10.20$Descr_Lesion_Lugar)

##########################################################################
# TABLA DINAMICA - PIVOT TABLE <3 -----------------------------------------

D5 <- PivotTable$new()
D5$addData(S.BASE)
D5$addColumnDataGroups("COVID")
D5$addRowDataGroups("week.in")
D5$defineCalculation(calculationName="Semana.Epidemiologica", summariseExpression="n()")
D5$renderPivot()
# D5$evaluatePivot() # sale como tabla/texto plana


# QPVT = quick-pivot-table: qpvt(data.frame, "filas", "columnas", "funcion de summarise")
d5 <- qpvt(S.BASE, "week.in", "COVID", "n()")


# FUENTES -----------------------------------------------------------------

font_import(path = "C:/Windows/Fonts/")
# El argumento 'device' también soporta "pdf" y "postscript"
loadfonts(device = "win", quiet = TRUE) 

# Especifica la familia de fuente con el nombre que se muestre en la lista fonts()
plot(trees$Volume,
     main = "Fuentes personalizadas en R",
     xlab = "",
     ylab = "Volumen",
     pch = 21, col = 4, bg = 4,
     family = "Algerian") # Estamos estableciendo la fuente "Algerian"

###############################################################################
# GRAFICOS ----------------------------------------------------------------
###############################################################################
# AGREGAR SIMBOLO A ETIQUETAS EJES ----------------------------------------

# TRUCO PARA AGREGAR % AL EJE Y
axisLabels <- seq(0, 60, by=10)
axisLabels <- as.character(axisLabels)
axisLabels <- paste("%", axisLabels)
axis(2, at = seq(0, 60, by=10) , labels =  axisLabels, las=2, cex.axis=1)

###############################################################################
# GUARDO COMO .xlsx -------------------------------------------------------

write_xlsx(Ocupacion.Consolidado, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/1- Monitor Indicadores (Ex-Informe)/Modelo de datos Seguimiento Camas/Ocupacion/OCUP_CONSOLIDADO_R.xlsx")
write_xlsx(SALA.FAL,noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/PPT Sala de Situación/Nuevo informe/gestion.camas.%Y.%m.%d.xlsx"),sep ="."))))

###############################################################################