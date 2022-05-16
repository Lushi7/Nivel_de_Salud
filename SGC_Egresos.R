# LIBRERIAS A USAR --------------------------------------------------------

library(writexl)
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)
library(datos)
library(stringr)
library(lubridate)
library(tidyr)


# LEO Y SELECCIONO DATOS DE EGRESOS EN SGC -------------------------------------

# FILTRO DESDE https://qlik2.ms.gba.gov.ar/hub/stream/ca901132-20be-4a04-8f67-c8d92ff857be
# SELECCIONO MESES DE EGRESO SEGUN AÑO (TODOS LOS CUIDADOS, GRUPOS, SECTORES)

# SIGEC_2021 <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Pacientes SGC/Pacientes SIGEC-Egresos_2021-(2022-03-29).xlsx")

##### CAMBIO FECHA DE DESCARGA DE ARCHIVO!!! #####
SIGEC_2022 <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Pacientes SGC/Pacientes SIGEC-Egresos_2022-(2022-05-11).xlsx")

# SIGEC <- SIGEC_2021
SIGEC <- SIGEC_2022

# colnames(SIGEC)

SGC <- SIGEC %>% 
  select(ESTABLECIMIENTO_ID, FECHA_EGRESO, JURISDICCION)

# SGC <- data.frame(SGC)

# Formato ID.PARTIDO / COD.EST --------------------------------

names(SGC)[names(SGC) == 'ESTABLECIMIENTO_ID'] <- 'COD.EST'

SGC$COD.EST <- stringr::str_pad(SGC$COD.EST, 8, side = "left", pad = 0)
SGC$ID.PARTIDO <- substr(SGC$COD.EST, 1, 3)


# Formato de Fecha a "Fecha Egreso" --------------------------------------------


SGC <- SGC %>% 
  mutate(FECHA_EGRESO= as.Date(FECHA_EGRESO, format="%Y-%m-%D HH:MM:SS"))

# ELIMINO LAS FILAS QUE NO TENGAN FECHA DE EGRESO/ALTA

SGC <- subset(SGC, !(is.na(FECHA_EGRESO)))


# GENERO COLUMNAS "A?O.SGC" Y "MES.SGC" -----------------------------------

SGC$AÑO.SGC <- format(SGC$FECHA_EGRESO, format = "%Y")

SGC$MES.SGC <- format(SGC$FECHA_EGRESO, format = "%m")


SGC$MES.SGC <- stringr::str_pad(SGC$MES.SGC, 2, side = "left", pad = 0)

# GUARDO EGRESOS_SIGEC ----------------------------------------------------


SGC <- SGC %>% 
  select(COD.EST, AÑO.SGC, MES.SGC)

# write_xlsx(SGC, "Egresos_SGC.xlsx")
# SGC <- read_xlsx("Egresos_SGC.xlsx")

################################################################################
# FILTRO POR PUBLICO ------------------------------------------------------

# SIGC <- SGC %>% 
#   filter(SECTOR == "PU")

# write_xlsx(SGC, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos_SGC.xlsx")

write_xlsx(SGC, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022_SGC_%Y-%m-%d.xlsx"),sep ="-"))))


################################################################################
################################################################################
################################################################################
# AGRUPO POR A?O SEGUN REGION / PARITDO / ESTABLECIMIENTO ----------------------
# 
# SGC.REG.ANUAL <- SIGC %>% 
#   group_by(REGION, A?O.SGC) %>% 
#   summarise(Cant.Egr.Anual = n())
# 
# SGC.PART.ANUAL <- SIGC %>% 
#   group_by(REGION, PARTIDO, ID.PARTIDO, A?O.SGC) %>% 
#   summarise(Cant.Egr.Anual = n())
# 
# SGC.EST.ANUAL <- SIGC %>% 
#   group_by(REGION, PARTIDO, ID.PARTIDO, ESTABLECIMIENTO, COD.EST, A?O.SGC) %>% 
#   summarise(Cant.Egr.Anual = n())
# 
# AGRUPO POR REGION -------------------------------------------------------
# 
# SGC.REG <- SIGC %>% 
#   group_by(REGION, A?O.SGC, MES.SGC) %>% 
#   summarise(Cant.Egr.Reg = n())
# 
# ## AGREGO DEPENDENCIA
# 
# SGC.REG.DEP <- SIGC %>% 
#   group_by(REGION, DEPENDENCIA, A?O.SGC, MES.SGC) %>% 
#   summarise(Cant.Egr.Reg = n())
# 
# 
# # AGRUPO POR PARTIDO ------------------------------------------------------
# 
# 
# SGC.PART <- SIGC %>% 
#   group_by(ID.PARTIDO, PARTIDO, A?O.SGC, MES.SGC) %>% 
#   summarise(Cant.Egr.Part = n())
# 
# ## AGREGO DEPENDENCIA
# 
# SGC.PART.DEP <- SIGC %>% 
#   group_by(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, A?O.SGC, MES.SGC) %>% 
#   summarise(Cant.Egr.Part = n())


# AGRUPO POR ESTABLECIMIENTO ----------------------------------------------

# SGC.ESTABL <- SIGC %>% 
#   group_by(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, A?O.SGC, MES.SGC) %>% 
#   summarise(EGRESO.MENSUAL.SGC = n())

# ##############################################################################
