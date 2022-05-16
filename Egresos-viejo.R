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


# LEO Y SELECCIONO DATOS DE EGRESOS EN CADA BASE -------------------------------------


SGC <- read_xlsx("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos_SGC.xlsx")
SRH.F10 <- read_xlsx("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Egresos-2022_SRH-CIPRES_2022-03-10.xlsx")
# SRH <- read_xlsx("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos_SRH.xlsx")
# F10 <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos_F10.xlsx", col_types = c("text", "numeric", "numeric", "skip", "skip", "numeric"))


# Busco en "Guia-R.xlsx" los datos de establecimiento para vincularlos --------


Guia <- read_excel("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/1- Monitor Indicadores (Ex-Informe)/Modelo de datos Seguimiento Camas/Establecimientos/Guia-R.xlsx", 
                   col_types = c("text", "text", "text", 
                                 "skip", "text", "text", "skip", "text"))


# Vinculo CODEST de BASES con Guia para agregar las columnas de RS, ESTAB --------

SGC$COD.EST <- stringr::str_pad(SGC$COD.EST, 8, side = "left", pad = 0)
SRH.F10$COD.EST <- stringr::str_pad(SRH$COD.EST, 8, side = "left", pad = 0)
# SRH$COD.EST <- stringr::str_pad(SRH$COD.EST, 8, side = "left", pad = 0)
# F10$COD.EST <- stringr::str_pad(F10$COD.EST, 8, side = "left", pad = 0)


SIGC <- merge(Guia, SGC, by = "COD.EST", all.y = TRUE)
SIRHF10 <- merge(Guia, SRH.F10, by = "COD.EST", all.y = TRUE)

# SIRH <- merge(Guia, SRH, by = "COD.EST", all.y = TRUE)
# f10 <- merge(Guia, F10, by = "COD.EST", all.y = TRUE)


# CAMBIO FORMATO Y NOMBRES DE VARIABLES A?O / MES PARA AGRUPAR ----------------------

# 
# names(SIGC)[names(SIGC) == 'AÑO.SGC'] <- 'AÑO'
# names(SIRH)[names(SIRH) == 'AÑO.SRH'] <- 'AÑO'
# names(f10)[names(f10) == 'AÑO.F10'] <- 'AÑO'

names(SIGC)[names(SIGC) == 'MES.SGC'] <- 'MES'
# names(SIRH)[names(SIRH) == 'MES.SRH'] <- 'MES'
# names(f10)[names(f10) == 'MES.F10'] <- 'MES'

SIGC$MES <- stringr::str_pad(SIGC$MES, 2, side = "left", pad = 0)
SIRH$MES <- stringr::str_pad(SIRH$MES, 2, side = "left", pad = 0)
f10$MES <- stringr::str_pad(f10$MES, 2, side = "left", pad = 0)



# AGRUPO POR ESTABLECIMIENTO ----------------------------------------------


SGC.ESTABL <- SIGC %>% 
  group_by(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, A?O, MES) %>% 
  summarise(EGRESO.MENSUAL.SGC = n())

# SACO LOS "1" QUE ME ARMA AL CONTAR POR FILAS, FILTRANDO POR LOS MESES "VAC?OS"

SGC.ESTABL <- SGC.ESTABL[!(is.na(SGC.ESTABL$MES) | SGC.ESTABL$MES==""), ]


SRH.ESTABL <- SIRH %>% 
  group_by(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, A?O, MES) %>% 
  summarise(EGRESO.SEMESTRAL.SRH = sum(Egreso_Semestral_SRH))


F10.ESTABL <- f10 %>% 
  group_by(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, A?O, MES) %>% 
  summarise(EGRESO.SEMESTRAL.F10 = sum(Egreso_Semestral_F10))

# COMPARACI?N DE EGRESOS MENSUALES EN SGC / SRH / CIPRES ------------------

# EGRESOS <- SGC

EGRESOS <- merge(SGC.ESTABL, merge(SRH.ESTABL, F10.ESTABL, all = TRUE), all = TRUE)
  
#   merge(SGC.ESTABL, SRH.ESTABL, all = TRUE)
#                  
# EGRESOS <- merge(EGRESOS, CIPRES.ESTABL, all = TRUE)
# 
# EGRESOS <- merge(EGRESOS, F10.ESTABL, all = TRUE)

colnames(EGRESOS)


# ORDENO LAS COLUMNAS DE EGRESOS ------------------------------------------

EGRESOS <- EGRESOS %>% 
  select(REGION, ID.PARTIDO, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, 
         A?O, MES, EGRESO.MENSUAL.SGC, EGRESO.SEMESTRAL.SRH, EGRESO.SEMESTRAL.F10)



# GUARDO EGRESOS.XLSX -----------------------------------------------------



write_xlsx(EGRESOS, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos.xlsx")