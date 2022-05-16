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


SGC22 <- read_xlsx("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022_SGC_2022-05-11.xlsx")
ERC21 <- read_xlsx("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2021_SRH-CIPRES_2022-05-11.xlsx")
ERC22 <- read_xlsx("//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022_SRH-CIPRES_2022-05-11.xlsx")

ERC21$AÑO <- 2021
ERC22$AÑO <- 2022

ERC <- rbind(ERC21, ERC22)
SGC <- SGC22

# Busco en "Guia-R.xlsx" los datos de establecimiento para vincularlos --------


Guia <- read_excel("//10.21.11.5/5-Nivel de Salud/Reparos Cobertura/2022-Luz/Guia-R.xlsx", 
                       col_types = c("text", "text", "text", 
                                     "text", "text", "text", "text", "text", "text"))


# Vinculo CODEST de BASES con Guia para agregar las columnas de RS, ESTAB --------

SGC$COD.EST <- stringr::str_pad(SGC$COD.EST, 8, side = "left", pad = 0)

SIGC <- merge(Guia, SGC, by = "COD.EST", all.y = TRUE)


# CAMBIO FORMATO Y NOMBRES DE VARIABLES A?O / MES PARA AGRUPAR ----------------------

names(SIGC)[names(SIGC) == 'AÑO.SGC'] <- 'AÑO'
names(SIGC)[names(SIGC) == 'MES.SGC'] <- 'MES'

names(ERC)[names(ERC) == 'Mes'] <- 'MES'

SIGC$MES <- stringr::str_pad(SIGC$MES, 2, side = "left", pad = 0)
ERC$MES <- stringr::str_pad(ERC$MES, 2, side = "left", pad = 0)



# AGRUPO POR ESTABLECIMIENTO ----------------------------------------------


SGC.ESTABL <- SIGC %>% 
  group_by(REGION, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, AÑO, MES) %>% 
  summarise(SIGEC = n())

# SACO LOS "1" QUE ME ARMA AL CONTAR POR FILAS, FILTRANDO POR LOS MESES "VAC?OS"

SGC.ESTABL <- SGC.ESTABL[!(is.na(SGC.ESTABL$MES) | SGC.ESTABL$MES==""), ]


SRH.ESTABL <- ERC %>% 
  group_by(REGION, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, AÑO, MES) %>% 
  summarise(SRH = sum(SRH))


F10.ESTABL <- ERC %>% 
  group_by(REGION, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, AÑO, MES) %>% 
  summarise(CIPRES = sum(CIPRES))

# COMPARACION DE EGRESOS MENSUALES EN SGC / SRH / CIPRES ------------------

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
  select(REGION, PARTIDO, DEPENDENCIA, COD.EST, ESTABLECIMIENTO, 
         AÑO, MES, SRH, CIPRES, SIGEC)

EGRESOS <- filter(EGRESOS, REGION != "-")

# GUARDO EGRESOS.XLSX -----------------------------------------------------


# write_xlsx(EGRESOS, noquote(paste(c(format(Sys.time(), "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022_%Y-%m-%d.xlsx"),sep ="-"))))

write_xlsx(EGRESOS, "//10.21.11.5/4-Servicios de Salud/Monitoreo SGC/Bases/Egresos/Egresos-2022.xlsx")
