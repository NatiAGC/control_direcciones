library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)
library(stringr)

LIZA <- read_xls ("C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/Exportaciones/LIZA_PRUEBADOMICILIOS.xls")
SUACI <- read.csv ("C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/Exportaciones/SUACI_PRUEBADOMICILIOS.csv")
ASIG <- read_xlsx ("C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/Exportaciones/Asignaciones221122.xlsx", sheet= "Cargas")



#Selección y nombramiento de columnas útiles
LIZA_DOM <- LIZA %>% 
  select ("Alias", "EntidadInspeccionable", "Actividad")%>%
  rename ("DEN"='Alias')%>%
  rename ("DIR_INSPECCIONADA" = 'EntidadInspeccionable')

ASIG_DOM <- ASIG %>% 
  select ("N Denuncia", "DEN", "Dirección", "Estado")%>%
  rename ("NSUACI"='N Denuncia')%>%
  rename ("DIR_PLANILLA" = 'Dirección')

SUACI_DOM <- SUACI %>%
  select ("Ubicación", "Número", "Observación", "Cuestionario.respondido")%>%
  rename ("DIR_DENUNCIADA" = 'Ubicación')%>%
  rename ("NSUACI" = 'Número')%>%
  rename ("Cuestionario" = 'Cuestionario.respondido')


#Eliminación de filas sin DEN (no fueron planificadas)
#ver si el filtro debe hacerse por estado en planilla luego de la unión
ASIG_DOM <- ASIG_DOM  %>% filter(!is.na(DEN))
ASIG_DOM$DEN <- gsub(" |\r\n|\n|\r|", "", ASIG_DOM$DEN)
ASIG_DOM$NSUACI <- gsub(" |\r\n|\n|\r|", "", ASIG_DOM$NSUACI)

#Unión de DF
DIR_REVISIÓN <- left_join (ASIG_DOM, SUACI_DOM, BY = "NSUACI")
DIR_REVISIÓN <- left_join (DIR_REVISIÓN, LIZA_DOM, BY = "DEN")
#Filtro de las denuncias que no tienen dirección denunciada en SUACI
#DIR_REVISIÓN <- DIR_REVISIÓN  %>% filter(!is.na(DIR_DENUNCIADA))

#control de den con mas de 8 caracteres


#Reordenamiento de columnas y revisión de nulos
order_dir <- c("NSUACI", "DEN", "Estado", "DIR_PLANILLA", "DIR_DENUNCIADA", "DIR_INSPECCIONADA", "Actividad", "Observación", "Cuestionario")
DIR_REVISIÓN <- DIR_REVISIÓN[, order_dir]
apply(X = is.na(DIR_REVISIÓN), MARGIN = 2, FUN = sum)

DIR_REVISIÓN <- mutate(DIR_REVISIÓN, CoincideDir = ifelse(DIR_REVISIÓN ['DIR_PLANILLA'] == DIR_REVISIÓN['DIR_INSPECCIONADA'], "SI","NO"))

DIRunicas <- DIR_REVISIÓN %>% 
  distinct (DIR_INSPECCIONADA)%>%
  rename ("DIRECCIONES" = 'DIR_INSPECCIONADA')
DIRunicas2 <- DIR_REVISIÓN %>%
  distinct (DIR_PLANILLA)%>%
  rename ("DIRECCIONES" = 'DIR_PLANILLA')

DIRunicas <- rbind (DIRunicas, DIRunicas2)
DIRunicas <- DIRunicas %>%
  distinct (DIRECCIONES)


write_xlsx(x = DIRunicas ,path = 'C:/Users/natva/OneDrive/Escritorio/AGC_DATOS/RESULTADOS_CONTROLES/DIRunicas.xlsx')
