rm(list=ls())

open_in_excel <- function(some_df){
    tFile<-tempfile(fileext=paste0(substitute(some_df), ".csv"),tmpdir="/tmp")
    write.csv2(some_df, tFile)
    system(paste('libreoffice ', tFile, '&'))
}

normaliza_texto <- function(texto) {
    texto <- str_replace_all(texto,"[[:punct:]]"," ")
    texto <- stri_trans_general(texto, "Any-Title")
    texto <- str_replace_all(texto,"\\s{2,4}"," ")
    texto <- stri_trans_general(texto, "latin-ascii")
    texto <- str_trim(texto)
    texto
}

require(plyr)
library(dplyr)
require(tidyr)
require(stringr)
require(stringi)
require(readxl)
require(lubridate)
require(jsonlite)
## devtools::install_github("jrowen/rhandsontable")
require(rhandsontable)

## googlesheets
## require(devtools)
## install.packages("googlesheets")
require(googlesheets)
# Give googlesheets permission to access your spreadsheets and google drive
gs_auth()
user_session_info <- gs_user()
gs_ls("Asistencias")
## If you plan to consume data from a sheet or edit it, you must first register it.
asistencias <- gs_title("Asistencias Casas de La cultura, IE y Hospital")
str(asistencias)
nWorkSheets <- asistencias$n_ws
nSheetNames <- asistencias$ws$ws_title
nSheetNames <- nSheetNames[-c(1,2)]

leer_hojas <- function(ws_title) {
    asistencias %>% gs_read(ws_title)
}

## setNames -> http://stackoverflow.com/questions/15553036/rename-id-column-in-ldply para que tengamos el .id con cada pestaña de donde viene
asistencias_todas <- ldply(setNames(nm = nSheetNames) ,leer_hojas)

## Retira los registros con NA
idx_na <- which(is.na(asistencias_todas$Apellidos))
asistencias_todas <- asistencias_todas[-idx_na,]

## Retira registros con las palabras TOTAL ASISTENTES", "CANTIDAD DE CLASES", "PROMEDIO ASISTENTES", "TOTAL HOMBRES", "TOTAL MUJERES"
retirar_resumen <- c("TOTAL ASISTENTES", "CANTIDAD DE CLASES", "PROMEDIO ASISTENTES", "TOTAL HOMBRES", "TOTAL MUJERES")
idx_retirar <- unlist(lapply(retirar_resumen, function(x) which(asistencias_todas$Apellidos == x)))
asistencias_todas <- asistencias_todas[-idx_retirar,]

asistencias_para_comparar <- asistencias_todas[,c(1,3,4,5)]
colnames(asistencias_para_comparar) <- c("equipamiento","apellidos","nombres","documento")
glimpse(asistencias_para_comparar)

sub_2015 <- read.csv2("./sub_actualizado_2015.csv", sep = ",", skip = 6)
sub_para_comparar <- sub_2015[,c(3,4,5,6,7,25)]
colnames(sub_para_comparar) <- c("documento","primer_nombre","segundo_nombre","primer_apellido","segundo_apellido","equipamiento")
glimpse(sub_2015)

## Concateno nombres para ambas listas
attach(sub_para_comparar)
sub_para_comparar$nombre_apellido <- paste0(primer_nombre," ",segundo_nombre," ",primer_apellido," ",segundo_apellido)
detach(sub_para_comparar)

attach(asistencias_para_comparar)
asistencias_para_comparar$nombre_apellido <- paste0(nombres," ", apellidos)
detach(asistencias_para_comparar)

sub_para_comparar$nombre_apellido <- normaliza_texto(sub_para_comparar$nombre_apellido)
asistencias_para_comparar$nombre_apellido <- normaliza_texto(asistencias_para_comparar$nombre_apellido)

match_nombre_exacto <- inner_join(asistencias_para_comparar, sub_para_comparar, by = 'nombre_apellido')
no_match_nombre_exacto <- anti_join(asistencias_para_comparar, sub_para_comparar, by = 'nombre_apellido')
open_in_excel(match_nombre_exacto)
open_in_excel(no_match_nombre_exacto)
open_in_excel(sub_para_comparar)

## Paste en un solo string todo el nombre para luego compararlo con el sub a través de n-gram
busqueda_nombres <- lapply(setNames(asistencias_para_comparar$nombre_apellido, paste0(asistencias_para_comparar$nombre_apellido," - ",asistencias_para_comparar$equipamiento)), function(x) agrep(x,sub_para_comparar$nombre_apellido,max.distance = 0.2))

## http://stackoverflow.com/questions/9950144/access-lapply-index-names-inside-fun
resultado_busqueda <- lapply(busqueda_nombres, function(x) sub_para_comparar[x,c("equipamiento","nombre_apellido")])

cat(toJSON(resultado_busqueda), file = "ResultadosBusqueda.json")
