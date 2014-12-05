rm(list=ls())

## Sacado de http://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}

options(java.parameters = "-Xmx8000m")
jgc()

require(gdata)
require(jsonlite)
require(Hmisc)
require(Gmisc)
require(memisc)
require(ggplot2)
require(gridExtra)
require(pander)
require(knitr)
require(RMySQL)
require(sqldf)
require(R2HTML)
require(reshape2)
require(xlsx)
require(stringr)
require(dplyr) ## Para filtrar como explican acá http://www.r-bloggers.com/dplyr-a-gamechanger-for-data-manipulation-in-r/
require(stringi) ## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

require(plyr)

# Se leen datos para cada nodo de los buzones
inscritos_asistencias_profes <- read.xlsx2("./Data/InscritosFormatoSUB/InscritosFormatoSUB_27_08_2014.xlsx",1) # Hoja resumen

## GRÁFICAS

p <- ggplot(inscritos_formato_sub, aes(x = Indicador, y = Cifra)) + geom_bar(aes(fill = Indicador) , stat="identity") + theme(axis.text.x  = element_text(angle=90)) + scale_x_discrete(breaks = NULL)
p

p <- ggplot(inscritos_asistencias_profes, aes(x = Indicador, y = as.numeric(as.character(Cifra)))) + geom_bar(aes(fill = Cifra) , stat="identity") + theme(axis.text.x  = element_text(angle=90)) + scale_y_continuous("Cifra")
p

## INFORME FINAL
## RitmosBrasileros
formadores.ritmosbra <- read.xlsx2("./Data/InformeFinal/InscripcionesSUBEscuelaDeFormadores.xlsx", 1, endRow = 36)

nombres_columnas <- c("nombre","tipo_documento","no_documento","sexo","edad","fecha_nacimiento","direccion","barrio","comuna","zona","estrato","telefono","celular","eps","correo","etnia","desplazado","diversidad_sexual","nombre_acudiente","telefono_acudiente")

inscritos_formato_sub <- formadores.ritmosbra

nombres_columnas_orig <- colnames(inscritos_formato_sub)

colnames(inscritos_formato_sub) <- nombres_columnas

## elimina las 6 primeras filas
inscritos_formato_sub <- inscritos_formato_sub[-c(1:6),]

inscritos_sub.tidy <- tbl_df(inscritos_formato_sub)

formadores.ritmosbra <- inscritos_formato_sub

## Amplificación ensambles acústicos
formadores.ampensamacust <- read.xlsx2("./Data/InformeFinal/InscripcionesSUBEscuelaDeFormadores.xlsx", 2, endRow = 21)

nombres_columnas <- c("nombre","tipo_documento","no_documento","sexo","edad","fecha_nacimiento","direccion","barrio","comuna","zona","estrato","telefono","celular","eps","correo","etnia","desplazado","diversidad_sexual","nombre_acudiente","telefono_acudiente")

inscritos_formato_sub <- formadores.ampensamacust

nombres_columnas_orig <- colnames(inscritos_formato_sub)

colnames(inscritos_formato_sub) <- nombres_columnas

inscritos_sub.tidy <- tbl_df(inscritos_formato_sub)

formadores.ampensamacust <- inscritos_formato_sub

## Improvisaciones y concepciones del jazz
formadores.improjazz <- read.xlsx2("./Data/InformeFinal/InscripcionesSUBEscuelaDeFormadores.xlsx", 3, endRow = 23)

nombres_columnas <- c("nombre","tipo_documento","no_documento","sexo","edad","fecha_nacimiento","direccion","barrio","comuna","zona","estrato","telefono","celular","eps","correo","etnia","desplazado","diversidad_sexual","nombre_acudiente","telefono_acudiente")

inscritos_formato_sub <- formadores.improjazz

nombres_columnas_orig <- colnames(inscritos_formato_sub)

colnames(inscritos_formato_sub) <- nombres_columnas

inscritos_sub.tidy <- tbl_df(inscritos_formato_sub)

formadores.improjazz <- inscritos_formato_sub

############################
# Uno todos los formatos de registro de los tres cursos que hay por ahora
escuelaFormadores_sub <- rbind(mutate(formadores.ritmosbra, taller = "Ritmos Brasileros"),mutate(formadores.ampensamacust, taller = "Amplificación de Ensambles Acústicos"),mutate(formadores.improjazz, taller = "Improvisación y Concepciones del Jazz"))

inscritos_sub.tidy <- tbl_df(escuelaFormadores_sub)

## tipo_documento
inscritos_sub.tidy %>%
    group_by("tipo_documento") %>%
    summarise(cuenta = n())

inscritos_sub.tidy$tipo_documento <- str_trim(inscritos_sub.tidy$tipo_documento)

inscritos_sub.tidy$tipo_documento <- str_replace(inscritos_sub.tidy$tipo_documento,".*NO [a-zA-Z].*","EN PROCESO DE VERIFICACIÓN") %>%
    str_replace(.,"^$","EN PROCESO DE VERIFICACIÓN") %>%
    str_replace(.,"\\b^TI.*","TARJETA DE IDENTIDAD") %>%
    str_replace(.,"\\b^R.*","REGISTRO CIVIL") %>%
    str_replace(.,"\\b^CC.*","CÉDULA DE CIUDADANÍA") %>%
    str_replace(.,"N(U|Ú)MERO.*","NÚMERO EQUIVOCADO")

## no_documento
inscritos_sub.tidy %>%
    group_by("no_documento") %>%
    summarise(cuenta = n())

inscritos_sub.tidy$no_documento <- inscritos_sub.tidy$no_documento %>%
    str_replace(.,"(\\d)\\D+(\\d\\D)*","\\1\\2") %>%
    str_replace(.,"^$","EN PROCESO DE VERIFICACIÓN") %>%
    str_replace(.,"[a-zA-Z].*","EN PROCESO DE VERIFICACIÓN") ## Corregir -> está mal

## comuna
inscritos_sub.tidy %>%
    group_by("comuna") %>%
    summarise(cuenta = n())

inscritos_sub.tidy$comuna <- str_trim(inscritos_sub.tidy$comuna)

## buscar como cargar desde un archivo
inscritos_sub.tidy$comuna <- inscritos_sub.tidy$comuna %>%
str_replace(.,ignore.case("\\b1\\b"),"Popular") %>%
str_replace(.,ignore.case("\\b2\\b"),"Santa Cruz") %>%
str_replace(.,ignore.case("\\b3\\b"),"Manrique") %>%
str_replace(.,ignore.case("\\b4\\b"),"Aranjuez") %>%
str_replace(.,ignore.case("\\b5\\b"),"Castilla") %>%
str_replace(.,ignore.case("\\b6\\b"),"Doce de Octubre") %>%
str_replace(.,ignore.case("\\b7\\b"),"Robledo") %>%
str_replace(.,ignore.case("\\b8\\b"),"Villa Hermosa") %>%
str_replace(.,ignore.case("\\b9\\b"),"Buenos Aires") %>%
str_replace(.,ignore.case("\\b10\\b"),"La Candelaria") %>%
str_replace(.,ignore.case("\\b11\\b"),"Laureles Estadio") %>%
str_replace(.,ignore.case("\\b12\\b"),"La América") %>%
str_replace(.,ignore.case("\\b13\\b"),"San Javier") %>%
str_replace(.,ignore.case("\\b14\\b"),"El Poblado") %>%
str_replace(.,ignore.case("\\b15\\b"),"Guayabal") %>%
str_replace(.,ignore.case("\\b16\\b"),"Belen") %>%
str_replace(.,ignore.case("\\b50\\b"),"Corregimiento de San Sebastian de Palmitas") %>%
str_replace(.,ignore.case("\\b60\\b"),"Corregimiento de San Cristobal") %>%
str_replace(.,ignore.case("\\b70\\b"),"Corregimiento de Altavista") %>%
str_replace(.,ignore.case("\\b80\\b"),"Corregimiento de San Antonio de Prado") %>%
str_replace(.,ignore.case("\\b90\\b"),"Corregimiento de Santa Elena") %>%
str_replace(.,ignore.case("\\b99\\b"),"Otro") %>%
str_replace(.,ignore.case("\\ba1\\b"),"Bello") %>%
str_replace(.,ignore.case("\\bA2\\b"),"Copacabana") %>%
str_replace(.,ignore.case("\\bA3\\b"),"Itaguí") %>%
str_replace(.,ignore.case("\\bA4\\b"),"Envigado") %>%
str_replace(.,ignore.case("\\bA5\\b"),"Sabaneta") %>%
str_replace(.,ignore.case("\\bA6\\b"),"Barbosa") %>%
str_replace(.,ignore.case("\\bA7\\b"),"Girardota") %>%
str_replace(.,ignore.case("\\bA8\\b"),"Caldas") %>%
str_replace(.,ignore.case("\\bA9\\b"),"La estrella") %>%
str_replace(.,ignore.case("\\bD1\\b"),"Rionegro") %>%
str_replace(.,ignore.case("\\bD2\\b"),"Guarne") %>%
str_replace(.,ignore.case("\\bD3\\b"),"La Ceja") %>%
str_replace(.,ignore.case("\\bD4\\b"),"El carmen") %>%
str_replace(.,ignore.case("\\bD5\\b"),"Marinilla") %>%
str_replace(.,ignore.case("\\bD6\\b"),"Santuario") %>%
str_replace(.,ignore.case("\\bD7\\b"),"La Unión") %>%
str_replace(.,ignore.case("\\bD8\\b"),"El Retiro")  %>%
str_replace(.,ignore.case("\\bNo registra\\b"),"EN PROCESO DE VERIFICACIÓN")  %>%
    str_replace(.,"^$","EN PROCESO DE VERIFICACIÓN")

## zona
inscritos_sub.tidy %>%
    group_by("zona") %>%
    summarise(cuenta = n())

inscritos_sub.tidy$zona <- str_trim(inscritos_sub.tidy$zona)

## Reemplaza los valores según código SUB
inscritos_sub.tidy$zona <- inscritos_sub.tidy$zona %>%
    str_replace(.,"\\b1\\b","Nor-Oriental") %>%
    str_replace(.,"\\b6\\b","Sur-Occidental") %>%
    str_replace(.,"\\b2\\b","Nor-Occidental") %>%
    str_replace(.,"\\b4\\b","Centro-Occidental") %>%
    str_replace(.,"\\b3\\b","Centro-Oriental") %>%
    str_replace(.,"\\b5\\b","Sur-Oriental") %>%
    str_replace(.,"\\b^$\\b","EN PROCESO DE VERIFICACIÓN") %>%
    str_replace(.,"\\b9\\b","Otro")

## Los que queda NA se reemplazan para verificar
inscritos_sub.tidy$zona[is.na(inscritos_sub.tidy$zona)] <- "EN PROCESO DE VERIFICACIÓN"

## sexo
inscritos_sub.tidy %>%
    group_by("sexo") %>%
    summarise(cuenta = n())

inscritos_sub.tidy$sexo <- inscritos_sub.tidy$sexo %>%
    str_replace(.,ignore.case("\\bf\\b"),"Femenino") %>%
    str_replace(.,ignore.case("\\bm\\b"),"Masculino")

## barrio
inscritos_sub.tidy %>%
    group_by("barrio") %>%
    summarise(cuenta = n())


inscritos_sub.tidy$barrio <- stri_trans_totitle(inscritos_sub.tidy$barrio) ## http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string

inscritos_sub.tidy$barrio <- str_trim(inscritos_sub.tidy$barrio) ## Remueve espacio del inicio y del final

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b101\\b"),"Santo Domingo Savio No.1") %>%
str_replace(.,ignore.case("\\b0101\\b"),"Santo Domingo Savio No.1") %>%
str_replace(.,ignore.case("\\b102\\b"),"Santo Domingo Savio No.2") %>%
str_replace(.,ignore.case("\\b103\\b"),"Popular") %>%
str_replace(.,ignore.case("\\b104\\b"),"Granizal") %>%
str_replace(.,ignore.case("\\b105\\b"),"Moscu No.2") %>%
str_replace(.,ignore.case("\\b106\\b"),"Villa Guadalupe") %>%
str_replace(.,ignore.case("\\b107\\b"),"San Pablo") %>%
str_replace(.,ignore.case("\\b108\\b"),"El Compromiso") %>%
str_replace(.,ignore.case("\\b109\\b"),"Aldea Pablo VI") %>%
str_replace(.,ignore.case("\\b110\\b"),"La Esperanza No.2") %>%
str_replace(.,ignore.case("\\b111\\b"),"La Avanzada") %>%
str_replace(.,ignore.case("\\b112\\b"),"Carpinelo") %>%
str_replace(.,ignore.case("\\b201\\b"),"La Isla") %>%
str_replace(.,ignore.case("\\b202\\b"),"Playon de Los Comuneros") %>%
str_replace(.,ignore.case("\\b203\\b"),"Pablo VI") %>%
str_replace(.,ignore.case("\\b204\\b"),"La Frontera") %>%
str_replace(.,ignore.case("\\b205\\b"),"La Francia") %>%
str_replace(.,ignore.case("\\b206\\b"),"Andalucia") %>%
str_replace(.,ignore.case("\\b207\\b"),"Villa del Socorro") %>%
str_replace(.,ignore.case("\\b208\\b"),"Villa Niza") %>%
str_replace(.,ignore.case("\\b209\\b"),"Moscu No.1") %>%
str_replace(.,ignore.case("\\b0210\\b"),"Santa Cruz") %>% ## Caso especial
str_replace(.,ignore.case("\\b2010\\b"),"Santa Cruz") %>% ## Caso especial
str_replace(.,ignore.case("\\b2\\b"),"Santa Cruz") %>% ## Caso especial
str_replace(.,ignore.case("\\b210\\b"),"Santa Cruz") %>%
str_replace(.,ignore.case("\\b211\\b"),"La Rosa") %>%
str_replace(.,ignore.case("\\b301\\b"),"La Salle") %>%
str_replace(.,ignore.case("\\b302\\b"),"Las Granjas") %>%
str_replace(.,ignore.case("\\b303\\b"),"Campo Valdes No.2") %>%
str_replace(.,ignore.case("\\b304\\b"),"Santa Ines") %>%
str_replace(.,ignore.case("\\b305\\b"),"El Raizal") %>%
str_replace(.,ignore.case("\\b306\\b"),"El Pomar") %>%
str_replace(.,ignore.case("\\b337\\b"),"Manrique Central No.2") %>% ## CAso especial
str_replace(.,ignore.case("\\b307\\b"),"Manrique Central No.2") %>%
str_replace(.,ignore.case("\\b308\\b"),"Manrique Oriental")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b309\\b"),"Versalles No.1") %>%
str_replace(.,ignore.case("\\b310\\b"),"Versalles No.2") %>%
str_replace(.,ignore.case("\\b311\\b"),"La Cruz") %>%
str_replace(.,ignore.case("\\b312\\b"),"Oriente") %>%
str_replace(.,ignore.case("\\b313\\b"),"Maria Cano-Carambolas") %>%
str_replace(.,ignore.case("\\b314\\b"),"San Jose La Cima No.1") %>%
str_replace(.,ignore.case("\\b315\\b"),"San Jose La Cima No.2") %>%
str_replace(.,ignore.case("\\b4\\b"),"Aranjuez") %>%  ## Caso especial, comuna
str_replace(.,ignore.case("\\b401\\b"),"Berlin") %>%
str_replace(.,ignore.case("\\b402\\b"),"San Isidro") %>%
str_replace(.,ignore.case("\\b403\\b"),"Palermo") %>%
str_replace(.,ignore.case("\\b404\\b"),"Bermejal-Los Alamos") %>%
str_replace(.,ignore.case("\\b405\\b"),"Moravia") %>%
str_replace(.,ignore.case("\\b407\\b"),"Sevilla") %>%
str_replace(.,ignore.case("\\b408\\b"),"San Pedro") %>%
str_replace(.,ignore.case("\\b409\\b"),"Manrique Central No.1") %>%
str_replace(.,ignore.case("\\b410\\b"),"Campo Valdes No.1") %>%
str_replace(.,ignore.case("\\b411\\b"),"Las Esmeraldas") %>%
str_replace(.,ignore.case("\\b412\\b"),"La Piñuela") %>%
str_replace(.,ignore.case("\\b413\\b"),"Aranjuez") %>%
str_replace(.,ignore.case("\\b414\\b"),"Brasilia") %>%
str_replace(.,ignore.case("\\b415\\b"),"Miranda") %>%
str_replace(.,ignore.case("\\b501\\b"),"Toscana") %>%
str_replace(.,ignore.case("\\b502\\b"),"Las Brisas") %>%
str_replace(.,ignore.case("\\b503\\b"),"Florencia") %>%
str_replace(.,ignore.case("\\b504\\b"),"Tejelo") %>%
str_replace(.,ignore.case("\\b505\\b"),"Boyaca") %>%
str_replace(.,ignore.case("\\b507\\b"),"Hector Abad Gomez") %>%
str_replace(.,ignore.case("\\b508\\b"),"Belalcazar") %>%
str_replace(.,ignore.case("\\b509\\b"),"Girardot") %>%
str_replace(.,ignore.case("\\b510\\b"),"Tricentenario") %>%
str_replace(.,ignore.case("\\b511\\b"),"Castilla")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b513\\b"),"Francisco Antonio Zea") %>%
str_replace(.,ignore.case("\\b514\\b"),"Alfonso Lopez") %>%
str_replace(.,ignore.case("\\b517\\b"),"Caribe") %>%
str_replace(.,ignore.case("\\b519\\b"),"Progreso") %>%
str_replace(.,ignore.case("\\b601\\b"),"Santander") %>%
str_replace(.,ignore.case("\\b602\\b"),"Doce de Octubre No.1") %>%
str_replace(.,ignore.case("\\b603\\b"),"Doce de Octubre No.2") %>%
str_replace(.,ignore.case("\\b604\\b"),"Pedregal") %>%
str_replace(.,ignore.case("\\b605\\b"),"La Esperanza") %>%
str_replace(.,ignore.case("\\b606\\b"),"San Martin de Porres") %>%
str_replace(.,ignore.case("\\b607\\b"),"Kennedy") %>%
str_replace(.,ignore.case("\\b0608\\b"),"Picacho") %>%
str_replace(.,ignore.case("\\b608\\b"),"Picacho") %>%
str_replace(.,ignore.case("\\b609\\b"),"Picachito") %>%
str_replace(.,ignore.case("\\b610\\b"),"Mirador del Doce") %>%
str_replace(.,ignore.case("\\b611\\b"),"Progreso No.2") %>%
str_replace(.,ignore.case("\\b612\\b"),"El Triunfo") %>%
str_replace(.,ignore.case("\\b7\\b"),"Robledo") %>% ## caso especial, comuna
str_replace(.,ignore.case("\\b702\\b"),"B. Cerro El Volador") %>%
str_replace(.,ignore.case("\\b703\\b"),"San German") %>%
str_replace(.,ignore.case("\\b705\\b"),"Facultad de Minas U. Nacional") %>%
str_replace(.,ignore.case("\\b706\\b"),"La Pilarica") %>%
str_replace(.,ignore.case("\\b707\\b"),"Bosques de San Pablo") %>%
str_replace(.,ignore.case("\\b708\\b"),"Altamira") %>%
str_replace(.,ignore.case("\\b709\\b"),"Cordoba") %>%
str_replace(.,ignore.case("\\b710\\b"),"Lopez de  Mesa") %>%
str_replace(.,ignore.case("\\b711\\b"),"El Diamante") %>%
str_replace(.,ignore.case("\\b712\\b"),"Aures No.2") %>%
str_replace(.,ignore.case("\\b713\\b"),"Aures No.1") %>%
str_replace(.,ignore.case("\\b0714\\b"),"Bello Horizonte") %>%
str_replace(.,ignore.case("\\b714\\b"),"Bello Horizonte") %>%
str_replace(.,ignore.case("\\b715\\b"),"Villa Flora") %>%
str_replace(.,ignore.case("\\b716\\b"),"Palenque") %>%
str_replace(.,ignore.case("\\b717\\b"),"Robledo") %>%
str_replace(.,ignore.case("\\b718\\b"),"Cucaracho") %>%
str_replace(.,ignore.case("\\b719\\b"),"Fuente Clara")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b720\\b"),"Santa Margarita") %>%
str_replace(.,ignore.case("\\b722\\b"),"Olaya Herrera") %>%
str_replace(.,ignore.case("\\b723\\b"),"Pajarito") %>%
str_replace(.,ignore.case("\\b724\\b"),"Monteclaro") %>%
str_replace(.,ignore.case("\\b725\\b"),"Nueva Villa de La Iguana") %>%
str_replace(.,ignore.case("\\b8\\b"),"Villa Hermosa") %>%
str_replace(.,ignore.case("\\b801\\b"),"Villa Hermosa") %>%
str_replace(.,ignore.case("\\b802\\b"),"La Mansion") %>%
str_replace(.,ignore.case("\\b803\\b"),"San Miguel") %>%
str_replace(.,ignore.case("\\b804\\b"),"La Ladera") %>%
str_replace(.,ignore.case("\\b805\\b"),"Batallon Girardot") %>%
str_replace(.,ignore.case("\\b806\\b"),"Llanaditas") %>%
str_replace(.,ignore.case("\\b807\\b"),"Los Mangos") %>%
str_replace(.,ignore.case("\\b808\\b"),"Enciso") %>%
str_replace(.,ignore.case("\\b809\\b"),"Sucre") %>%
str_replace(.,ignore.case("\\b810\\b"),"El Pinal") %>%
str_replace(.,ignore.case("\\b811\\b"),"Trece de Noviembre") %>%
str_replace(.,ignore.case("\\b812\\b"),"La Libertad") %>%
str_replace(.,ignore.case("\\b813\\b"),"Villatina") %>%
str_replace(.,ignore.case("\\b814\\b"),"San Antonio") %>%
str_replace(.,ignore.case("\\b815\\b"),"Las Estancias") %>%
str_replace(.,ignore.case("\\b816\\b"),"Villa Turbay") %>%
str_replace(.,ignore.case("\\b817\\b"),"La Sierra") %>%
str_replace(.,ignore.case("\\b819\\b"),"Villa Lilliam") %>%
str_replace(.,ignore.case("\\b901\\b"),"Juan Pablo II") %>%
str_replace(.,ignore.case("\\b902\\b"),"Barrios de Jesus") %>%
str_replace(.,ignore.case("\\b903\\b"),"Bombona No.2") %>%
str_replace(.,ignore.case("\\b904\\b"),"Los Cerros El Vergel") %>%
str_replace(.,ignore.case("\\b905\\b"),"Alejandro Echavarria") %>%
str_replace(.,ignore.case("\\b906\\b"),"Barrio Caycedo") %>%
str_replace(.,ignore.case("\\b907\\b"),"Buenos Aires") %>%
str_replace(.,ignore.case("\\b908\\b"),"Miraflores") %>%
str_replace(.,ignore.case("\\b909\\b"),"Cataluña") %>%
str_replace(.,ignore.case("\\b910\\b"),"La Milagrosa")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b911\\b"),"Gerona") %>%
str_replace(.,ignore.case("\\b912\\b"),"El Salvador") %>%
str_replace(.,ignore.case("\\b913\\b"),"Loreto") %>%
str_replace(.,ignore.case("\\b914\\b"),"Asomadera No.1") %>%
str_replace(.,ignore.case("\\b915\\b"),"Asomadera No.2") %>%
str_replace(.,ignore.case("\\b916\\b"),"Asomadera No.3") %>%
str_replace(.,ignore.case("\\b917\\b"),"Ocho de Marzo") %>%
str_replace(.,ignore.case("\\b1001\\b"),"Prado") %>%
str_replace(.,ignore.case("\\b1003\\b"),"Jesus Nazareno") %>%
str_replace(.,ignore.case("\\b1004\\b"),"El Chagualo") %>%
str_replace(.,ignore.case("\\b1005\\b"),"Estacion Villa") %>%
str_replace(.,ignore.case("\\b1006\\b"),"San Benito") %>%
str_replace(.,ignore.case("\\b1007\\b"),"Guayaquil") %>%
str_replace(.,ignore.case("\\b1008\\b"),"Corazon de Jesus") %>%
str_replace(.,ignore.case("\\b1011\\b"),"Calle Nueva") %>%
str_replace(.,ignore.case("\\b1012\\b"),"Perpetuo Socorro") %>%
str_replace(.,ignore.case("\\b1013\\b"),"Barrio Colon") %>%
str_replace(.,ignore.case("\\b1014\\b"),"Las Palmas") %>%
str_replace(.,ignore.case("\\b1015\\b"),"Bombona No.1") %>%
str_replace(.,ignore.case("\\b1016\\b"),"Boston") %>%
str_replace(.,ignore.case("\\b1017\\b"),"Los Angeles") %>%
str_replace(.,ignore.case("\\b1018\\b"),"Villa Nueva") %>%
str_replace(.,ignore.case("\\b1019\\b"),"La Candelaria") %>%
str_replace(.,ignore.case("\\b1020\\b"),"San Diego") %>%
str_replace(.,ignore.case("\\b1101\\b"),"Carlos E. Restrepo") %>%
str_replace(.,ignore.case("\\b1102\\b"),"Suramericana") %>%
str_replace(.,ignore.case("\\b1103\\b"),"Naranjal") %>%
str_replace(.,ignore.case("\\b1104\\b"),"San Joaquin") %>%
str_replace(.,ignore.case("\\b1105\\b"),"Los Conquistadores") %>%
str_replace(.,ignore.case("\\b1107\\b"),"Bolivariana") %>%
str_replace(.,ignore.case("\\b1108\\b"),"Laureles") %>%
str_replace(.,ignore.case("\\b1109\\b"),"Las Acacias") %>%
str_replace(.,ignore.case("\\b1110\\b"),"La Castellana") %>%
str_replace(.,ignore.case("\\b1111\\b"),"Lorena")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b1112\\b"),"El Velodromo") %>%
str_replace(.,ignore.case("\\b1113\\b"),"Estadio") %>%
str_replace(.,ignore.case("\\b1114\\b"),"Los Colores") %>%
str_replace(.,ignore.case("\\b1115\\b"),"Cuarta Brigada") %>%
str_replace(.,ignore.case("\\b1117\\b"),"Florida Nueva") %>%
str_replace(.,ignore.case("\\b12\\b"),"La América") %>% ## Codificado barrio como la comuna
str_replace(.,ignore.case("\\b1201\\b"),"Ferrini") %>%
str_replace(.,ignore.case("\\b1202\\b"),"Calasanz") %>%
str_replace(.,ignore.case("\\b1203\\b"),"Los Pinos") %>%
str_replace(.,ignore.case("\\b1204\\b"),"La America") %>%
str_replace(.,ignore.case("\\b1205\\b"),"La Floresta") %>%
str_replace(.,ignore.case("\\b1206\\b"),"Santa Lucia") %>%
str_replace(.,ignore.case("\\b1207\\b"),"El Danubio") %>%
str_replace(.,ignore.case("\\b1208\\b"),"Campo Alegre") %>%
str_replace(.,ignore.case("\\b1209\\b"),"Santa Monica") %>%
str_replace(.,ignore.case("\\b1210\\b"),"Barrio Cristobal") %>%
str_replace(.,ignore.case("\\b1211\\b"),"Simon Bolivar") %>%
str_replace(.,ignore.case("\\b1212\\b"),"Santa Teresita") %>%
str_replace(.,ignore.case("\\b1213\\b"),"Calasanz Parte Alta") %>%
str_replace(.,ignore.case("\\b13013\\b"),"El Pesebre") %>% ## Caso especial, creo que es este valor
str_replace(.,ignore.case("\\b1301\\b"),"El Pesebre") %>%
str_replace(.,ignore.case("\\b1302\\b"),"Blanquizal") %>%
str_replace(.,ignore.case("\\b1303\\b"),"Santa Rosa de Lima") %>%
str_replace(.,ignore.case("\\b1304\\b"),"Los Alcazares") %>%
str_replace(.,ignore.case("\\b1305\\b"),"Metropolitano") %>%
str_replace(.,ignore.case("\\b1306\\b"),"La Pradera") %>%
str_replace(.,ignore.case("\\b1307\\b"),"Juan XXIII La Quiebra") %>%
str_replace(.,ignore.case("\\b1308\\b"),"San Javier No.2") %>%
str_replace(.,ignore.case("\\b1309\\b"),"San Javier No.1") %>%
str_replace(.,ignore.case("\\b1310\\b"),"Veinte de Julio") %>%
str_replace(.,ignore.case("\\b1311\\b"),"Belencito")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b1312\\b"),"Betania") %>%
str_replace(.,ignore.case("\\b1313\\b"),"El Corazon") %>%
str_replace(.,ignore.case("\\b1314\\b"),"Las Independencias") %>%
str_replace(.,ignore.case("\\b1315\\b"),"Nuevos Conquistadores") %>%
str_replace(.,ignore.case("\\b1316\\b"),"El Salado") %>%
str_replace(.,ignore.case("\\b1317\\b"),"Eduardo Santos") %>%
str_replace(.,ignore.case("\\b1318\\b"),"Antonio Nari±o") %>%
str_replace(.,ignore.case("\\b1319\\b"),"El Socorro") %>%
str_replace(.,ignore.case("\\b1401\\b"),"Barrio Colombia") %>%
str_replace(.,ignore.case("\\b1403\\b"),"Villa Carlota") %>%
str_replace(.,ignore.case("\\b1404\\b"),"Castropol") %>%
str_replace(.,ignore.case("\\b1405\\b"),"Lalinde") %>%
str_replace(.,ignore.case("\\b1406\\b"),"Las Lomas No.1") %>%
str_replace(.,ignore.case("\\b1407\\b"),"Las Lomas No.2") %>%
str_replace(.,ignore.case("\\b1408\\b"),"Altos del Poblado") %>%
str_replace(.,ignore.case("\\b1409\\b"),"El Tesoro") %>%
str_replace(.,ignore.case("\\b1410\\b"),"Los Naranjos") %>%
str_replace(.,ignore.case("\\b1411\\b"),"Los Balsos No.1") %>%
str_replace(.,ignore.case("\\b1412\\b"),"San Lucas") %>%
str_replace(.,ignore.case("\\b1413\\b"),"El Diamante No.2") %>%
str_replace(.,ignore.case("\\b1414\\b"),"El Castillo") %>%
str_replace(.,ignore.case("\\b1415\\b"),"Los Balsos No.2") %>%
str_replace(.,ignore.case("\\b1416\\b"),"Alejandria") %>%
str_replace(.,ignore.case("\\b1417\\b"),"La Florida") %>%
str_replace(.,ignore.case("\\b1418\\b"),"El Poblado") %>%
str_replace(.,ignore.case("\\b1419\\b"),"Manila") %>%
str_replace(.,ignore.case("\\b1420\\b"),"Astorga") %>%
str_replace(.,ignore.case("\\b1421\\b"),"Patio Bonito") %>%
str_replace(.,ignore.case("\\b1422\\b"),"La Aguacatala") %>%
str_replace(.,ignore.case("\\b1423\\b"),"Santa Maria de Los Angeles") %>%
str_replace(.,ignore.case("\\b1502\\b"),"Tenche") %>%
str_replace(.,ignore.case("\\b1503\\b"),"Trinidad") %>%
str_replace(.,ignore.case("\\b1504\\b"),"Santa Fe") %>%
str_replace(.,ignore.case("\\b1507\\b"),"Campo Amor")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b1509\\b"),"Cristo Rey") %>%
str_replace(.,ignore.case("\\b1510\\b"),"Guayabal") %>%
str_replace(.,ignore.case("\\b1511\\b"),"La Colina") %>%
str_replace(.,ignore.case("\\b1601\\b"),"Fatima") %>%
str_replace(.,ignore.case("\\b1602\\b"),"Rosales") %>%
str_replace(.,ignore.case("\\b1603\\b"),"Belen") %>%
str_replace(.,ignore.case("\\bbel.n\\b"),"Belen") %>%
str_replace(.,ignore.case("\\b1604\\b"),"Granada") %>%
str_replace(.,ignore.case("\\b1605\\b"),"San Bernardo") %>%
str_replace(.,ignore.case("\\b1606\\b"),"Las Playas") %>%
str_replace(.,ignore.case("\\b1607\\b"),"Diego Echavarria") %>%
str_replace(.,ignore.case("\\b1608\\b"),"La Mota") %>%
str_replace(.,ignore.case("\\b1609\\b"),"La Hondonada") %>%
str_replace(.,ignore.case("\\b1610\\b"),"El Rincon") %>%
str_replace(.,ignore.case("\\b1611\\b"),"La Loma de Los Bernal") %>%
str_replace(.,ignore.case("\\b1612\\b"),"La Gloria") %>%
str_replace(.,ignore.case("\\b1613\\b"),"Altavista") %>%
str_replace(.,ignore.case("\\b1614\\b"),"La Palma") %>%
str_replace(.,ignore.case("\\b1615\\b"),"Los Alpes") %>%
str_replace(.,ignore.case("\\b1616\\b"),"Las Violetas") %>%
str_replace(.,ignore.case("\\b1617\\b"),"Las Mercedes") %>%
str_replace(.,ignore.case("\\b1618\\b"),"Nueva Villa del Aburra") %>%
str_replace(.,ignore.case("\\b1619\\b"),"Miravalle") %>%
str_replace(.,ignore.case("\\b1620\\b"),"El Nogal-Los Almendros") %>%
str_replace(.,ignore.case("\\b1621\\b"),"Cerro Nutibara") %>%
str_replace(.,ignore.case("\\b5001\\b"),"La Suiza") %>%
str_replace(.,ignore.case("\\b5002\\b"),"La Sucia") %>%
str_replace(.,ignore.case("\\b5003\\b"),"Urquita") %>%
str_replace(.,ignore.case("\\b5004\\b"),"Palmitas Sector Central") %>%
str_replace(.,ignore.case("\\b5005\\b"),"Volcana Guayabal") %>%
str_replace(.,ignore.case("\\b5006\\b"),"La Frisola") %>%
str_replace(.,ignore.case("\\b5007\\b"),"La Aldea") %>%
str_replace(.,ignore.case("\\b5008\\b"),"Potrera Miserenga") %>%
str_replace(.,ignore.case("\\b6001\\b"),"La Palma") %>%
str_replace(.,ignore.case("\\b6002\\b"),"El Patio")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b6003\\b"),"El Uvito") %>%
str_replace(.,ignore.case("\\b6004\\b"),"La Cuchilla") %>%
str_replace(.,ignore.case("\\b6005\\b"),"Naranjal") %>%
str_replace(.,ignore.case("\\b6006\\b"),"Boqueron") %>%
str_replace(.,ignore.case("\\b6007\\b"),"San Jose de La Monta±a") %>%
str_replace(.,ignore.case("\\b6008\\b"),"La Ilusion") %>%
str_replace(.,ignore.case("\\b6009\\b"),"Yolombo") %>%
str_replace(.,ignore.case("\\b6010\\b"),"El Carmelo") %>%
str_replace(.,ignore.case("\\b6011\\b"),"El Picacho") %>%
str_replace(.,ignore.case("\\b6012\\b"),"Pajarito") %>%
str_replace(.,ignore.case("\\b6013\\b"),"Pedregal Alto") %>%
str_replace(.,ignore.case("\\b6014\\b"),"La Loma") %>%
str_replace(.,ignore.case("\\b6015\\b"),"Las Playas") %>%
str_replace(.,ignore.case("\\b6016\\b"),"Travesias") %>%
str_replace(.,ignore.case("\\b6017\\b"),"El Llano") %>%
str_replace(.,ignore.case("\\b7001\\b"),"Buga Patio Bonito") %>%
str_replace(.,ignore.case("\\b7002\\b"),"Aguas Frias") %>%
str_replace(.,ignore.case("\\b7003\\b"),"El Corazon El Morro") %>%
str_replace(.,ignore.case("\\b7004\\b"),"San Pablo") %>%
str_replace(.,ignore.case("\\b7005\\b"),"Altavista Sector Central") %>%
str_replace(.,ignore.case("\\b7006\\b"),"La Esperanza") %>%
str_replace(.,ignore.case("\\b7007\\b"),"San Jose del Manzanillo") %>%
str_replace(.,ignore.case("\\b7008\\b"),"El Jardin") %>%
str_replace(.,ignore.case("\\b8001\\b"),"La Florida") %>%
str_replace(.,ignore.case("\\b8002\\b"),"Potrerito") %>%
str_replace(.,ignore.case("\\b8003\\b"),"Montañita") %>%
str_replace(.,ignore.case("\\b8004\\b"),"Yarumalito") %>%
str_replace(.,ignore.case("\\b8005\\b"),"El Astillero") %>%
str_replace(.,ignore.case("\\b8006\\b"),"El Salado") %>%
str_replace(.,ignore.case("\\b8007\\b"),"La Verde") %>%
str_replace(.,ignore.case("\\b8008\\b"),"San Jose") %>%
str_replace(.,ignore.case("\\b9001\\b"),"Las Palmas") %>%
str_replace(.,ignore.case("\\b9002\\b"),"Media Luna") %>%
str_replace(.,ignore.case("\\b9003\\b"),"Piedras Blancas - Matasano")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b9004\\b"),"Barro Blanco") %>%
str_replace(.,ignore.case("\\b9005\\b"),"El Placer") %>%
str_replace(.,ignore.case("\\b9006\\b"),"Santa Elena Sector Central") %>%
str_replace(.,ignore.case("\\b9007\\b"),"El Cerro") %>%
str_replace(.,ignore.case("\\b9008\\b"),"El Llano SE") %>%
str_replace(.,ignore.case("\\b9009\\b"),"El Plan") %>%
str_replace(.,ignore.case("\\b9010\\b"),"Piedra Gorda") %>%
str_replace(.,ignore.case("\\b9011\\b"),"Mazo") %>%
str_replace(.,ignore.case("\\bAE1\\b"),"Area de Expansion Pajarito") %>%
str_replace(.,ignore.case("\\bAE2\\b"),"Area de Expansion San Cristobal") %>%
str_replace(.,ignore.case("\\bAE3\\b"),"Eduardo Santos") %>%
str_replace(.,ignore.case("\\bAE4\\b"),"Area de Expansion El Noral") %>%
str_replace(.,ignore.case("\\bAE5\\b"),"Area de Expansion Altavista") %>%
str_replace(.,ignore.case("\\bAE6\\b"),"Area de Expansion BelÚn Rincon") %>%
str_replace(.,ignore.case("\\bAE7\\b"),"Area de Expansion San Antonio de Prado") %>%
str_replace(.,ignore.case("\\bAUC1\\b"),"Cabecera Urbana Corregimiento San Cristobal") %>%
str_replace(.,ignore.case("\\bAUC2\\b"),"San Antonio de Prado") %>%
str_replace(.,ignore.case("\\bINST_1\\b"),"Plaza de Ferias") %>%
str_replace(.,ignore.case("\\bINST_10\\b"),"Jardin Botanico") %>%
str_replace(.,ignore.case("\\bINST_11\\b"),"Universidad de Antioquia") %>%
str_replace(.,ignore.case("\\bINST_12\\b"),"Hospital San Vicente de Paul") %>%
str_replace(.,ignore.case("\\bINST_13\\b"),"Batallon Cuarta Brigada") %>%
str_replace(.,ignore.case("\\bINST_14\\b"),"U.D. Atanasio Girardot") %>%
str_replace(.,ignore.case("\\bINST_15\\b"),"U.P.B") %>%
str_replace(.,ignore.case("\\bINST_16\\b"),"La Alpujarra") %>%
str_replace(.,ignore.case("\\bINST_17\\b"),"Centro Administrativo") %>%
str_replace(.,ignore.case("\\bINST_18\\b"),"Cerro Nutibara") %>%
str_replace(.,ignore.case("\\bINST_19\\b"),"Parque Juan Pablo II") %>%
str_replace(.,ignore.case("\\bINST_2\\b"),"Oleoducto") %>%
str_replace(.,ignore.case("\\bINST_20\\b"),"El Rodeo") %>%
str_replace(.,ignore.case("\\bINST_3\\b"),"Cementerio Universal") %>%
str_replace(.,ignore.case("\\bINST_4\\b"),"Terminal de Transporte")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\bINST_5\\b"),"Facultad de Minas U. Nal") %>%
str_replace(.,ignore.case("\\bINST_6\\b"),"Fac. Veterinaria y Zootecnia U.de.A.") %>%
str_replace(.,ignore.case("\\bINST_7\\b"),"Ecoparque Cerro El Volador") %>%
str_replace(.,ignore.case("\\bINST_8\\b"),"Universidad Nacional") %>%
str_replace(.,ignore.case("\\bINST_9\\b"),"Parque Norte") %>%
str_replace(.,ignore.case("\\bSN01\\b"),"Sin Nombre") %>%
str_replace(.,ignore.case("\\bSN02\\b"),"Sin Nombre") %>%
str_replace(.,ignore.case("\\bA1\\b"),"Bello") %>% ## Codificado como el corregimiento
str_replace(.,ignore.case("\\bA101\\b"),"Bello") %>%
str_replace(.,ignore.case("\\bA201\\b"),"Copacabana") %>%
str_replace(.,ignore.case("\\bA301\\b"),"Itaguí") %>%
str_replace(.,ignore.case("\\bA401\\b"),"Envigado") %>%
str_replace(.,ignore.case("\\bA501\\b"),"Sabaneta") %>%
str_replace(.,ignore.case("\\bA601\\b"),"Barbosa") %>%
str_replace(.,ignore.case("\\bA701\\b"),"Girardota") %>%
str_replace(.,ignore.case("\\bA801\\b"),"Caldas") %>%
str_replace(.,ignore.case("\\bA901\\b"),"La estrella") %>%
str_replace(.,ignore.case("\\bD101\\b"),"Rionegro") %>%
str_replace(.,ignore.case("\\bD201\\b"),"Guarne") %>%
str_replace(.,ignore.case("\\bD301\\b"),"La Ceja") %>%
str_replace(.,ignore.case("\\bD401\\b"),"El carmen") %>%
str_replace(.,ignore.case("\\bD501\\b"),"Marinilla") %>%
str_replace(.,ignore.case("\\bD601\\b"),"Santuario") %>%
str_replace(.,ignore.case("\\bD701\\b"),"La Unión") %>%
str_replace(.,ignore.case("\\bD801\\b"),"El Retiro")

inscritos_sub.tidy$barrio <- inscritos_sub.tidy$barrio %>%
str_replace(.,ignore.case("\\b9901\\b"),"Otro") %>%
str_replace(.,ignore.case("\\bStadio\\b"),"Estadio") %>%
str_replace(.,ignore.case("\\bBelen\\b"),"Belén") %>%
str_replace(.,"^$","EN PROCESO DE VERIFICACIÓN")

## taller
inscritos_sub.tidy %>%
    group_by("taller") %>%
    summarise(cuenta = n())


## el organizado lo ponemos como el que se viene graficando
inscritos_formato_sub <- inscritos_sub.tidy


## Ubicación por comuna
p <- ggplot(inscritos_formato_sub, aes(`comuna`)) +  geom_bar(aes(fill = `comuna`)) + scale_fill_discrete("Comuna") + scale_x_discrete("") + scale_y_continuous("Cantidad de inscritos") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + facet_wrap("taller")
p

dev.new()

inscritos_sub.tidy%>%
    group_by(comuna) %>%
    summarise(participantes = n()) %>%
    arrange(desc(participantes))

## ## Ubicación por zona
## p <- ggplot(inscritos_formato_sub, aes(`zona`)) +  geom_bar(aes(fill = `zona`)) + scale_fill_discrete("Zona") + scale_x_discrete("") + scale_y_continuous("Cantidad de inscritos")
## p

## inscritos %>%
##     group_by(zona) %>%
##     summarise(participantes = n()) %>%
##     arrange(desc(participantes))

## Inscritos por nodo y sexo
p <- ggplot(inscritos_formato_sub, aes(`sexo`), na.rm = FALSE) +  geom_bar(aes(fill = `sexo`)) + scale_fill_discrete("Sexo") + facet_wrap("taller") + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")
p

dev.new()

## nombres_columnas en lugar de vars
inscritos_formato_sub %>%
    group_by(nombre_equipamiento_que_asiste, sexo) %>%
    summarise(participantes = n())



## Inscritos por edad y por sexo
p <- ggplot(inscritos_formato_sub, aes(fill = `sexo`)) +  geom_histogram(aes(as.integer(as.character(`edad`)), na.keep = TRUE ))  + scale_y_discrete("Inscritos") + scale_x_discrete("Edad") + scale_fill_discrete("Sexo")
p

dev.new()

inscritos %>%
    group_by(edad, sexo) %>%
    summarise(participantes = n())


## Inscritos por barrio
inscritos_formato_sub %>%
    select(zona, barrio) %>%
    group_by(zona, barrio) %>%
    top_n(10)


p <- ggplot(inscritos_formato_sub, aes(`barrio`), na.rm = FALSE) +  geom_bar(aes(fill = `barrio`)) + scale_fill_discrete(breaks = NULL) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Barrio") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p

dev.new()

## Inscritos por Tipo de documento
p <- ggplot(inscritos_formato_sub, aes(`tipo_documento`), na.rm = FALSE) +  geom_bar(aes(fill = "documento")) + scale_fill_discrete(breaks = NULL) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Tipo de documento") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p



## Test zone
pattern <- "\\bEducación Superior.*"
inscritos_sub.tidy$grado %>% str_match(.,ignore.case(pattern))
cuales <- which(!is.na(inscritos_sub.tidy$grado %>% str_match(.,pattern)))
View(inscritos_sub.tidy[cuales,"grado"])

inscritos_sub.tidy[inscritos_sub.tidy$no_documento == "8155540",]

## Test zone
