rm(list=ls())

## Sacado de http://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r

jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}

options(java.parameters = "-Xmx8000m")
jgc()

require(gdata)
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
require(dplyr) ## Para filtrar como explican acá http://www.r-bloggers.com/dplyr-a-gamechanger-for-data-manipulation-in-r/
require(stringr)
require(stringi)
require(plyr)

########### Amplificación de Ensambles Acústicos #########
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",1, endRow = 82)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 4 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:4),]

## Elimar columas que sobran
AsistenciasNodos <- AsistenciasNodos[,-c(2,3,11,12)]

nombres_columnas <- c("Nombre","Cedula","Sep s4",paste("Oct s",1:3,sep = ""),paste("Nov s",1:2,sep = ""),"Organizacion")

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `Sep s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Nov s1`,`Nov s2`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X")))%>%
    mutate(inscrito = paste(`Sep s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Nov s1`,`Nov s2`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%   mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

nombres_columnas <- c("Nombre","Cedula","Organizacion","Sep s4",paste("Oct s",1:3,sep = ""),paste("Nov s",1:2,sep = ""),"Inscrito")

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.ensamblesacusticos <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`Sep s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Nov s1`,`Nov s2` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Nov s1", "Nov s2")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Ensamble una forma de creación #################
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",2, endRow = 49)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 3 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:3),]

nombres_columnas <- c("Nombre","Cedula","Organizacion",paste("s",1:11,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(  `s1`, `s2`, `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`, `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

AsistenciasNodos

colnames(AsistenciasNodos) <- c(nombres_columnas,"Inscrito")

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.ensablescreacion <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`, `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11`  ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1",  "s2",  "s3",  "s4",  "s5",  "s6",  "s7",  "s8",  "s9",  "s10", "s11")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Ritmos brasileros ###############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",3, endRow = 65)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 3 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:3),]

## Elimar columas que sobran
AsistenciasNodos <- AsistenciasNodos[,-c(2,3,10)]

nombres_columnas <- c("Nombre","Cedula","Organizacion",paste("s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(  `s1`, `s2`, `s3`,  `s4`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`, `s3`,  `s4`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

AsistenciasNodos

colnames(AsistenciasNodos) <- c(nombres_columnas,"Inscrito")

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.ritmosbrasileros <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`, `s3`,  `s4`) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1",  "s2",  "s3",  "s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Algoritmos  ###########
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",4, endRow = 25)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 4 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:4),]

## Elimar columas que sobran
AsistenciasNodos <- AsistenciasNodos[,-c(2,3)]

nombres_columnas <- c("Nombre","Cedula",paste("s",1:12, sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `s1`, `s2`,  `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11`, `s12` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`,  `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11`, `s12`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula")],.)

AsistenciasNodos

colnames(AsistenciasNodos) <- c(nombres_columnas,"Inscrito")

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.algoritmos <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`,  `s3`,  `s4`,  `s5`,  `s6`,  `s7`,  `s8`,  `s9`,  `s10`, `s11`, `s12`) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE) ## Cargué primero dplyr que plyr y se solucionó error http://stackoverflow.com/questions/25794400/ddply-error-with-drop-argument

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1", "s2",  "s3",  "s4",  "s5",  "s6",  "s7",  "s8",  "s9",  "s10", "s11", "s12")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Jazz MikeMoreno  ###########
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",5, endRow = 33)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 4 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:3),]

## Elimar columas que sobran
AsistenciasNodos <- AsistenciasNodos[,-c(2,3,9)]

nombres_columnas <- c("Nombre","Cedula","Organizacion",paste("s",1:3, sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `s1`, `s2`,  `s3`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`,  `s3`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

AsistenciasNodos

colnames(AsistenciasNodos) <- c(nombres_columnas,"Inscrito")

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.mikemoreno <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`,  `s3`) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1",  "s2",  "s3",  "s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Producción Musical - Ciudad Frecuencia  ###########
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",6, endRow = 19)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 4 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:4),]

## Elimar columas que sobran
AsistenciasNodos <- AsistenciasNodos[,-c(2)]

nombres_columnas <- c("Nombre","Organizacion","Correo","Cedula",paste("s",1:6, sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `s1`, `s2`,  `s3`, `s4`, `s5`, `s6`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`,  `s3`, `s4`, `s5`, `s6`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

nombres_columnas <- c("Nombre","Cedula","Organizacion",paste("s",1:6, sep = ""),"Inscrito")
colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.ciudadfrecuencia <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`,  `s3`, `s4`, `s5`, `s6`) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1",  "s2",  "s3",  "s4",  "s5",  "s6")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### Producción Musical - Juan Fernando Giraldo  ###########
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaEscuelaFormadores.xlsx",7, endRow = 19)

nombres_columnas_orig <- colnames(AsistenciasNodos)

## Eliminar primeras 4 filas
AsistenciasNodos <- AsistenciasNodos[-c(1:3),]

## Eliminar columna 2
AsistenciasNodos <- AsistenciasNodos[,-2]

nombres_columnas <- c("Nombre","Organizacion","Correo","Cedula",paste("s",1:6, sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `s1`, `s2`,  `s3`, `s4`, `s5`, `s6`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    mutate(inscrito = paste(`s1`, `s2`,  `s3`, `s4`, `s5`, `s6`, sep = "")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("\\bX.*"),"inscrito")) %>%
    mutate(inscrito = str_replace(inscrito,ignore.case("^$"),"fantasma")) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Cedula","Organizacion")],.)

nombres_columnas <- c("Nombre","Cedula","Organizacion",paste("s",1:6, sep = ""),"Inscrito")
colnames(AsistenciasNodos) <- nombres_columnas

AsistenciasNodos

AsistenciasNodos <- AsistenciasNodos %>%
    filter( Inscrito == "inscrito")

formadores.produccionGuapito <- AsistenciasNodos

pa_pintar <- AsistenciasNodos %>%
    group_by(`s1`, `s2`,  `s3`, `s4`, `s5`, `s6`) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "s1",  "s2",  "s3",  "s4",  "s5",  "s6")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

## Organizo datos de participantes talleres y organizaciones
formadores.algoritmos <- mutate(formadores.algoritmos, Organizacion = "Sin definir", Taller = "Algoritmos")
formadores.mikemoreno <- mutate(formadores.mikemoreno, Taller = "Improvisación y Concepciones del Jazz")
formadores.ciudadfrecuencia <- mutate(formadores.ciudadfrecuencia, Taller = "Producción musical - Ciudad Frecuencia")
formadores.ritmosbrasileros <- mutate(formadores.ritmosbrasileros, Taller = "Ritmos Brasileros")
formadores.produccionGuapito <- mutate(formadores.produccionGuapito, Taller = "Producción musical - Juan Fdo Giraldo")
formadores.ensamblesacusticos <- mutate(formadores.ensamblesacusticos, Taller = "Amplificación de Ensambles Acústicos")
formadores.ensablescreacion <- mutate(formadores.ensablescreacion, Taller = "Ensamble una Forma de Creación")


asistencias.escuela.formadores <- rbind(
    formadores.algoritmos[,c("Nombre","Cedula","Organizacion","Taller")],
    formadores.mikemoreno[,c("Nombre","Cedula","Organizacion","Taller")],
    formadores.ciudadfrecuencia[,c("Nombre","Cedula","Organizacion","Taller")],
    formadores.ritmosbrasileros[,c("Nombre","Cedula","Organizacion","Taller")],
    formadores.produccionGuapito[,c("Nombre","Cedula","Organizacion","Taller")],
    formadores.ensamblesacusticos[,c("Nombre","Cedula","Organizacion","Taller")],   formadores.ensablescreacion[,c("Nombre","Cedula","Organizacion","Taller")]
)

asistencias.escuela.formadores$Organizacion <- str_trim(asistencias.escuela.formadores$Organizacion)

asistencias.escuela.formadores$Organizacion <- stri_trans_totitle(asistencias.escuela.formadores$Organizacion)

asistencias.escuela.formadores$Organizacion <- asistencias.escuela.formadores$Organizacion %>%
    str_replace(.,ignore.case("\\bColegio.*\\b"),"Colegio De Música") %>%
    str_replace(.,ignore.case("\\bDébora Arango.*\\b"),"Débora Arango") %>%
    str_replace(.,ignore.case("\\bGénesis.*\\b"),"Génesis Producciones") %>%
    str_replace(.,ignore.case("\\bIndepen.*\\b"),"Independiente") %>%
    str_replace(.,ignore.case("\\bItm.*\\b"),"ITM") %>%
    str_replace(.,ignore.case("\\b.*ino Express\\b"),"Mocaccino Express") %>%
    str_replace(.,ignore.case("\\b.*De Escuelas\\b"),"Red De Escuelas") %>%
    str_replace(.,ignore.case("\\b.*ventura\\b"),"U San Buenaventura") %>%
    str_replace(.,ignore.case("\\bUniversidad De Antioquia\\b"),"Udea") %>%
    str_replace(.,ignore.case("\\b.*Arte Libre\\b"),"Arte Libre") %>%
    str_replace(.,ignore.case("\\b^$\\b"),"SIN DEFINIR") %>%
    str_replace(.,ignore.case("Sin Definir"),"SIN DEFINIR")

### pinta taller por organizaciones
pinta.organizaciones <- asistencias.escuela.formadores %>%
    group_by("Organizacion","Taller") %>%
    summarise(n = n())

pinta <- pinta.organizaciones

p <- ggplot(pinta, aes(Taller, n)) + geom_bar(stat = "identity", aes(fill = Organizacion), position = "stack" ) + facet_wrap("Taller", scales = "free_x") + guides(fill = guide_legend(ncol = 2)) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete(breaks = NULL) + scale_fill_discrete("Organizaciones")
p


## pinta participantes de la red a que talleres fueron y pinta la participación en esos talleres
asistentes.red <- asistencias.escuela.formadores %>%
    group_by("Organizacion","Taller") %>%
    filter(Organizacion == "Red De Escuelas") %>%
    summarise(n = n())

asistentes.donde.red <- asistencias.escuela.formadores %>%
    group_by("Taller") %>%
    filter( Taller %in% asistentes.red$Taller) %>%
    summarise(n = n())

asistentes.donde.red <- mutate(asistentes.donde.red, Organizacion = "No pertenece a la Red")

asistentes.donde.red$n <- asistentes.donde.red$n - asistentes.red$n

pinta.red <- rbind(
    asistentes.red[,c("Organizacion","Taller","n")],
    asistentes.donde.red[,c("Organizacion","Taller","n")]
)

pinta <- pinta.red

p <- ggplot(pinta, aes(Taller, n)) + geom_bar(stat = "identity", aes(fill = Organizacion), position = "stack" ) + facet_wrap("Taller", scales = "free_x") + guides(fill = guide_legend(ncol = 2)) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete(breaks = NULL) + scale_fill_discrete("Organizaciones")
p + geom_text(aes(Taller, n, ymax = n, label = n))

## Los que fueron a más de uno
asistentes.varios.talleres <- asistencias.escuela.formadores %>%
    group_by("Cedula") %>%
    summarise(n = n()) %>%
    filter(n > 1, Cedula != "")

asistentes.varios.talleres.all <- asistencias.escuela.formadores %>%
    filter(Cedula %in% asistentes.varios.talleres$Cedula)

## escribe a archivo para pintar con raw
write.csv2(asistentes.varios.talleres.all,"/tmp/asistentesvariostalleres.csv")

## Encuestas de satisfacción
