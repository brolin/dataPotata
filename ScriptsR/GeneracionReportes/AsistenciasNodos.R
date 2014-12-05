rm(list=ls())

require(gdata)

## Sacado de http://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}

options(java.parameters = "-Xmx8000m")
jgc()

require(Gmisc)
require(memisc)
require(ggplot2)
require(gridExtra)
require(pander)
require(knitr)
require(RMySQL)
require(sqldf)
require(plyr)
require(R2HTML)
require(reshape2)
require(xlsx)
require(dplyr) ## Para filtrar como explican acá http://www.r-bloggers.com/dplyr-a-gamechanger-for-data-manipulation-in-r/
require(stringr)
require(stringi)

######### Doce de octubre tarde #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",1, endRow = 30)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

nombres_columnas <- colnames(AsistenciasNodos)

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x]"),""))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Doce de octubre mañana #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",2, endRow = 19)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

######### Doce de octubre mañana #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",2, endRow = 19)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x]"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

p

######### Alcázares electrónica  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",3, endRow = 51)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4`) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x]"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p


#############################################################################
######### Alcázares  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",4, endRow = 98)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono"),paste("Ago s",1:3,sep = ""),paste("Sept s",1:5,sep = ""),paste("Oct s",1:5,sep = ""),paste("Nov s",1:4,sep = ""),"Edad")

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Sept s5`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:3,sep = ""),paste("Sept s",1:5,sep = ""),paste("Oct s",1:5,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Sept s5`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Sept s5", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Oct s5", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Belén Electrónica  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",5, endRow = 21)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Belén Percusión  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",6, endRow = 18)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Guayabal  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",7, endRow = 16)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### La Ladera - Hip Hop  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",8, endRow = 20)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select( `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by( `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c( "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### La Ladera Tarde  #############
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",9, endRow = 58)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Nuestra Gente #####
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",10, endRow = 44)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Nuestra Gente Tarde #####
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",11, endRow = 23)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

nombres_columnas <- c(c("Nombre","Teléfono","Edad"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

#############################################################################
######### Pedregal Julián Úsuga ###
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",12, endRow = 60)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:5,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Ago s5`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Ago s5`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Ago s5", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

########### UNA FUNCIÓN #########
graficar_asistencias <- function(hoja, filas) {
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",hoja, endRow = filas)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:4,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4",  "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p
}

## Pedregal otro horario
graficar_asistencias(13,30)
## Los Colores Nicolas Tomas
# graficar_asistencias(14,80) ## No funciona porque noviembre tiene 5 semanas
#############################################################################
######### Los Colores Nicolas Tomas ###
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",14, endRow = 80)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Ago s",1:4,sep = ""),paste("Sept s",1:4,sep = ""),paste("Oct s",1:5,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Ago s1`, `Ago s2`, `Ago s3`, `Ago s4`, `Sept s1`, `Sept s2`, `Sept s3`, `Sept s4`, `Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Ago s1", "Ago s2", "Ago s3", "Ago s4", "Sept s1", "Sept s2", "Sept s3", "Sept s4", "Oct s1", "Oct s2", "Oct s3", "Oct s4", "Oct s5", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

## Los Colores Jornadad de La mañana
graficar_asistencias(15,21)
## Casa de la cultura manrique magaly
graficar_asistencias(16,50)
## Casa de la cultura manrique ele
graficar_asistencias(17,17)
## UVA la esperanza
graficar_asistencias(18,69)
## UVA los sueños jornada mañana
graficar_asistencias(19,28)
## UVA los sueños jornada tarde niños
graficar_asistencias(20,45)
## UVA los sueños jornada tarde ad
graficar_asistencias(21,55)
## AVILA
##graficar_asistencias(22,51) ## No funciona solo tiene octubre y noviembre
#############################################################################
######### Los Colores Nicolas Tomas ###
AsistenciasNodos <- read.xlsx2("./Data/InformeFinal/AsistenciaMusicaEnNodos.xlsx",22, endRow = 51)

## Renombrar columnas
nombres_columnas <- c(c("Nombre","Edad","Teléfono"),paste("Oct s",1:5,sep = ""),paste("Nov s",1:4,sep = ""))

colnames(AsistenciasNodos) <- nombres_columnas

## Eliminar primera fila
AsistenciasNodos <- AsistenciasNodos[-1,]

AsistenciasNodos <- tbl_df(AsistenciasNodos)

AsistenciasNodos <- AsistenciasNodos %>%
    select(`Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    mutate_each(funs(str_trim(.))) %>%
    mutate_each(funs(str_replace(.,ignore.case("\\bnuevo\\b"),"X"))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[^x].*"),""))) %>%
    mutate_each(funs(str_replace(.,ignore.case("[x]"),"X"))) %>%
    data.frame(AsistenciasNodos[,c("Nombre","Teléfono","Edad")],.)

colnames(AsistenciasNodos) <- nombres_columnas

pa_pintar <- AsistenciasNodos %>%
    group_by(`Oct s1`, `Oct s2`, `Oct s3`, `Oct s4`, `Oct s5`, `Nov s1`,`Nov s2`, `Nov s3`, `Nov s4` ) %>%
    tally() %>%
    melt( id.vars=c("n") )

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(arrange(X.tidy,desc(value)), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Oct s1", "Oct s2", "Oct s3", "Oct s4", "Oct s5", "Nov s1", "Nov s2", "Nov s3", "Nov s4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p
