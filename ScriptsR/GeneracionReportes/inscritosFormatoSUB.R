rm(list=ls())

require(gdata)

## Sacado de http://stackoverflow.com/questions/21937640/handling-java-lang-outofmemoryerror-when-writing-to-excel-from-r
jgc <- function()
{
  .jcall("java/lang/System", method = "gc")
}

options(java.parameters = "-Xmx8000m")
jgc()

require(Gmisc) ## Problemas instalándola para la versión 3.1 de R
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

# Se leen datos para cada nodo de los buzones
inscritos_asistencias_profes <- read.xlsx2("./Data/InscritosFormatoSUB/InscritosFormatoSUB_27_08_2014.xlsx",1) # Hoja resumen

inscritos_formato_sub <- read.xlsx2("./Data/InscritosFormatoSUB/InscritosFormatoSUB_27_08_2014.xlsx",3) # Hoja resumen

## GRÁFICAS

p <- ggplot(inscritos_formato_sub, aes(x = Indicador, y = Cifra)) + geom_bar(aes(fill = Indicador) , stat="identity") + theme(axis.text.x  = element_text(angle=90)) + scale_x_discrete(breaks = NULL)
p

p <- ggplot(inscritos_asistencias_profes, aes(x = Indicador, y = as.numeric(as.character(Cifra)))) + geom_bar(aes(fill = Cifra) , stat="identity") + theme(axis.text.x  = element_text(angle=90)) + scale_y_continuous("Cifra")
p

## Análisis de datos del formato SUB
inscritos_formato_sub <- read.xlsx2("./Data/InscritosFormatoSUB/FormatoSUB_CasasDeMusica_refined.xlsx",1)

vars <- colnames(inscritos_formato_sub)

length(which(is.na(as.integer(as.character(inscritos_formato_sub[,8]))))) ## 91 datos sin edad


## Inscritos por nodo y sexo
p <- ggplot(inscritos_formato_sub, aes(inscritos_formato_sub[,7]), na.rm = FALSE) +  geom_bar(aes(fill = inscritos_formato_sub[,7])) + scale_fill_discrete("Sexo") + facet_wrap(vars[30]) + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")
p

## Mostrar cuántos no tienen dato de edad
laply(inscritos_formato_sub$EDAD..DESPLEGAR., as.character)

inscritos_formato_sub[inscritos_formato_sub[,8] == "",]

(inscritos_formato_sub$EDAD..DESPLEGAR.[is.na(inscritos_formato_sub[,8])]) <- as.factor("Sin dato")


## Inscritos por edad y por sexo
p <- ggplot(inscritos_formato_sub, aes(fill = inscritos_formato_sub[,7])) +  geom_histogram(aes(as.integer(as.character(inscritos_formato_sub[,8])), na.keep = TRUE ))  + scale_y_discrete("Inscritos") + scale_x_discrete("Edad") + scale_fill_discrete("Sexo")
p


p <- ggplot(inscritos_formato_sub, aes(as.integer(as.character(inscritos_formato_sub[,8])))) +  geom_bar() + scale_fill_discrete("Sexo") + facet_wrap(vars[7]) + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")
p


## Inscritos por nodo y por edad
p <- ggplot(inscritos_formato_sub, aes(as.integer(as.character(inscritos_formato_sub[,8])), fill = as.character(inscritos_formato_sub[,7] ))) +  geom_bar()

+ scale_fill_discrete("Sexo") + facet_wrap(vars[30]) + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")

p

## Inscritos por edad y jornada
p <- ggplot(inscritos_formato_sub, aes(as.integer(as.character(inscritos_formato_sub[,8])))) +  geom_histogram() + scale_fill_discrete("Sexo") + facet_wrap(vars[23]) + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")
p

## Inscritos por sexo y jornada
p <- ggplot(inscritos_formato_sub, aes(as.integer(as.character(inscritos_formato_sub[,7])))) +  geom_histogram() + scale_fill_discrete("Sexo") + facet_wrap(vars[23]) + scale_y_continuous("Inscritos") + scale_x_discrete("Sexo")
p

## Inscritos por nodo = vars[30] y edad = vars[8], sexo = vars[7]
p <- ggplot(inscritos_formato_sub, aes( as.factor(inscritos_formato_sub[,7]) )) + geom_bar() + facet_wrap(vars[30])
p



p + facet_wrap("Nodo") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.grid.minor = element_blank())


grid.arrange(p,g, ncol = 1)
