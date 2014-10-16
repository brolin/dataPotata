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
require(RMySQL) ## No tengo instalado mysql
require(sqldf)
require(plyr)
require(R2HTML)
require(reshape2)
require(xlsx)

require(dplyr) ## Para filtrar como explican acá http://www.r-bloggers.com/dplyr-a-gamechanger-for-data-manipulation-in-r/

## Inscritos bd sub
inscritos_formato_sub <- read.xlsx2("./Data/SUB_join_Asistencias/BD_SUB_CasasDeMusica.xlsx",1)
