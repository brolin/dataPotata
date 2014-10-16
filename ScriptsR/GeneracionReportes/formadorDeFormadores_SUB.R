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

forfor_ensambles <- read.xlsx2("./Data/SUB_FormadorDeFormadores/Lista de asistencia Escuela de formadores COMPLETA.xlsx",1)

pa_pintar <- forfor_ensambles %>% group_by(S1, S2) %>% tally(sort = TRUE) %>% melt( id.vars=c("n"))

X.tidy = ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(sort(X.tidy, decreasing = TRUE), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Sesión 1", "Sesión 2")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

###############################################################################

forfor_ensamble_crea <- read.xlsx2("./Data/SUB_FormadorDeFormadores/Lista de asistencia Escuela de formadores COMPLETA.xlsx",2)

tbl_df(forfor_ensamble_crea)

pa_pintar <- forfor_ensamble_crea %>% group_by(X1, X2, X3) %>% tally(sort = TRUE) %>% melt( id.vars=c("n"))

X.tidy = ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(sort(X.tidy, decreasing = TRUE), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Sesión 1", "Sesión 2", "Sesión 3")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

###############################################################################

forfor_ritmos_bra <- read.xlsx2("./Data/SUB_FormadorDeFormadores/Lista de asistencia Escuela de formadores COMPLETA.xlsx",3)

tbl_df(forfor_ritmos_bra)

pa_pintar <- forfor_ritmos_bra %>% group_by(X1, X2, X3, X4) %>% tally(sort = TRUE) %>% melt( id.vars=c("n"))

X.tidy <- ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(sort(X.tidy, decreasing = TRUE), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Sesión 1", "Sesión 2", "Sesión 3", "Sesión 4")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

###############################################################################

forfor_algo_ritmos <- read.xlsx2("./Data/SUB_FormadorDeFormadores/Lista de asistencia Escuela de formadores COMPLETA.xlsx",4)

tbl_df(forfor_algo_ritmos)

pa_pintar <- forfor_algo_ritmos %>% group_by(X1) %>% tally(sort = TRUE) %>% melt( id.vars=c("n"))

X.tidy = ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(sort(X.tidy, decreasing = TRUE), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Sesión 1")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p

###############################################################################

forfor_improvisaciones <- read.xlsx2("./Data/SUB_FormadorDeFormadores/Lista de asistencia Escuela de formadores COMPLETA.xlsx",5)

tbl_df(forfor_improvisaciones)

pa_pintar <- forfor_improvisaciones %>% group_by(X1, X2, X3) %>% tally(sort = TRUE) %>% melt( id.vars=c("n"))

X.tidy = ddply(pa_pintar, .(variable,value), summarise, count=sum(n), .drop=FALSE)

p <- ggplot(sort(X.tidy, decreasing = TRUE), aes(variable, count)) + geom_bar(stat = "identity", aes(fill = value) ) + scale_y_continuous("Cantidad de inscritos") + scale_x_discrete("Sesiones", labels = c("Sesión 1", "Sesión 2", "Sesión 3")) + scale_fill_discrete("Asistentes", labels =  c("no asistió","asistió"))
p
