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
require(ggplot2)
require(R2HTML)
require(reshape2)
require(xlsx)

# Se leen datos para cada nodo de los buzones
uva_la_esperanza <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",1) # 60
belen_percusion <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",2) # 12
belen_electronica <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",3) # 13
la_ladera <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",4) # 17
casa_de_la_memoria <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",5) # 0
pedregal_2a5 <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",6) # 40
pedregal_9a12 <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",7) # 7
guayabal <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",8) # 8
nuestra_gente <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",9) # 32
uva_de_los_suenos_manana<- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",10) # 19
uva_de_los_suenos_tarde <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",11) # 56
colores_tarde <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",12) # 46
colores_manana <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",13) # 12
colores_electronica <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",14) # 0
doce_de_octubre_tarde <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",15) # 34
doce_de_octubre_manana <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",16) # 12
alcazares <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",17) # 62
alcazares_electronica  <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",18) # 14
casa_gardeliana <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",19) # 33
casa_gardeliana_electronica <- read.xlsx2("./Data/FORMATO ASISTENCIA.xlsx",20) # 10

sum(c(60,12,13,17,0,40,7,8,32,19,56,46,12,0,34,12,62,14,33,10)) # Total inscritos

agregarNodoDiaJornada <- function(df,nombre_nodo, dia_semana, jornada) {
    pa_agregar <- data.frame(rep(nombre_nodo,length(rownames(df))),rep(dia_semana,length(rownames(df))),rep(jornada,length(rownames(df))))
    colnames(pa_agregar) <- c("Nodo","Día de la semana","Jornada")
    if(length(levels(as.factor(df$F))) > 1) { df$F <- recode(as.factor(df$F),"Femenino" <- "X", otherwise = "") }
    if(length(levels(as.factor(df$M))) > 1) { df$M <- recode(as.factor(df$M),"Masculino" <- "X", otherwise = "") }
    pa_agregar$Sexo <- paste(df$F,df$M)
    pa_agregar
}

uva_la_esperanza_con_nodo <- data.frame(agregarNodoDiaJornada(uva_la_esperanza,"UVA (EPM) Esperanza","Viernes","Mañana"), uva_la_esperanza, stringsAsFactors=FALSE )
belen_percusion_con_nodo <- data.frame(agregarNodoDiaJornada(belen_percusion,"Parque Biblioteca Belén","Martes","Tarde"), belen_percusion, stringsAsFactors=FALSE )
belen_electronica_con_nodo <- data.frame(agregarNodoDiaJornada(belen_electronica,"Parque Biblioteca Belén","Lunes","Tarde"), belen_electronica, stringsAsFactors=FALSE )
la_ladera_con_nodo <- data.frame(agregarNodoDiaJornada(la_ladera,"Parque Biblioteca León de Greiff - La Ladera","Jueves","Tarde"), la_ladera, stringsAsFactors=FALSE )
casa_de_la_memoria_con_nodo <- data.frame(agregarNodoDiaJornada(casa_de_la_memoria,"Museo Casa de la Memoria","Sin definir","Sin definir"), casa_de_la_memoria, stringsAsFactors=FALSE )
pedregal_2a5_con_nodo <- data.frame(agregarNodoDiaJornada(pedregal_2a5,"Centro Cultural y Teatro al Aire Libre de Pedregal","Lunes","Tarde"), pedregal_2a5, stringsAsFactors=FALSE )
pedregal_9a12 <- data.frame(agregarNodoDiaJornada(pedregal_9a12,"Centro Cultural y Teatro al Aire Libre de Pedregal","Martes","Mañana"), pedregal_9a12, stringsAsFactors=FALSE )
guayabal_con_nodo <- data.frame(agregarNodoDiaJornada(guayabal,"Parque Biblioteca Manuel Mejía Vallejo - Guayabal","Viernes","Tarde"), guayabal, stringsAsFactors=FALSE )
nuestra_gente_con_nodo <- data.frame(agregarNodoDiaJornada(nuestra_gente,"Corporación Cultural Nuestra Gente","Miércoles","Tarde"), nuestra_gente, stringsAsFactors=FALSE )
uva_de_los_suenos_manana_con_nodo <- data.frame(agregarNodoDiaJornada(uva_de_los_suenos_manana,"UVA (EPM) Sueños","Viernes","Mañana"), uva_de_los_suenos_manana, stringsAsFactors=FALSE )
uva_de_los_suenos_tarde_con_nodo <- data.frame(agregarNodoDiaJornada(uva_de_los_suenos_tarde,"UVA (EPM) Sueños","Viernes","Tarde"), uva_de_los_suenos_tarde, stringsAsFactors=FALSE )
colores_tarde_con_nodo <- data.frame(agregarNodoDiaJornada(colores_tarde,"Centro Cultural y Juvenil Los Colores","Jueves","Tarde"), colores_tarde, stringsAsFactors=FALSE )
colores_manana_con_nodo <- data.frame(agregarNodoDiaJornada(colores_manana,"Centro Cultural y Juvenil Los Colores","Miércoles","Mañana"), colores_manana, stringsAsFactors=FALSE )
colores_electronica_con_nodo <- data.frame(agregarNodoDiaJornada(colores_electronica,"Centro Cultural y Juvenil Los Colores","Miércoles","Mañana"), colores_electronica, stringsAsFactors=FALSE )
doce_de_octubre_tarde_con_nodo <- data.frame(agregarNodoDiaJornada(doce_de_octubre_tarde,"El parque biblioteca Doce de Octubre","Martes","Tarde"), doce_de_octubre_tarde, stringsAsFactors=FALSE )
doce_de_octubre_manana_con_nodo <- data.frame(agregarNodoDiaJornada(doce_de_octubre_manana,"El parque biblioteca Doce de Octubre","Martes","Mañana"), doce_de_octubre_manana, stringsAsFactors=FALSE )
alcazares_con_nodo <- data.frame(agregarNodoDiaJornada(alcazares,"Casa de la Cultura Alzáceres","Miércoles","Tarde"), alcazares, stringsAsFactors=FALSE )
alcazares_electronica_con_nodo <-  data.frame(agregarNodoDiaJornada(alcazares_electronica,"Casa de la Cultura Alzáceres","Miércoles","Tarde"), alcazares_electronica, stringsAsFactors=FALSE )
casa_gardeliana_con_nodo <- data.frame(agregarNodoDiaJornada(casa_gardeliana,"Museo Casa Gardeliana","Lunes","Tarde"), casa_gardeliana, stringsAsFactors=FALSE )
casa_gardeliana_electronica_con_nodo <- data.frame(agregarNodoDiaJornada(casa_gardeliana_electronica,"Museo Casa Gardeliana","Lunes","Tarde"), casa_gardeliana_electronica, stringsAsFactors=FALSE )

todos_los_nodos <- rbind(uva_la_esperanza_con_nodo, belen_percusion_con_nodo, belen_electronica_con_nodo, la_ladera_con_nodo, casa_de_la_memoria_con_nodo, pedregal_2a5_con_nodo, pedregal_9a12, guayabal_con_nodo, nuestra_gente_con_nodo, uva_de_los_suenos_manana_con_nodo, uva_de_los_suenos_tarde_con_nodo, colores_tarde_con_nodo, colores_manana_con_nodo, colores_electronica_con_nodo, doce_de_octubre_tarde_con_nodo, doce_de_octubre_manana_con_nodo, alcazares_con_nodo, alcazares_electronica_con_nodo, casa_gardeliana_con_nodo, casa_gardeliana_electronica_con_nodo)

levels(todos_los_nodos$Nodo)[3]
length(todos_los_nodos$Nodo[todos_los_nodos$Nodo == levels(todos_los_nodos$Nodo)[3]])

todos_los_nodos$Sexo[todos_los_nodos$Sexo == levels(as.factor(todos_los_nodos$Sexo))[1]] <- "Sin dato"

todos_los_nodos$EDAD <- laply(todos_los_nodos$EDAD, as.character)

todos_los_nodos$EDAD[todos_los_nodos$EDAD == ""] <- "Sin dato"

todos_los_nodos$EDAD <- factor(todos_los_nodos$EDAD)

unique(sort(as.numeric(todos_los_nodos$EDAD)))

## http://www.r-bloggers.com/reorder-factor-levels-2/
orden <- data.frame(rownames(as.data.frame(as.numeric(levels(as.factor(todos_los_nodos$EDAD))))),as.data.frame(as.numeric(levels(as.factor(todos_los_nodos$EDAD)))))

colnames(orden) <- c("indice","factor.level")
orden$orddenado <- order(orden[,2])

edad.ordenada <- factor(todos_los_nodos$EDAD, levels(as.factor(todos_los_nodos$EDAD))[orden$orddenado])

## GRÁFICAS

## Inscritos por nodo según edad y sexo
p <- ggplot(todos_los_nodos, aes(edad.ordenada)) +  geom_freqpoly()
p + facet_wrap("Nodo") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(panel.grid.minor = element_blank())

p <- ggplot(todos_los_nodos[todos_los_nodos$Nodo == levels(todos_los_nodos$Nodo)[1],], aes(edad.ordenada, fill=Sexo)) + geom_bar(position = "dodge")
p #+ facet_wrap("Nodo")

p <- ggplot(todos_los_nodos, aes(edad.ordenada, fill=Día.de.la.semana)) +  geom_histogram(na.rm = FALSE) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + facet_wrap("Nodo")

## Evolución semanas
evolucion.semanas <- read.xlsx2("./Data/InscritosPorSemanaPorNodo.xlsx",1)
colnames(evolucion.semanas) <- c("Nodo","Semana 1 - Julio 15 al 19","Semana 2 - Julio 22 al 25","Semana 3 - Julio 28 a agosto 1","Semana 4 - Agosto 4 al 8","Semana 4  - Agosto 4 al 8")

evolucion.semanas <- evolucion.semanas[-6]

evolucion.semanas.melt <- melt(evolucion.semanas, id="Nodo")
p <- ggplot(evolucion.semanas.melt, aes(x = variable, y = as.numeric(as.character(value)), fill = Nodo)) + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(axis.text.y = element_text(size = 10) ) + scale_y_continuous("Inscritos") + scale_x_discrete("Semanas")
p + facet_wrap("Nodo")

## Cobertura
cobertura.nodos <- read.xlsx2("./Data/Cobertura.xlsx",1)
write.xlsx2(cobertura.nodos, "./Data/Cobertura.xlsx")

cobertura.nodos$diferencia <- abs(as.integer(as.character(cobertura.nodos[,7])) - as.integer(as.character(cobertura.nodos[,5])))

cobertura.nodos.melt <- melt(cobertura.nodos, id.vars = "Sede", measure.vars = c("Cobertura.actual","Diferencia"), variable.name = "Cobertura" , value.name = "Inscritos")

cobertura.nodos.melt2 <- melt(cobertura.nodos, id.vars = "Sede", measure.vars = c("Capacidad.instalada"), variable.name = "Cobertura" , value.name = "Inscritos")

cober <- cbind(cobertura.nodos.melt,cobertura.nodos.melt2)

p <- ggplot(cobertura.nodos.melt, aes(x = Sede, y = as.integer(as.character(value)) )) + geom_bar( aes(fill = variable), stat="identity") + scale_y_continuous("Ocupación actual") + scale_x_discrete(breaks = NULL) + guides(fill = guide_legend(reverse = TRUE))

g <- ggplot(cobertura.nodos.melt2, aes(x = Sede, y = as.integer(as.character(value)) )) + geom_bar( aes(fill = variable), stat="identity") + scale_y_continuous("Capacidad Instalada") + scale_x_discrete( breaks = NULL) + geom_text(aes(label = Sede), angle = 90, size = 4)

grid.arrange(p,g, ncol = 1)

## Inscritos por edad
p <- ggplot(todos_los_nodos, aes(x = EDAD)) + geom_histogram()
p## + scale_x_discrete(breaks=NULL) + scale_y_discrete(breaks=NULL)


## Registros para retirar:
p <- ggplot(casa_gardeliana_electronica_con_nodo, aes(x = Sexo)) + geom_freqpoly() + stat_bin(binwidth = 1)
p## + scale_x_discrete(breaks=NULL) + scale_y_discrete(breaks=NULL)

## X es el consecutivo de los inscritos
p <- ggplot(casa_gardeliana_electronica_con_nodo, aes(Edad, fill=Sexo)) + geom_bar()
p ##+ scale_x_discrete(breaks=NULL) ##+ scale_y_discrete(breaks=NULL)

p <- ggplot(inscritos, aes(x = Edad)) + geom_histogram() + stat_bin(binwidth = 1) + facet_wrap("Nodo")
p## + scale_x_discrete(breaks=NULL) + scale_y_discrete(breaks=NULL)

p <- ggplot(casa_gardeliana_electronica_con_nodo, aes(x = as.numeric(Edad))) + geom_histogram()
p## + scale_x_discrete(breaks=NULL) + scale_y_discrete(breaks=NULL)

p <- ggplot(casa_gardeliana_electronica_con_nodo, aes(x = as.numeric(Edad))) + geom_freqpoly()
p## + scale_x_discrete(breaks=NULL) + scale_y_discrete(breaks=NULL)


htmlTable(getDescriptionStatsBy(casa_gardeliana_electronica_con_nodo$Edad,casa_gardeliana_electronica_con_nodo$Sexo,html=TRUE,total_col_show_perc=TRUE))

## Exportando archivo con pestañas por nodo
InscritosDepurados <- read.csv2("./FusionBuzonesConWEB.csv")

nodos <- levels(InscritosDepurados$NODO_WEB)
cantidad_nodos <- length(levels(InscritosDepurados$NODO_WEB))

write.xlsx2(InscritosDepurados[InscritosDepurados$NODO_WEB == nodos[1],], "./InscritosNoFantasmas.xlsx","Alcazares")

guardarSheets <- function(d.f.guardar, nodo, str_sheet) {
    write.xlsx2(d.f.guardar[d.f.guardar$NODO_WEB == nodo,], "./InscritosNoFantasmas.xlsx", str_sheet, append = TRUE)
}

guardarSheets(InscritosDepurados, nodos[1], "Alcazares")
guardarSheets(InscritosDepurados, nodos[2], "CasaGardeliana")
guardarSheets(InscritosDepurados, nodos[3], "Moravia")
guardarSheets(InscritosDepurados, nodos[4], "LosColores")
guardarSheets(InscritosDepurados, nodos[5], "NuestraGente")
guardarSheets(InscritosDepurados, nodos[6], "CasaDeLaMemoria")
guardarSheets(InscritosDepurados, nodos[7], "12deOctubre")
guardarSheets(InscritosDepurados, nodos[8], "Belen")
guardarSheets(InscritosDepurados, nodos[9], "LaLadera")
guardarSheets(InscritosDepurados, nodos[10], "Guayabal")
guardarSheets(InscritosDepurados, nodos[11], "Pedregal")
guardarSheets(InscritosDepurados, nodos[12], "UVAdeLaEsperanza")
guardarSheets(InscritosDepurados, nodos[13], "UVAdeLosSuenos")
