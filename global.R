################################################
# Proyecto: MarcaPatrones                      #  
#           Herramiento para manualmente       #
#           marcar eventos de incubacion       #
#           con series de tiempo               #
#           de temperatura con ruido de        #
#           HOBO dataloggers                   #
# Autores: Justin Baldwin,                     #
#       Mario Agustin Loaiza Muñoz,            #
#       Gustavo Adolfo Londoño                 #
# Fechas: Diciembre 2017-Enero 2018            #
################################################



library(devtools)
library(dplyr)
library(ggplot2)
#install_github("mdjbru-R-packages/thermPerf")
library(lubridate)
library(scales)
library(shiny)
library(thermPerf)
library(tidyr)

Sys.setenv(TZ='GMT')

#cambia este vara
#setwd("C:\\Users\\Justin\\Desktop\\Fulbright Colombia 2017\\temperature viewer app\\tempViewer")

##################
# esencial
##################
# meter los datos crudos en un archivo tipo .csv que usa ";" como separadores
# en una carpeta que se llama "hobo"
#>MarcaPatrones/hobo/archivo_1.csv
# Ademas, asegurate que el argumento sep en server.R en la linea del read.csv() esta adecuado
# Ademas, todo fallara si no hay columnas que se llaman "nido", "huevo" y "amb"
##

archivos <- list.files("hobo")[grep(list.files("hobo"), pattern = ".csv")]

palette <- c("yellow", "blue", "green", "red")

events<-c("Off"  ,"On"   )


empty_with_both<-data.frame("fecha"  =character(),
                            "hora"   =character(),
                            "huevo"  =numeric(),
                            "nido"   =numeric(),
                            "amb"   =numeric(),
                            "evento" =character(),
                            "X"      =character(),
                            "ts"=as.POSIXct(character()), 
                            "patron"=character(),
                            "event_number"=numeric(), 
                            "alpha"=numeric(),
                            "T_s"=numeric(),
                            
                            stringsAsFactors = F)

