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


library(changepoint)
library(devtools)
library(dplyr)
library(ggplot2)
#install_github("mdjbru-R-packages/thermPerf")
library(lubridate)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(thermPerf)
library(tidyr)
library(zoo)

Sys.setenv(TZ='GMT')

#cambia este vara
#setwd("C:\\Users\\Justin\\Desktop\\Fulbright Colombia 2017\\temperature viewer app\\tempViewer")

##################
# esencial
##################
# en una carpeta que se llama "hobo"
#>MarcaPatrones/hobo/archivo_1.csv
# Ademas, todo fallara si no hay columnas que se llaman "nido", "huevo" y "amb" // "fecha", "hora"
##

archivos <- list.files("hobo")[grep(list.files("hobo"), pattern = ".csv")]

palette <- c("yellow", "blue", "green", "red")

events<-c("Off"  ,"On"   )



#####
# features to add
####
#1. add your day 0 of development
#should work
#reset collection/mapped events after save data
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

#3. proportional threshold
#yes
#4. record threshold used
#useful?

#6. works for all combinations of data present?
#should work


####################
# remove linear fit, replace with mean estimate so its always worse than the cooling newton
# lines 33-41
# for nido sensors, cooling newton is not maybe the best
#####################