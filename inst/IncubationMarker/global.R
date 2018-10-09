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



list.of.packages <- c("changepoint", "devtools", "dplyr","dygraphs",  "ggplot2", "lubridate", "scales", "shiny", "shinydashboard", "shinyjs", "tidyr","V8", "xts", "zoo")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!require(thermPerf)){
  install_github("mdjbru-R-packages/thermPerf")
}



library(changepoint)
library(devtools)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(lubridate)
library(scales)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(thermPerf)
library(tidyr)
library(xts)
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

archivos <- list.files("hobo")[grep(list.files("hobo"), pattern = ".txt")]

palette <- c("yellow", "blue", "green", "red")

events<-c("Off"  ,"On"   )

models_labels<-c("Linear", "Polynomial-2 Grado", "Polynomial-3 Grado", "Newton")
models<-c("linearFit", "O2_poly", "O3_poly", "newton")

#####
# features to add
####

#reset collection/mapped events after save data
jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

#4. record threshold used
#useful?

on_text<-HTML("Seleccionaras un On-Bout")
off_text<-HTML("Seleccionaras un Off-Bout")


#############
# fitting and splitting function
###########

#source
#fit_each_bout


########################
#
#
