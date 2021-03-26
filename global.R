## Load packages
library("tidyr")
library("httr")
library("lubridate")
library("knitr")
library("shiny")
library("shinyjs")
library("shinyBS")
library("plotly")
library("shinythemes")
library("shinycssloaders")
library("markdown")
library("flexdashboard")
library("tidyverse")
library("ggthemes")
library("treemap")
library("leaflet")
library("readxl")
library("readODS")
library("htmlTable")
library("stringr")
library("esquisse")
library("gridExtra")
library("ggpubr")
library("RColorBrewer")
library("data.table")
options(DT.options = list(scrollY="300px",scrollX="300px", 
                          pageLength = 100, 
                          columnDefs = list(list(className = 'dt-center', targets = "_all"))))
library("shinydashboard")
library("shinyWidgets") # nicer inputs
##############################################################################

## Define font to be used later
f1 = list(family = "Arial", size = 10, color = "rgb(30,30,30)")

## Function to format the dates for better plotting
printDate = function(date){
  # paste0(day(date),"/",month(date, lab=T, locale="us"))
  monthsEn=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  paste0(day(date),"/",monthsEn[month(date)])
}

## colors for observed data
blu = 'rgb(100,140,240)'
dblu = 'rgb(0,0,102)'
red = 'rgb(200,30,30)'
dred = 'rgb(100,30,30)'

##############################################################################
## DATA SOURCES
dados <- readRDS("LeidenRanking.Rds")
#pop <- data.table::fread("Populacao.csv")
#names(pop) <- c("UF_EXERCICIO", "POPULACAO", "REGIAO")
