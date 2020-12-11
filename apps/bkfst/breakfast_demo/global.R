
#library(plyr)
library(shiny)
library(dplyr)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggiraph)
library(ggthemes)
library(shinyWidgets)
library(htmlwidgets)
library(xlsx)
library(stringi)
library(stringr)
library(shinyjs)
library(DT)
library(rio)
library(readxl)
library(tibble)


source("Output_ParameterSettings.R")
source("Output_PortfolioAnalysis.R")
source("UI_graph.R")
#source("Output_ProductAnalysis.R")
source("Fonction_graph.R")
source("Fonction_calcul_apports.R")
source("Fonction_selection.R",encoding = "UTF-8")
source("Fonction_reco_table.R")


#++++++++++++++++++++++++
# Fonction helper pour ajouter des titres dans feuille Excel
#++++++++++++++++++++++++
# - sheet : la feuille Excel pour contenir le titre
# - rowIndex : numéro de la ligne pour contenir le titre 
# - title : texte du titre
# - titleStyle : l'objet style pour le titre
xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  xlsx::setCellValue(sheetTitle[[1,1]], title)
  xlsx::setCellStyle(sheetTitle[[1,1]], titleStyle)
}

erreur_col <- function(data,col,type){
  isolate({
    if (type=="cereals"){
    validate(
      need(!(FALSE%in%sapply(data%>%select(col),is.numeric)),
           as.character('Nutrients, volume and serving size values must be numeric (0,1,2,3,4,5,6,7,8,9,"." or ","). Please check relevant columns in cereals file.'))
           )
      }
      if (type=="reco"){
    validate(
        need(!(FALSE%in%sapply(data%>%select(col),is.numeric)),
             as.character('Recommendations values must be numeric (0,1,2,3,4,5,6,7,8,9,"." or ","). Please check relevant columns in recommendations file.'))
           )
      }
    if (type=="compo"){
      validate(
        need(!(FALSE%in%sapply(data%>%select(col),is.numeric)),
             as.character('Nutrients and serving size values must be numeric (0,1,2,3,4,5,6,7,8,9,"." or ","). 
                          Please check relevant columns in component(s) file(s)'))
      )
    }
  }) 
}