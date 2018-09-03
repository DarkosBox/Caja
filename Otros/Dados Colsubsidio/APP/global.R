if (!("db" %in% as.character(ls())))
  db <- readRDS("./Data/bd.rds")

if (!("puntos" %in% as.character(ls())))
  puntos <- read.csv("./Data/Puntos.csv")

### Carga e instalacion de paquetes ====

library("dplyr")
library("leaflet.extras")
library("geosphere") 
library("DT")
library("shinydashboard")