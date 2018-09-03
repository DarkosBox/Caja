if (!("BD" %in% as.character(ls())))
    BD <- readRDS("./Data/BD.rds")

Merge_All <- function(x, y){
    df <- merge(x, y, by= "Score", all=TRUE)
    return(df)
}

### Carga e instalacion de paquetes ====

library("dplyr")
library("shiny") 
library("ggplot2")
library("Hmisc") 
library("scales") 
library("plotly") 
library("DT") 
library("factoextra") 
library("tidyr") 
library("data.table")