<<<<<<< HEAD
#### Cargue de Paquetes ----

IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = TRUE)
}
pkgs<-c("data.table", "dplyr", "DT", "lubridate", "woeBinning")
Loadpkg(pkgs)

# library("data.table")
# library("dplyr")
# library("DT")
# library("lubridate")
# library("woeBinning")

#### Cargue de Modelos ----
bin<-readRDS("./bin.rds")
modelo1<-readRDS("./Clasificacion.rds")
thresh <-readRDS("./best_thresh.rds")
modelo_2<-readRDS("./Colsubsidio.rds")
=======
#### Cargue de Paquetes ----

IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = TRUE)
}
pkgs<-c("data.table", "dplyr", "DT", "lubridate", "woeBinning")
Loadpkg(pkgs)

# library("data.table")
# library("dplyr")
# library("DT")
# library("lubridate")
# library("woeBinning")

#### Cargue de Modelos ----
bin<-readRDS("./bin.rds")
modelo1<-readRDS("./Clasificacion.rds")
thresh <-readRDS("./best_thresh.rds")
modelo_2<-readRDS("./Colsubsidio.rds")
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
modelo_3<-readRDS("./Convenios.rds")