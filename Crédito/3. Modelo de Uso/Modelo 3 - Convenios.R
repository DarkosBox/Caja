<<<<<<< HEAD
rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC", "randomForest")
Loadpkg(pkgs)

#### Cargue y particion de Datos ----
names(DB)
DB<-readRDS("./BD/Depuradas/BD.rds") %>% filter(Colsubsidio==0, !is.na(categoria)) %>% 
  select(categoria,fechaproceso, ciudad:zona) %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento, -idamparador, -zona, -ciudad, -limiteavances, -id_ciiu) %>% 
  mutate(categoria=as.factor(categoria),
         genero=as.factor(genero),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         Cupo_VI=limitecupo/salario) %>% 
  group_by(segmpoblacion) %>% 
  mutate(limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
         EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
         Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),limitecupo)
  )

map_dbl(DB, .f = function(x){sum(is.na(x))})

str(DB)
levels(DB$categoria) <- make.names(levels(factor(DB$categoria)))

set.seed(31415)
idpart<- createDataPartition(DB$categoria,p=0.7,list=F)
train<-DB[idpart,]
test<-DB[-idpart,]

names(train)
### Multinomial ----

cctrl1 <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
logmodel <- train(categoria ~ ., data = train, 
                  method = "multinom", 
                  maxit=10, 
                  tuneLength=1, 
                  trControl = cctrl1)
logmodel$finalModel

saveRDS(logmodel, "./Resultados/Modelos/Convenios.rds")
=======
rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC", "randomForest")
Loadpkg(pkgs)

#### Cargue y particion de Datos ----
names(DB)
DB<-readRDS("./BD/Depuradas/BD.rds") %>% filter(Colsubsidio==0, !is.na(categoria)) %>% 
  select(categoria,fechaproceso, ciudad:zona) %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento, -idamparador, -zona, -ciudad, -limiteavances, -id_ciiu) %>% 
  mutate(categoria=as.factor(categoria),
         genero=as.factor(genero),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         Cupo_VI=limitecupo/salario) %>% 
  group_by(segmpoblacion) %>% 
  mutate(limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
         EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
         Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),limitecupo)
  )

map_dbl(DB, .f = function(x){sum(is.na(x))})

str(DB)
levels(DB$categoria) <- make.names(levels(factor(DB$categoria)))

set.seed(31415)
idpart<- createDataPartition(DB$categoria,p=0.7,list=F)
train<-DB[idpart,]
test<-DB[-idpart,]

names(train)
### Multinomial ----

cctrl1 <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
logmodel <- train(categoria ~ ., data = train, 
                  method = "multinom", 
                  maxit=10, 
                  tuneLength=1, 
                  trControl = cctrl1)
logmodel$finalModel

saveRDS(logmodel, "./Resultados/Modelos/Convenios.rds")
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
