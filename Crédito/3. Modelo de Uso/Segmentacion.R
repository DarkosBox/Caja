#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC")
Loadpkg(pkgs)

BD$Colsubsidio<-as.factor(ifelse(BD$razonzocial=="COLSUBSIDIO", 1,0))

##### 1 Modelo Clasificación Colsubsidio/No Colsubsidio-----

names(BD)
names(BD_Clasificacion)
BD_Clasificacion<-BD %>% 
  select(fechaproceso,ciudad:Colsubsidio) %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360))
         ) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento) %>% 
  mutate(Colsubsidio=as.factor(Colsubsidio),
         ciudad=as.factor(ciudad),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         genero=as.factor(genero),
         id_ciiu=as.factor(id_ciiu),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         Cupo_VI=limitecupo/salario
         )

summary(BD_Clasificacion)
str(BD_Clasificacion)
vars_to_bin<-BD_Clasificacion %>% select(-c(Colsubsidio, actividad, segmpoblacion, genero)) %>% names()
bin<-woe.tree.binning(BD_Clasificacion, 'Colsubsidio', vars_to_bin, 0.05)
BD_Clasificacion<-woe.binning.deploy(BD_Clasificacion, bin) %>% select(-one_of(vars_to_bin))

str(BD_Clasificacion)
levels(BD_Clasificacion$zona)

levels(BD_Clasificacion$Colsubsidio) <- make.names(levels(factor(BD_Clasificacion$Colsubsidio)))

set.seed(31415)
idpart<- createDataPartition(BD_Clasificacion$Colsubsidio,p=0.7,list=F)
train<-BD_Clasificacion[idpart,]
test<-BD_Clasificacion[-idpart,]

str(train)
str(test)

set.seed(31415)
cctrl1 <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

## GLM
modelo_lr <- train(Colsubsidio ~ ., data = train,
                   method = "glm",
                   metric = "ROC",
                   trControl = cctrl1,
                   family=binomial())
summary(modelo_lr)
modelo_lr.Pred=predict(modelo_lr,test,type="prob")
modelo_lr.roc<-roc(test$Colsubsidio,modelo_lr.Pred[,2])
plot(modelo_lr.roc, print.thres = "best", col="darkorchid")
modelo_lr.auc=pROC::auc(modelo_lr.roc);modelo_lr.auc


=======
#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC")
Loadpkg(pkgs)

BD$Colsubsidio<-as.factor(ifelse(BD$razonzocial=="COLSUBSIDIO", 1,0))

##### 1 Modelo Clasificación Colsubsidio/No Colsubsidio-----

names(BD)
names(BD_Clasificacion)
BD_Clasificacion<-BD %>% 
  select(fechaproceso,ciudad:Colsubsidio) %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360))
         ) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento) %>% 
  mutate(Colsubsidio=as.factor(Colsubsidio),
         ciudad=as.factor(ciudad),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         genero=as.factor(genero),
         id_ciiu=as.factor(id_ciiu),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         Cupo_VI=limitecupo/salario
         )

summary(BD_Clasificacion)
str(BD_Clasificacion)
vars_to_bin<-BD_Clasificacion %>% select(-c(Colsubsidio, actividad, segmpoblacion, genero)) %>% names()
bin<-woe.tree.binning(BD_Clasificacion, 'Colsubsidio', vars_to_bin, 0.05)
BD_Clasificacion<-woe.binning.deploy(BD_Clasificacion, bin) %>% select(-one_of(vars_to_bin))

str(BD_Clasificacion)
levels(BD_Clasificacion$zona)

levels(BD_Clasificacion$Colsubsidio) <- make.names(levels(factor(BD_Clasificacion$Colsubsidio)))

set.seed(31415)
idpart<- createDataPartition(BD_Clasificacion$Colsubsidio,p=0.7,list=F)
train<-BD_Clasificacion[idpart,]
test<-BD_Clasificacion[-idpart,]

str(train)
str(test)

set.seed(31415)
cctrl1 <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

## GLM
modelo_lr <- train(Colsubsidio ~ ., data = train,
                   method = "glm",
                   metric = "ROC",
                   trControl = cctrl1,
                   family=binomial())
summary(modelo_lr)
modelo_lr.Pred=predict(modelo_lr,test,type="prob")
modelo_lr.roc<-roc(test$Colsubsidio,modelo_lr.Pred[,2])
plot(modelo_lr.roc, print.thres = "best", col="darkorchid")
modelo_lr.auc=pROC::auc(modelo_lr.roc);modelo_lr.auc
