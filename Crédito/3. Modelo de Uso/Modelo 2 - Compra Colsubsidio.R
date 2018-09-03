rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC", "randomForest", "mice", "tidyverse", "tree", "DMwR")
Loadpkg(pkgs)

#### Cargue y particion de Datos ----
DB<-readRDS("./BD/Depuradas/BD.rds") %>% filter(Colsubsidio==1) %>% 
  select(ues,fechaproceso, ciudad:zona) %>%
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento, -idamparador, -id_ciiu, -ciudad,-limiteavances) %>%
  mutate(ues=as.factor(ues),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
         genero=as.factor(genero),
         # actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         Cupo_VI=limitecupo/salario)

levels(DB$zona)


set.seed(31415)
idpart<- createDataPartition(DB$ues,p=0.7,list=F)
train<-DB[idpart,]
test<-DB[-idpart,]
cctrl1 <- trainControl(method = "cv", number = 3, savePredictions = TRUE)
### Multinomial ----

logmodel <- train(ues ~ . , data = train,
                  method = "multinom", 
                  maxit=10, 
                  tuneLength=1, 
                  trControl = cctrl1)

table(predict(logmodel, test), test$ues)
saveRDS(logmodel$finalModel, "./Resultados/Modelos/Colsubsidio.rds")

### KNN ----
# set.seed(1905)
# v_k <- data.frame(k = c(10,50,100, 200))
# knn <- train(ues ~ ., data = train,
#              method = "knn",
#              tuneGrid = v_k,
#              trControl = cctrl1)
# plot(knn)
# confusionMatrix(knn)

# ### Random Forest Ranger ----
# set.seed(1905)
# param <- expand.grid(mtry = seq(1,11, by=1), #Cambiar
#                      min.node.size = seq(1,3, by=0.5), #Cambiar
#                      splitrule = "gini")
# 
# modelo_rf <- train(ues ~ ., data = train,
#                    method = "ranger",
#                    tuneGrid = param,
#                    trControl = cctrl1,
#                    num.trees = 100)
# plot(modelo_rf)
# confusionMatrix(modelo_rf)
# 
# 
=======
rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("dplyr","cluster", "factoextra", "woeBinning","caret", "randomForest",
        "pROC", "randomForest", "mice", "tidyverse", "tree", "DMwR")
Loadpkg(pkgs)

#### Cargue y particion de Datos ----
DB<-readRDS("./BD/Depuradas/BD.rds") %>% filter(Colsubsidio==1) %>% 
  select(ues,fechaproceso, ciudad:zona) %>%
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))) %>% 
  select(-fechaexpedicion,-fechaproceso, -fechanacimiento, -idamparador, -id_ciiu, -ciudad,-limiteavances) %>%
  mutate(ues=as.factor(ues),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
         genero=as.factor(genero),
         # actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         Cupo_VI=limitecupo/salario)

levels(DB$zona)


set.seed(31415)
idpart<- createDataPartition(DB$ues,p=0.7,list=F)
train<-DB[idpart,]
test<-DB[-idpart,]
cctrl1 <- trainControl(method = "cv", number = 3, savePredictions = TRUE)
### Multinomial ----

logmodel <- train(ues ~ . , data = train,
                  method = "multinom", 
                  maxit=10, 
                  tuneLength=1, 
                  trControl = cctrl1)

table(predict(logmodel, test), test$ues)
saveRDS(logmodel$finalModel, "./Resultados/Modelos/Colsubsidio.rds")

### KNN ----
# set.seed(1905)
# v_k <- data.frame(k = c(10,50,100, 200))
# knn <- train(ues ~ ., data = train,
#              method = "knn",
#              tuneGrid = v_k,
#              trControl = cctrl1)
# plot(knn)
# confusionMatrix(knn)

# ### Random Forest Ranger ----
# set.seed(1905)
# param <- expand.grid(mtry = seq(1,11, by=1), #Cambiar
#                      min.node.size = seq(1,3, by=0.5), #Cambiar
#                      splitrule = "gini")
# 
# modelo_rf <- train(ues ~ ., data = train,
#                    method = "ranger",
#                    tuneGrid = param,
#                    trControl = cctrl1,
#                    num.trees = 100)
# plot(modelo_rf)
# confusionMatrix(modelo_rf)
#
