rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "stringr", "woeBinning", "tidyverse")
Loadpkg(pkgs)

### Estructuración de Base entregada ----

# Base de cupo sin uso
file="./BD/Calificaciones/_201707.csv"
miss<-c("NA", "", "-",  "SIN SEGMENTO")

data<-fread(file, na.strings = miss) %>% 
  filter(`DESC BLOQUEO`=="NORMAL") %>% 
  select( # Selección de las variables útlies en el modelo
    `TIPO IDENTIFICACIÓN`
    ,`NRO IDENTIFICACIÓN`
    ,`NRO IDENTIFICACION TITULAR`
    ,`LIMITE CUPO`
    ,`CIUDAD`
    ,`FECHA EXPEDICIÓN`
    ,`GENERO`
    ,`SEGMENTO POBLACIONAL`
  ) %>% 
  mutate(
    IdCliente=paste0(`TIPO IDENTIFICACIÓN`,`NRO IDENTIFICACIÓN`), 
    Amparada=as.factor(ifelse(`NRO IDENTIFICACION TITULAR`==`NRO IDENTIFICACIÓN`, 2, 1)), 
    FechaApertura=as.Date(as.character(`FECHA EXPEDICIÓN`), "%d/%m/%Y"),
    Ciudad=as.factor(ifelse(grepl("BOGOTA",CIUDAD), "BOGOTA","OTROS")), 
    Segmento=as.factor(ifelse(toupper(`SEGMENTO POBLACIONAL`)=="ALTO","Alto",
                        ifelse(toupper(`SEGMENTO POBLACIONAL`) %in% c("BÁSICO","BASICO"),"Básico",
                         ifelse(toupper(`SEGMENTO POBLACIONAL`)=="JOVEN","Joven",
                          ifelse(toupper(`SEGMENTO POBLACIONAL`)=="MEDIO","Medio",NA))))),
    Genero=as.factor(ifelse(GENERO %in% c("F","MUJER"),"F",
                      ifelse(GENERO %in% c("M","HOMBRE"),"M",NA)))
  ) %>% 
  select(IdCliente, LimiteCupo=`LIMITE CUPO`, FechaApertura, Amparada, Segmento, Ciudad, Genero) 
str(data)

# Base de Clientes -

### La variable Zona fue discontinuada por los BDA, es necesario reestimar los betas.

zona<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>%
  select(IdPersona, ZONA)

Afiliado<-fread("BD/Originales/modelo.txt", na.strings = ("NA"),dec=",") %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y"),
         ACTIVIDAD=toupper(ACTIVIDAD),
         ACTIVIDAD=as.factor(
           ifelse(ACTIVIDAD %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                  ifelse(ACTIVIDAD %in% c("INDUSTRIA"),"Industria",
                         ifelse(ACTIVIDAD %in% c("COMERCIO AL POR MAYOR Y AL POR MENOR"),"Comercio al por Mayor y al por Menor",
                                ifelse(ACTIVIDAD %in% c("OTROS"),"Otros",
                                       ifelse(ACTIVIDAD %in% c("CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."),"Construccion, demoliciones, terrenos, Vias.",
                                              ifelse(ACTIVIDAD %in% c("PUBLICO"),"Publico",
                                                     ifelse(ACTIVIDAD %in% c("TANSPORTE Y ALMACENAMIENTO", "TRANSPORTE Y ALMACENAMIENTO"),"Tansporte y Almacenamiento",
                                                            ifelse(ACTIVIDAD %in% c("TEMPORALES"),"Temporales",
                                                                   ifelse(ACTIVIDAD %in% c("AGRICULTURA, CAZA, SILVICULTURA Y PESCA"),"Agricultura, Caza, Silvicultura y pesca",
                                                                          ifelse(ACTIVIDAD %in% c("ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS"),"Electricidad, Gas, Agua y explotacion de minas",NA)))))))))))) %>% 
  left_join(zona, by=c("id_persona"="IdPersona")) %>% 
  arrange(id_persona) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  select(-estrato)

names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)

# Consolidacion ADB
fecha=as.Date("2018-06-30") #### Cambiar

Base_Modelo<- data %>% left_join(Afiliado, by=c("IdCliente"="id_persona")) %>% 
  mutate(Edad=as.numeric(floor(difftime(fecha,fechanacimiento, units = 'days')/360)),
         Edad_Plastico=as.numeric(difftime(fecha,FechaApertura, units = 'days')),
         Cupo_VI=LimiteCupo/salario,
         zona=as.factor(ifelse(toupper(zona)=="MUNICIPIOS",1,
                          ifelse(toupper(zona)=="ZONA CENTRO",2,
                            ifelse(toupper(zona)=="ZONA CHAPINERO",3,
                              ifelse(toupper(zona)=="ZONA NORTE",4,
                                ifelse(toupper(zona)=="ZONA SUR",5,NA))))))) %>% 
  mutate_at(c("salario", "LimiteCupo"), funs(as.numeric(.))) %>% 
  mutate_at(c("id_ciiu", "actividad", "zona"), funs(as.factor(as.character(.)))) %>% 
  select(IdCliente, LimiteCupo, Ciudad, Amparada, Segmento, salario, Genero, id_ciiu, actividad, zona,
        Edad, Edad_Plastico, Cupo_VI)
names(Base_Modelo)<-c("id_cliente","limitecupo","ciudad","amparada","segmpoblacion","salario","genero","id_ciiu",
                  "actividad","zona","Edad","EdadPlastico","Cupo_VI")
    
str(Base_Modelo)

rm(Afiliado, data, zona)

### Cargue de Modelos ----

bin<-readRDS("Resultados/Modelos/bin.rds")
modelo1<-readRDS("Resultados/Modelos/Clasificacion.rds")
thresh <-readRDS("Resultados/Modelos/best_thresh.rds" )
modelo_2<-readRDS("Resultados/Modelos/Colsubsidio.rds")
modelo_3<-readRDS("Resultados/Modelos/Convenios.rds")

#### Calificacion de la base ----
step1<-woe.binning.deploy(Base_Modelo, bin)
step1$p_pred <- predict(modelo1,step1,type="prob")[,2]
step1$Colsubsidio <- ifelse(step1$p_pred>thresh,1,0)

step2<-step1 %>%
  select(id_cliente:Cupo_VI, Colsubsidio, -id_ciiu, -ciudad) %>%
  mutate(amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
         segmpoblacion=as.factor(ifelse(is.na(segmpoblacion),"Básico",as.character(segmpoblacion))),
         genero=as.factor(ifelse(is.na(genero),"M",as.character(genero))),
         actividad=as.factor(ifelse(is.na(actividad),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",as.character(actividad))),
         salario=ifelse(is.na(salario),mean(salario, na.rm=T),salario),
         limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
         Edad=ifelse(is.na( Edad),mean( Edad, na.rm=T), Edad),
         EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
         Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),Cupo_VI)
  )

test<-predict(modelo_2,step2,type="prob")
test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
step2$UES <- test$UES

test<-predict(modelo_3,step2,type="prob")
test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
step2$Categoria <- test$Categoria

Calificada <- step2 %>%
  mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
  select(IdPersona=id_cliente, Cupo=limitecupo, UES, Categoria, PrimeraCompra) %>%
  group_by(IdPersona) %>%
  arrange(IdPersona) %>%
  filter(row_number()==1)

fwrite(Calificada, "BD/Calificaciones/Calificada_17Jun18.csv", sep=";")

rm(list=ls(all=T))

=======
rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "stringr", "woeBinning", "tidyverse")
Loadpkg(pkgs)

### Estructuración de Base entregada ----

# Base de cupo sin uso
file="./BD/Calificaciones/_201707.csv"
miss<-c("NA", "", "-",  "SIN SEGMENTO")

data<-fread(file, na.strings = miss) %>% 
  filter(`DESC BLOQUEO`=="NORMAL") %>% 
  select( # Selección de las variables útlies en el modelo
    `TIPO IDENTIFICACIÓN`
    ,`NRO IDENTIFICACIÓN`
    ,`NRO IDENTIFICACION TITULAR`
    ,`LIMITE CUPO`
    ,`CIUDAD`
    ,`FECHA EXPEDICIÓN`
    ,`GENERO`
    ,`SEGMENTO POBLACIONAL`
  ) %>% 
  mutate(
    IdCliente=paste0(`TIPO IDENTIFICACIÓN`,`NRO IDENTIFICACIÓN`), 
    Amparada=as.factor(ifelse(`NRO IDENTIFICACION TITULAR`==`NRO IDENTIFICACIÓN`, 2, 1)), 
    FechaApertura=as.Date(as.character(`FECHA EXPEDICIÓN`), "%d/%m/%Y"),
    Ciudad=as.factor(ifelse(grepl("BOGOTA",CIUDAD), "BOGOTA","OTROS")), 
    Segmento=as.factor(ifelse(toupper(`SEGMENTO POBLACIONAL`)=="ALTO","Alto",
                        ifelse(toupper(`SEGMENTO POBLACIONAL`) %in% c("BÁSICO","BASICO"),"Básico",
                         ifelse(toupper(`SEGMENTO POBLACIONAL`)=="JOVEN","Joven",
                          ifelse(toupper(`SEGMENTO POBLACIONAL`)=="MEDIO","Medio",NA))))),
    Genero=as.factor(ifelse(GENERO %in% c("F","MUJER"),"F",
                      ifelse(GENERO %in% c("M","HOMBRE"),"M",NA)))
  ) %>% 
  select(IdCliente, LimiteCupo=`LIMITE CUPO`, FechaApertura, Amparada, Segmento, Ciudad, Genero) 
str(data)

# Base de Clientes -

### La variable Zona fue discontinuada por los BDA, es necesario reestimar los betas.

zona<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>%
  select(IdPersona, ZONA)

Afiliado<-fread("BD/Originales/modelo.txt", na.strings = ("NA"),dec=",") %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y"),
         ACTIVIDAD=toupper(ACTIVIDAD),
         ACTIVIDAD=as.factor(
           ifelse(ACTIVIDAD %in% c("PRESTACION DE SERVICIOS (EDUCACION, BANCA, COMUNICACIONES, SEGUROS, SALUD, ARTE Y CULTURA)"),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",
                  ifelse(ACTIVIDAD %in% c("INDUSTRIA"),"Industria",
                         ifelse(ACTIVIDAD %in% c("COMERCIO AL POR MAYOR Y AL POR MENOR"),"Comercio al por Mayor y al por Menor",
                                ifelse(ACTIVIDAD %in% c("OTROS"),"Otros",
                                       ifelse(ACTIVIDAD %in% c("CONSTRUCCION, DEMOLICIONES, TERRENOS, VIAS."),"Construccion, demoliciones, terrenos, Vias.",
                                              ifelse(ACTIVIDAD %in% c("PUBLICO"),"Publico",
                                                     ifelse(ACTIVIDAD %in% c("TANSPORTE Y ALMACENAMIENTO", "TRANSPORTE Y ALMACENAMIENTO"),"Tansporte y Almacenamiento",
                                                            ifelse(ACTIVIDAD %in% c("TEMPORALES"),"Temporales",
                                                                   ifelse(ACTIVIDAD %in% c("AGRICULTURA, CAZA, SILVICULTURA Y PESCA"),"Agricultura, Caza, Silvicultura y pesca",
                                                                          ifelse(ACTIVIDAD %in% c("ELECTRICIDAD, GAS, AGUA Y EXPLOTACION DE MINAS"),"Electricidad, Gas, Agua y explotacion de minas",NA)))))))))))) %>% 
  left_join(zona, by=c("id_persona"="IdPersona")) %>% 
  arrange(id_persona) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1) %>% 
  select(-estrato)

names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)

# Consolidacion ADB
fecha=as.Date("2018-06-30") #### Cambiar

Base_Modelo<- data %>% left_join(Afiliado, by=c("IdCliente"="id_persona")) %>% 
  mutate(Edad=as.numeric(floor(difftime(fecha,fechanacimiento, units = 'days')/360)),
         Edad_Plastico=as.numeric(difftime(fecha,FechaApertura, units = 'days')),
         Cupo_VI=LimiteCupo/salario,
         zona=as.factor(ifelse(toupper(zona)=="MUNICIPIOS",1,
                          ifelse(toupper(zona)=="ZONA CENTRO",2,
                            ifelse(toupper(zona)=="ZONA CHAPINERO",3,
                              ifelse(toupper(zona)=="ZONA NORTE",4,
                                ifelse(toupper(zona)=="ZONA SUR",5,NA))))))) %>% 
  mutate_at(c("salario", "LimiteCupo"), funs(as.numeric(.))) %>% 
  mutate_at(c("id_ciiu", "actividad", "zona"), funs(as.factor(as.character(.)))) %>% 
  select(IdCliente, LimiteCupo, Ciudad, Amparada, Segmento, salario, Genero, id_ciiu, actividad, zona,
        Edad, Edad_Plastico, Cupo_VI)
names(Base_Modelo)<-c("id_cliente","limitecupo","ciudad","amparada","segmpoblacion","salario","genero","id_ciiu",
                  "actividad","zona","Edad","EdadPlastico","Cupo_VI")
    
str(Base_Modelo)

rm(Afiliado, data, zona)

### Cargue de Modelos ----

bin<-readRDS("Resultados/Modelos/bin.rds")
modelo1<-readRDS("Resultados/Modelos/Clasificacion.rds")
thresh <-readRDS("Resultados/Modelos/best_thresh.rds" )
modelo_2<-readRDS("Resultados/Modelos/Colsubsidio.rds")
modelo_3<-readRDS("Resultados/Modelos/Convenios.rds")

#### Calificacion de la base ----
step1<-woe.binning.deploy(Base_Modelo, bin)
step1$p_pred <- predict(modelo1,step1,type="prob")[,2]
step1$Colsubsidio <- ifelse(step1$p_pred>thresh,1,0)

step2<-step1 %>%
  select(id_cliente:Cupo_VI, Colsubsidio, -id_ciiu, -ciudad) %>%
  mutate(amparada=as.factor(ifelse(is.na(amparada),1,as.numeric(amparada))),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         zona=as.factor(ifelse(is.na(zona),"Ausente",zona)),
         segmpoblacion=as.factor(ifelse(is.na(segmpoblacion),"Básico",as.character(segmpoblacion))),
         genero=as.factor(ifelse(is.na(genero),"M",as.character(genero))),
         actividad=as.factor(ifelse(is.na(actividad),"Prestacion de Servicios (Educacion, Banca, Comunicaciones, Seguros, Salud, Arte y Cultura)",as.character(actividad))),
         salario=ifelse(is.na(salario),mean(salario, na.rm=T),salario),
         limitecupo=ifelse(is.na(limitecupo),mean(limitecupo, na.rm=T),limitecupo),
         Edad=ifelse(is.na( Edad),mean( Edad, na.rm=T), Edad),
         EdadPlastico=ifelse(is.na(EdadPlastico),mean(EdadPlastico, na.rm=T),EdadPlastico),
         Cupo_VI=ifelse(is.na(Cupo_VI),mean(Cupo_VI, na.rm=T),Cupo_VI)
  )

test<-predict(modelo_2,step2,type="prob")
test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
step2$UES <- test$UES

test<-predict(modelo_3,step2,type="prob")
test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
step2$Categoria <- test$Categoria

Calificada <- step2 %>%
  mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>%
  select(IdPersona=id_cliente, Cupo=limitecupo, UES, Categoria, PrimeraCompra) %>%
  group_by(IdPersona) %>%
  arrange(IdPersona) %>%
  filter(row_number()==1)

fwrite(Calificada, "BD/Calificaciones/Calificada_17Jun18.csv", sep=";")

rm(list=ls(all=T))
