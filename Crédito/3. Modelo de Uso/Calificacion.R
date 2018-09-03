rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "stringr", "woeBinning", "tidyverse", "mice")
Loadpkg(pkgs)

#### cargue de datos ----

### Afiliados

Afiliado<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>% 
  filter(toupper(IdPersona_Emp_Seg_Empresarial)=="X") %>% 
  select(-12, -Cx, -Cy, -NivelSocioRes, -IdPersona_Emp_Seg_Empresarial) %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y")) 
names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)
names(Afiliado)

### Calificar

fecha=as.Date("2018-04-30") #### Cambiar

Seg<-fread("./BD/Depuradas/Calificar2.csv") %>%  select(Titular, SegmentoTitular=Segmento) %>% 
  filter(SegmentoTitular!="Sin segmento")

Calif<-fread("./BD/Depuradas/Calificar2.csv") %>%
  left_join(Seg) %>% 
  mutate(Id_Cliente=paste0(`Tipo Identificación`, `Nro Identificación`),
         fechaexpedicion=as.Date(`Fecha Expedición`,"%d/%m/%Y"),
         amparada=ifelse(`Nro Identificación`!=Titular,1,0),
         fechaproceso=fecha) %>% 
  select(Id_Cliente, `Limite Cupo`, `Ciudad`, GENERO_T=GENERO, amparada, fechaproceso, 
         fechaexpedicion, SegmentoTitular) %>% 
  left_join(Afiliado, by=c("Id_Cliente"="idpersona")) 
  # group_by(Id_Cliente) %>% 
  # arrange(Id_Cliente, desc(zona)) %>% 
  # filter(row_number()==1)
names(Calif)<-Limpiar.Cadenas(names(Calif), espacios = F)

Calif<-Calif %>% 
  ungroup() %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))
         ) %>%   
  select(-fechaexpedicion, -fechanacimiento, -fechaproceso, -segmentotitular, genero) %>%
  mutate(ciudad=as.factor(ifelse(grepl("BOGOTA",ciudad), "BOGOTA","OTROS")),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         genero=as.factor(genero_t),
         id_ciiu=as.factor(id_ciiu),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(amparada),
         Cupo_VI=limitecupo/salario,
         limitecupo=as.numeric(limitecupo)
  ) %>% 
  select(-genero_t)


names(Calif)
map_dbl(Calif, .f = function(x){sum(is.na(x))})
str(Calif)
#### Calificacion de Base ----

# modelo Colsubsidio / No Colsubsidio

# Aplicacion del Binning

# sort(names(BD_Clasificacion))
# sort(names(Calif))
str(Calif)

bin<-readRDS("./Resultados/Modelos/bin.rds")
Calif_bin<-woe.binning.deploy(Calif, bin)


# Modelo

modelo1<-readRDS("./Resultados/Modelos/Clasificacion.rds")
thresh <-readRDS("./Resultados/Modelos/best_thresh.rds")
Calif_bin$p_pred <- predict(modelo1,Calif_bin,type="prob")[,2]
Calif_bin$Colsubsidio <- ifelse(Calif_bin$p_pred>thresh,1,0)

# Modelo Colsubsidio
modelo_2<-read_rds("./Resultados/Modelos/Colsubsidio.rds")
modelo_3<-read_rds("./Resultados/Modelos/Convenios.rds")

  Calif_Col<-Calif_bin %>% 
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
str(Calif_Col)

test<-predict(modelo_2,Calif_Col,type="prob") 
test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
Calif_Col$UES <- test$UES

test<-predict(modelo_3,Calif_Col,type="prob") 
test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
Calif_Col$Categoria <- test$Categoria


PrimCompra <- Calif_Col %>% 
  mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>% 
  select(IdPersona=id_cliente, PrimeraCompra) %>% 
  group_by(IdPersona) %>% 
  arrange(IdPersona) %>% 
  filter(row_number()==1)

prop.table(table(PrimCompra$PrimeraCompra))

fwrite(PrimCompra, "./Resultados/Bases de Datos/BaseCalificada2.csv")
=======
rm(list=ls(all=TRUE))

#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "stringr", "woeBinning", "tidyverse", "mice")
Loadpkg(pkgs)

#### cargue de datos ----

### Afiliados

Afiliado<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>% 
  filter(toupper(IdPersona_Emp_Seg_Empresarial)=="X") %>% 
  select(-12, -Cx, -Cy, -NivelSocioRes, -IdPersona_Emp_Seg_Empresarial) %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y")) 
names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)
names(Afiliado)

### Calificar

fecha=as.Date("2018-04-30") #### Cambiar

Seg<-fread("./BD/Depuradas/Calificar2.csv") %>%  select(Titular, SegmentoTitular=Segmento) %>% 
  filter(SegmentoTitular!="Sin segmento")

Calif<-fread("./BD/Depuradas/Calificar2.csv") %>%
  left_join(Seg) %>% 
  mutate(Id_Cliente=paste0(`Tipo Identificación`, `Nro Identificación`),
         fechaexpedicion=as.Date(`Fecha Expedición`,"%d/%m/%Y"),
         amparada=ifelse(`Nro Identificación`!=Titular,1,0),
         fechaproceso=fecha) %>% 
  select(Id_Cliente, `Limite Cupo`, `Ciudad`, GENERO_T=GENERO, amparada, fechaproceso, 
         fechaexpedicion, SegmentoTitular) %>% 
  left_join(Afiliado, by=c("Id_Cliente"="idpersona")) 
  # group_by(Id_Cliente) %>% 
  # arrange(Id_Cliente, desc(zona)) %>% 
  # filter(row_number()==1)
names(Calif)<-Limpiar.Cadenas(names(Calif), espacios = F)

Calif<-Calif %>% 
  ungroup() %>% 
  mutate(Edad=floor(as.numeric(difftime(fechaproceso,fechanacimiento,units = "days")/360)),
         EdadPlastico=as.numeric(difftime(fechaproceso,fechaexpedicion,units = "days"))
         ) %>%   
  select(-fechaexpedicion, -fechanacimiento, -fechaproceso, -segmentotitular, genero) %>%
  mutate(ciudad=as.factor(ifelse(grepl("BOGOTA",ciudad), "BOGOTA","OTROS")),
         zona=as.factor(ifelse(zona=="",NA,zona)),
         genero=as.factor(genero_t),
         id_ciiu=as.factor(id_ciiu),
         actividad=as.factor(actividad),
         segmpoblacion=as.factor(segmpoblacion),
         amparada=as.factor(amparada),
         Cupo_VI=limitecupo/salario,
         limitecupo=as.numeric(limitecupo)
  ) %>% 
  select(-genero_t)


names(Calif)
map_dbl(Calif, .f = function(x){sum(is.na(x))})
str(Calif)
#### Calificacion de Base ----

# modelo Colsubsidio / No Colsubsidio

# Aplicacion del Binning

# sort(names(BD_Clasificacion))
# sort(names(Calif))
str(Calif)

bin<-readRDS("./Resultados/Modelos/bin.rds")
Calif_bin<-woe.binning.deploy(Calif, bin)


# Modelo

modelo1<-readRDS("./Resultados/Modelos/Clasificacion.rds")
thresh <-readRDS("./Resultados/Modelos/best_thresh.rds")
Calif_bin$p_pred <- predict(modelo1,Calif_bin,type="prob")[,2]
Calif_bin$Colsubsidio <- ifelse(Calif_bin$p_pred>thresh,1,0)

# Modelo Colsubsidio
modelo_2<-read_rds("./Resultados/Modelos/Colsubsidio.rds")
modelo_3<-read_rds("./Resultados/Modelos/Convenios.rds")

  Calif_Col<-Calif_bin %>% 
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
str(Calif_Col)

test<-predict(modelo_2,Calif_Col,type="prob") 
test$Cat_2<-colnames(test[1:5])[apply(test[1:5],1,which.max)]
test$UES<-ifelse(test$SUPERMERCADOS<0.8, test$Cat_2, "SUPERMERCADOS")
Calif_Col$UES <- test$UES

test<-predict(modelo_3,Calif_Col,type="prob") 
test$Ues_2<-colnames(test[1:17])[apply(test[1:17],1,which.max)]
test$Categoria<-ifelse(test$VESTUARIO<0.1, test$Ues_2, "VESTUARIO")
Calif_Col$Categoria <- test$Categoria


PrimCompra <- Calif_Col %>% 
  mutate(PrimeraCompra=as.factor(ifelse(Colsubsidio==1, as.character(UES), as.character(Categoria)))) %>% 
  select(IdPersona=id_cliente, PrimeraCompra) %>% 
  group_by(IdPersona) %>% 
  arrange(IdPersona) %>% 
  filter(row_number()==1)

prop.table(table(PrimCompra$PrimeraCompra))

fwrite(PrimCompra, "./Resultados/Bases de Datos/BaseCalificada2.csv")
