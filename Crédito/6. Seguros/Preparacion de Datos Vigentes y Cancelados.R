<<<<<<< HEAD
rm(list=ls(all=T))

source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
pkgs <- c("tidyverse","data.table", "zoo", "leaflet")
Loadpkg(pkgs)


## Cargar Datos ---

#Seguros
miss<-c("", "NA")
tipos<-c(rep("character",2),"numeric", rep("character",9))
data<- fread("./BD/Depuradas/VigentesCancelados.csv", select = c(1:10), na.strings = miss, colClasses = tipos)
names(data)<-Limpiar.Cadenas(names(data))

## Depuración
mes<-as.Date("2018-08-31")
mes_ini<-as.Date("2015-12-31")

data<-data %>%
  mutate(id_persona=paste0(tipodeindentificacion, numero),
         fechafincobertura=as.Date(fechafincobertura, tryFormats =   c("%Y%m%d","%d/%m/%Y")),
         fechainiciocobertura=as.Date(fechainiciocobertura, tryFormats =   c("%Y%m%d","%d/%m/%Y"))
         ) %>%
  mutate_if(is.character, funs(toupper(.))) %>% 
  select(-tipodeindentificacion)

#Persona
persona<-fread("./BD/Depuradas/ConsolidadaPersona.csv", select=c(2:12)) %>% 
  arrange(id_persona, piramide2) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1)

length(unique(persona$id_persona))==dim(persona)[1] ## Debe ser TRUE

# Union
bd<-data %>% left_join(persona, by=c("id_persona"="id_persona"))





fwrite(bd, "./BD/Depuradas/Unida.csv")

rm(persona, data)

## Construccion Cosechas.
mes<-as.Date("2018-08-31")
mes_ini<-as.Date("2015-12-31")

bd2<-bd %>%
  filter(!is.na(fechainiciocobertura), fechainiciocobertura >= as.Date("2017-01-01")) %>% 
  mutate(
         fechafincobertura=as.Date(ifelse(is.na(fechafincobertura), mes, fechafincobertura)),
         fechainiciocobertura=as.Date(ifelse(fechainiciocobertura<=mes_ini,mes_ini,fechainiciocobertura)),
         MOB=(as.yearmon(fechafincobertura)-as.yearmon(fechainiciocobertura))*12,
         Periodo_INI=year(fechainiciocobertura)*100+month(fechainiciocobertura),
         Periodo_INI=ifelse(Periodo_INI<201600, "<2016", as.character(Periodo_INI)),
         Periodo_FIN=as.character(year(fechafincobertura)*100+month(fechafincobertura)),
         Periodo_FIN=ifelse(Periodo_FIN<201600, "<2016", as.character(Periodo_FIN))
         ) %>% 
  filter(MOB>=0)
  
mobs<-bd2 %>% 
  select(id_persona, producto, Periodo_INI, MOB) %>% 
  unique()

bd3<-expand.grid(id_persona=mobs$id_persona, n_mobs=0:max(mobs$MOB)) %>% 
  left_join(mobs, by=c("id_persona"="id_persona")) %>% 
  filter(n_mobs<=MOB) %>% 
  mutate(cont=ifelse(n_mobs==0,1,-1)) %>% 
  unique()

fwrite(bd3, "./BD/Depuradas/Cosecha.csv")
# Conversión a formato Cosecha


=======
rm(list=ls(all=T))

source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.r")
pkgs <- c("tidyverse","data.table", "zoo", "leaflet")
Loadpkg(pkgs)


## Cargar Datos ---

#Seguros
miss<-c("", "NA")
tipos<-c(rep("character",2),"numeric", rep("character",9))
data<- fread("./BD/Depuradas/VigentesCancelados.csv", select = c(1:10), na.strings = miss, colClasses = tipos)
names(data)<-Limpiar.Cadenas(names(data))

## Depuración
mes<-as.Date("2018-08-31")
mes_ini<-as.Date("2015-12-31")

data<-data %>%
  mutate(id_persona=paste0(tipodeindentificacion, numero),
         fechafincobertura=as.Date(fechafincobertura, tryFormats =   c("%Y%m%d","%d/%m/%Y")),
         fechainiciocobertura=as.Date(fechainiciocobertura, tryFormats =   c("%Y%m%d","%d/%m/%Y"))
         ) %>%
  mutate_if(is.character, funs(toupper(.))) %>% 
  select(-tipodeindentificacion)

#Persona
persona<-fread("./BD/Depuradas/ConsolidadaPersona.csv", select=c(2:12)) %>% 
  arrange(id_persona, piramide2) %>% 
  group_by(id_persona) %>% 
  filter(row_number()==1)

length(unique(persona$id_persona))==dim(persona)[1] ## Debe ser TRUE

# Union
bd<-data %>% left_join(persona, by=c("id_persona"="id_persona"))





fwrite(bd, "./BD/Depuradas/Unida.csv")

rm(persona, data)

## Construccion Cosechas.
mes<-as.Date("2018-08-31")
mes_ini<-as.Date("2015-12-31")

bd2<-bd %>%
  filter(!is.na(fechainiciocobertura), fechainiciocobertura >= as.Date("2017-01-01")) %>% 
  mutate(
         fechafincobertura=as.Date(ifelse(is.na(fechafincobertura), mes, fechafincobertura)),
         fechainiciocobertura=as.Date(ifelse(fechainiciocobertura<=mes_ini,mes_ini,fechainiciocobertura)),
         MOB=(as.yearmon(fechafincobertura)-as.yearmon(fechainiciocobertura))*12,
         Periodo_INI=year(fechainiciocobertura)*100+month(fechainiciocobertura),
         Periodo_INI=ifelse(Periodo_INI<201600, "<2016", as.character(Periodo_INI)),
         Periodo_FIN=as.character(year(fechafincobertura)*100+month(fechafincobertura)),
         Periodo_FIN=ifelse(Periodo_FIN<201600, "<2016", as.character(Periodo_FIN))
         ) %>% 
  filter(MOB>=0)
  
mobs<-bd2 %>% 
  select(id_persona, producto, Periodo_INI, MOB) %>% 
  unique()

bd3<-expand.grid(id_persona=mobs$id_persona, n_mobs=0:max(mobs$MOB)) %>% 
  left_join(mobs, by=c("id_persona"="id_persona")) %>% 
  filter(n_mobs<=MOB) %>% 
  mutate(cont=ifelse(n_mobs==0,1,-1)) %>% 
  unique()

fwrite(bd3, "./BD/Depuradas/Cosecha.csv")
# Conversión a formato Cosecha


>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
