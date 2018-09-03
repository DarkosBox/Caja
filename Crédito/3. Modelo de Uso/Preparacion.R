rm(list=ls(all=TRUE))
#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "stringr")
Loadpkg(pkgs)

# Cargue de Datos ----

# Primera compra
prim<-fread("./BD/Originales/C-PrimeraCompra.txt", dec=",") %>% 
  select(-c(8,9,10)) %>% 
  mutate(`FECHA PROCESO`=as.Date(substr(`FECHA PROCESO`,1,
                                        (str_locate(pattern = " ",`FECHA PROCESO`)[,1])-1), "%d/%m/%Y"))
names(prim)<-Limpiar.Cadenas(names(prim), espacios = F)

# Informacion Afiliado
Afiliado<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>% 
  select(-12) %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y"))
names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)
names(Afiliado)

# Infomacion Crédito.
Cobranzas<-fread("./BD/Originales/cobranzas04.csv")[,c(1,2,4,15,47,81, 92)] %>% 
  mutate(`Fecha Expedición`=as.Date(`Fecha Expedición`,"%d/%m/%Y"),
         `Limite Cupo`=as.numeric(gsub("$","",gsub("\\s","",`Limite Cupo`))),
         `Limite Avances`=as.numeric(gsub("$","",gsub("\\s","",`Limite Avances`))),
         Id_Cliente=paste0(ifelse(`Tipo Identificación`==1,"CD",
                                   ifelse(`Tipo Identificación`==2,"CC",
                                          ifelse(`Tipo Identificación`==3,"CE","PA"))),`Nro Identificación`),
         Tarjeta=as.numeric(Tarjeta),
         Amparada=as.factor(ifelse(`Id Amparador`>0,1,0))
           ) %>% 
  select(-c(1,2),-Id_Cliente, `Id Amparador`) 
names(Cobranzas)<-Limpiar.Cadenas(names(Cobranzas), espacios = F)
str(Cobranzas)

# Informacion del Convenio.
Conv<-fread("./BD/Originales/NITS.csv", dec=",") %>% 
  mutate(NIT=as.numeric(NIT))
names(Conv)<-Limpiar.Cadenas(names(Conv), espacios = F)


# Union de Datos ----

BD<-prim %>% 
  left_join(Conv, by=c("nitestablecimiento"="nit")) %>% 
  left_join(Cobranzas, by=c("tarjeta"="tarjeta")) %>%
  left_join(Afiliado) %>% 
  select(-c(cx,cy, nivelsociores,nombreestablecimiento, estado)) %>% 
  na.omit()

BD$Colsubsidio<-as.factor(ifelse(BD$nitestablecimiento==8600073361, 1,0))

saveRDS(BD, "./BD/Depuradas/BD.rds")
rm(prim, Cobranzas, Conv, Afiliado)  

=======
rm(list=ls(all=TRUE))
#### Paquetes y funciones ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr", "stringr")
Loadpkg(pkgs)

# Cargue de Datos ----

# Primera compra
prim<-fread("./BD/Originales/C-PrimeraCompra.txt", dec=",") %>% 
  select(-c(8,9,10)) %>% 
  mutate(`FECHA PROCESO`=as.Date(substr(`FECHA PROCESO`,1,
                                        (str_locate(pattern = " ",`FECHA PROCESO`)[,1])-1), "%d/%m/%Y"))
names(prim)<-Limpiar.Cadenas(names(prim), espacios = F)

# Informacion Afiliado
Afiliado<-fread("./BD/Originales/Consulta1.txt", na.strings = ("NA"), dec=",") %>% 
  select(-12) %>% 
  mutate(FechaNacimiento=as.Date(substr(FechaNacimiento,1,
                                        (str_locate(pattern = " ",FechaNacimiento)[,1])-1), "%d/%m/%Y"))
names(Afiliado)<-Limpiar.Cadenas(names(Afiliado), espacios = F)
names(Afiliado)

# Infomacion Crédito.
Cobranzas<-fread("./BD/Originales/cobranzas04.csv")[,c(1,2,4,15,47,81, 92)] %>% 
  mutate(`Fecha Expedición`=as.Date(`Fecha Expedición`,"%d/%m/%Y"),
         `Limite Cupo`=as.numeric(gsub("$","",gsub("\\s","",`Limite Cupo`))),
         `Limite Avances`=as.numeric(gsub("$","",gsub("\\s","",`Limite Avances`))),
         Id_Cliente=paste0(ifelse(`Tipo Identificación`==1,"CD",
                                   ifelse(`Tipo Identificación`==2,"CC",
                                          ifelse(`Tipo Identificación`==3,"CE","PA"))),`Nro Identificación`),
         Tarjeta=as.numeric(Tarjeta),
         Amparada=as.factor(ifelse(`Id Amparador`>0,1,0))
           ) %>% 
  select(-c(1,2),-Id_Cliente, `Id Amparador`) 
names(Cobranzas)<-Limpiar.Cadenas(names(Cobranzas), espacios = F)
str(Cobranzas)

# Informacion del Convenio.
Conv<-fread("./BD/Originales/NITS.csv", dec=",") %>% 
  mutate(NIT=as.numeric(NIT))
names(Conv)<-Limpiar.Cadenas(names(Conv), espacios = F)


# Union de Datos ----

BD<-prim %>% 
  left_join(Conv, by=c("nitestablecimiento"="nit")) %>% 
  left_join(Cobranzas, by=c("tarjeta"="tarjeta")) %>%
  left_join(Afiliado) %>% 
  select(-c(cx,cy, nivelsociores,nombreestablecimiento, estado)) %>% 
  na.omit()

BD$Colsubsidio<-as.factor(ifelse(BD$nitestablecimiento==8600073361, 1,0))

saveRDS(BD, "./BD/Depuradas/BD.rds")
rm(prim, Cobranzas, Conv, Afiliado)  
