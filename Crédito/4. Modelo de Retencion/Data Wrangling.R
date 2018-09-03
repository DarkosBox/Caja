<<<<<<< HEAD
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr")
Loadpkg(pkgs)

### Cargue de datos ---

fecha="2018-04-01"

data<-fread("./BD/BD_FIN.txt",na.strings = "") %>%
  select(-c(Cancelacion, Gestionable, Gestionable,Retencion, Valor.Credito, Observaciones, TIPO, ANO_MES))

data$Identificacion<-as.character(data$Identificacion)
data$Tarjeta<-as.character(data$Tarjeta)
data$intento=as.factor(data$intento)
data$Fecha.Expedicion<-as.Date(data$Fecha.Expedicion, "%d/%m/%Y")
data$Disponible.Avances<-as.numeric(gsub("\\s","",data$Disponible.Avances))
data$Limite.Avances<-as.numeric(gsub("\\s","",data$Limite.Avances))
data$Total.Intereses<-as.numeric(gsub("\\s","",data$Total.Intereses))
data$Saldos.Mes.Ant<-as.numeric(gsub("\\s","",data$Saldos.Mes.Ant))
data$Pagos.Mes.Ant<-as.numeric(gsub("\\s","",data$Pagos.Mes.Ant))
data$Vtas.Mes.Ant<-as.numeric(gsub("\\s","",data$Vtas.Mes.Ant))
data$Edad.Mora<-as.numeric(gsub("\\s","",data$Edad.Mora))
data$Limite.Cupo<-as.numeric(gsub("\\s","",data$Limite.Cupo))
data$Pago.del.Mes<-as.numeric(gsub("\\s","",data$Pago.del.Mes))
data$Pago.Minimo<-as.numeric(gsub("\\s","",data$Pago.Minimo))
data$Vr.Mora<-as.numeric(gsub("\\s","",data$Vr.Mora))
data$Vr.Cuota.Manejo<-as.numeric(gsub("\\s","",data$Vr.Cuota.Manejo))
data$Saldo<-as.numeric(gsub("\\s","",data$Saldo))
data$Fecha.Proceso=ifelse(is.na(data$Fecha.Proceso), fecha, data$Fecha.Proceso)
data$Fecha.Proceso<-as.Date(data$Fecha.Proceso)
data$dias_intencion=difftime(data$Fecha.Proceso,data$Fecha.Expedicion, units=c("days"))

names(data)<-Limpiar.Cadenas(names(data))
str(data)

## 
test <- data %>% 
  filter(is.na(dias_intencion)) 

### 
persona<-fread("./BD/Persona_.txt") %>% 
  mutate(IdPersona=substr(x = IdPersona, 3, nchar(IdPersona))) %>% 
  arrange(IdPersona) %>% 
  group_by(IdPersona) %>% 
  filter(row_number()==1)

BD <-data %>% left_join(persona, by=c("identificacion"="IdPersona"))
sum(is.na(BD$Categoria))

table(BD[!is.na(BD$Categoria),]$intento)

rm(data, persona)
saveRDS(data, "./BD/Resultados/data.rds")
fwrite(data, "./BD/Resultados/data.csv")
=======
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/Camilo Yate Tamara/Google Drive/R/Funciones.R")
pkgs<-c("data.table", "dplyr")
Loadpkg(pkgs)

### Cargue de datos ---

fecha="2018-04-01"

data<-fread("./BD/BD_FIN.txt",na.strings = "") %>%
  select(-c(Cancelacion, Gestionable, Gestionable,Retencion, Valor.Credito, Observaciones, TIPO, ANO_MES))

data$Identificacion<-as.character(data$Identificacion)
data$Tarjeta<-as.character(data$Tarjeta)
data$intento=as.factor(data$intento)
data$Fecha.Expedicion<-as.Date(data$Fecha.Expedicion, "%d/%m/%Y")
data$Disponible.Avances<-as.numeric(gsub("\\s","",data$Disponible.Avances))
data$Limite.Avances<-as.numeric(gsub("\\s","",data$Limite.Avances))
data$Total.Intereses<-as.numeric(gsub("\\s","",data$Total.Intereses))
data$Saldos.Mes.Ant<-as.numeric(gsub("\\s","",data$Saldos.Mes.Ant))
data$Pagos.Mes.Ant<-as.numeric(gsub("\\s","",data$Pagos.Mes.Ant))
data$Vtas.Mes.Ant<-as.numeric(gsub("\\s","",data$Vtas.Mes.Ant))
data$Edad.Mora<-as.numeric(gsub("\\s","",data$Edad.Mora))
data$Limite.Cupo<-as.numeric(gsub("\\s","",data$Limite.Cupo))
data$Pago.del.Mes<-as.numeric(gsub("\\s","",data$Pago.del.Mes))
data$Pago.Minimo<-as.numeric(gsub("\\s","",data$Pago.Minimo))
data$Vr.Mora<-as.numeric(gsub("\\s","",data$Vr.Mora))
data$Vr.Cuota.Manejo<-as.numeric(gsub("\\s","",data$Vr.Cuota.Manejo))
data$Saldo<-as.numeric(gsub("\\s","",data$Saldo))
data$Fecha.Proceso=ifelse(is.na(data$Fecha.Proceso), fecha, data$Fecha.Proceso)
data$Fecha.Proceso<-as.Date(data$Fecha.Proceso)
data$dias_intencion=difftime(data$Fecha.Proceso,data$Fecha.Expedicion, units=c("days"))

names(data)<-Limpiar.Cadenas(names(data))
str(data)

## 
test <- data %>% 
  filter(is.na(dias_intencion)) 

### 
persona<-fread("./BD/Persona_.txt") %>% 
  mutate(IdPersona=substr(x = IdPersona, 3, nchar(IdPersona))) %>% 
  arrange(IdPersona) %>% 
  group_by(IdPersona) %>% 
  filter(row_number()==1)

BD <-data %>% left_join(persona, by=c("identificacion"="IdPersona"))
sum(is.na(BD$Categoria))

table(BD[!is.na(BD$Categoria),]$intento)

rm(data, persona)
saveRDS(data, "./BD/Resultados/data.rds")
fwrite(data, "./BD/Resultados/data.csv")
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
