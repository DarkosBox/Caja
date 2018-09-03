<<<<<<< HEAD
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.R")
pkgs<-c("data.table", "dplyr")
Loadpkg(pkgs)

### Cargue de Datos ---

calif<-fread("./BD/Calificar/_20180701.csv", dec=",")[,c(1,2,4,47,19,93,92,55,46,45,43,18,15,14,13,12,10,7,31)]
names(calif)<-Limpiar.Cadenas(names(calif), espacios = F)

id_<-data.frame(tipoidentificacion=c(1,2,3,4),
                t_ID=c("","CC","CE","TI"))

estados<-fread("./BD/Auxiliar/EstadosTarjeta.csv")

calif<-calif %>% 
  left_join(id_) %>%
  left_join(estados) %>% 
  filter(ESTADOCUPO=="ACTIVO") %>% 
  mutate(IdPersona=paste0(t_ID, nroidentificacion))%>% 
  select(-c(tipoidentificacion, t_ID, nroidentificacion, ESTADOCUPO, DESCRIPCION, estadotarjeta)) %>% 
  select(IdPersona, 1:16)

names(calif)<-c("identificacion","tarjeta","fecha_expedicion","fecha_proceso","disponible_avances",
                "limite_avances","total_intereses","saldos_mes_ant","pagos_mes_ant","vtas_mes_ant","edad_mora",
                "limite_cupo","pago_del_mes","pago_minimo","vr_mora","vr_cuota_manejo","saldo")

calif$identificacion<-as.character(calif$identificacion)
calif$tarjeta<-as.character(calif$tarjeta)
calif$fecha_expedicion<-as.Date(calif$fecha_expedicion, "%d/%m/%Y")
calif$fecha_proceso<-as.Date(calif$fecha_proceso, "%d/%m/%Y")
calif$disponible_avances<-as.numeric(gsub(",","",calif$disponible_avances))
calif$limite_avances<-as.numeric(gsub(",","",calif$limite_avances))
calif$total_intereses<-as.numeric(gsub(",","",calif$total_intereses))
calif$saldos_mes_ant<-as.numeric(gsub(",","",calif$saldos_mes_ant))
calif$pagos_mes_ant<-as.numeric(gsub(",","",calif$pagos_mes_ant))
calif$vtas_mes_ant<-as.numeric(gsub(",","",calif$vtas_mes_ant))
calif$edad_mora<-as.numeric(gsub(",","",calif$edad_mora))
calif$limite_cupo<-as.numeric(gsub(",","",calif$limite_cupo))
calif$pago_del_mes<-as.numeric(gsub(",","",calif$pago_del_mes))
calif$pago_minimo<-as.numeric(gsub(",","",calif$pago_minimo))
calif$vr_mora<-as.numeric(gsub(",","",calif$vr_mora))
calif$vr_cuota_manejo<-as.numeric(gsub(",","",calif$vr_cuota_manejo))
calif$saldo<-as.numeric(gsub(",","",calif$saldo))

calif$dias_intencion=difftime(calif$fecha_proceso,calif$fecha_expedicion, units=c("days"))

fwrite(calif, "./BD/Calificar/Calificar.csv")
=======
rm(list=ls(all=T))

###  Librerias ----
source("C:/Users/hernyatt/Documents/CamiloYate/Funciones.R")
pkgs<-c("data.table", "dplyr")
Loadpkg(pkgs)

### Cargue de Datos ---

calif<-fread("./BD/Calificar/_20180701.csv", dec=",")[,c(1,2,4,47,19,93,92,55,46,45,43,18,15,14,13,12,10,7,31)]
names(calif)<-Limpiar.Cadenas(names(calif), espacios = F)

id_<-data.frame(tipoidentificacion=c(1,2,3,4),
                t_ID=c("","CC","CE","TI"))

estados<-fread("./BD/Auxiliar/EstadosTarjeta.csv")

calif<-calif %>% 
  left_join(id_) %>%
  left_join(estados) %>% 
  filter(ESTADOCUPO=="ACTIVO") %>% 
  mutate(IdPersona=paste0(t_ID, nroidentificacion))%>% 
  select(-c(tipoidentificacion, t_ID, nroidentificacion, ESTADOCUPO, DESCRIPCION, estadotarjeta)) %>% 
  select(IdPersona, 1:16)

names(calif)<-c("identificacion","tarjeta","fecha_expedicion","fecha_proceso","disponible_avances",
                "limite_avances","total_intereses","saldos_mes_ant","pagos_mes_ant","vtas_mes_ant","edad_mora",
                "limite_cupo","pago_del_mes","pago_minimo","vr_mora","vr_cuota_manejo","saldo")

calif$identificacion<-as.character(calif$identificacion)
calif$tarjeta<-as.character(calif$tarjeta)
calif$fecha_expedicion<-as.Date(calif$fecha_expedicion, "%d/%m/%Y")
calif$fecha_proceso<-as.Date(calif$fecha_proceso, "%d/%m/%Y")
calif$disponible_avances<-as.numeric(gsub(",","",calif$disponible_avances))
calif$limite_avances<-as.numeric(gsub(",","",calif$limite_avances))
calif$total_intereses<-as.numeric(gsub(",","",calif$total_intereses))
calif$saldos_mes_ant<-as.numeric(gsub(",","",calif$saldos_mes_ant))
calif$pagos_mes_ant<-as.numeric(gsub(",","",calif$pagos_mes_ant))
calif$vtas_mes_ant<-as.numeric(gsub(",","",calif$vtas_mes_ant))
calif$edad_mora<-as.numeric(gsub(",","",calif$edad_mora))
calif$limite_cupo<-as.numeric(gsub(",","",calif$limite_cupo))
calif$pago_del_mes<-as.numeric(gsub(",","",calif$pago_del_mes))
calif$pago_minimo<-as.numeric(gsub(",","",calif$pago_minimo))
calif$vr_mora<-as.numeric(gsub(",","",calif$vr_mora))
calif$vr_cuota_manejo<-as.numeric(gsub(",","",calif$vr_cuota_manejo))
calif$saldo<-as.numeric(gsub(",","",calif$saldo))

calif$dias_intencion=difftime(calif$fecha_proceso,calif$fecha_expedicion, units=c("days"))

fwrite(calif, "./BD/Calificar/Calificar.csv")
>>>>>>> 5308310d9fd680f7f63a4eff58a3d19ee6c67bee
