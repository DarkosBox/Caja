rm(list=ls(all=T))

### Funciones ----

Loadpkg <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

convertir.contabilidad <- function(x){
  if(grepl("\\(.*\\)", x)){
    as.numeric(paste0("-", gsub("\\(|\\)", "", gsub("[\\$, ]", "", x))))
  } else {
    as.numeric(gsub("[\\$, ]", "", x))
  }
}

Limpiar.Cadenas <-function(x, espacios=T){
  x<-gsub("\\.", "_",tolower(x))
  if(espacios){
    x<-gsub("\\W\\s","",iconv(x,to="ASCII//TRANSLIT"))
    } else {
    x<-gsub("\\W","",iconv(x,to="ASCII//TRANSLIT"))
    }
}

### Librerias ----

pkgs<-c("dplyr","stringr","readr", "data.table", "ggplot2", "plotly", "agricolae", "GGally", 
        "DT", "MASS", "caret", "glmnet", "randomForest", "e1071")
Loadpkg(pkgs)

### Cargar Datos -----

## Aportes y remanentes

Aportes <- fread("ConsultaAportantes.txt",sep="\t")

### Correccion de nombres de variables
names(Aportes)<-Limpiar.Cadenas(names(Aportes), F)

### Transformación de tipo de documento
Aportes$id_empresas<-as.numeric(gsub("\\D","",Aportes$id_empresas))

### Depuracion de formatos.

Aportes$aportes<-sapply(Aportes$aportes, convertir.contabilidad, USE.NAMES = F)
Aportes$remanetebruto<-sapply(Aportes$remanetebruto, convertir.contabilidad, USE.NAMES = F)
Aportes$remaneteneto<-sapply(Aportes$remaneteneto, convertir.contabilidad, USE.NAMES = F)

### Fecha de análisis

Aportes$Periodo=as.factor(Aportes$ano*100+Aportes$mes)
Aportes<- Aportes %>% dplyr::select(-c(mes, ano))

levels(Aportes$Periodo)
unique(as.numeric(as.character(Aportes$Periodo)))

## base de Datos EMIS

Emis_Afiliados <- fread("Afiliados.csv",sep=";", na.strings = c("","n/a"), dec=",")
names(Emis_Afiliados)<-Limpiar.Cadenas(names(Emis_Afiliados), F)
Emis_Afiliados$idsfiscalesnacionales<-as.character(Emis_Afiliados$idsfiscalesnacionales)

Emis_NoAfiliados <- fread("NoAfiliados.csv",sep=";", na.strings = c("","n/a"), dec=",")
names(Emis_NoAfiliados)<-names(Emis_Afiliados)

Emis_Afiliados$Source<-"Afiliados"
Emis_NoAfiliados$Source<-"No Afiliados"

EMIS<-rbind(Emis_Afiliados, Emis_NoAfiliados)
rm(Emis_Afiliados, Emis_NoAfiliados)

EMIS$idsfiscalesnacionales<-as.numeric(gsub("\\D","",EMIS$idsfiscalesnacionales))

EMIS <- EMIS %>% 
  dplyr::select(-c(num, ciudad, direccion, telefono, fax, correoelectronico, paginaweb,
            tipodedireccion, principalesejecutivos, descripciondelnegocioproductos,
            auditoresfinancieros, listadanolistada, isin, accionistas, productosprincipales,
            fechadereferenciadelosempleados, fechadereferenciadelprecio, fechadeincorporacion,
            fuente, emisid, anofiscal, auditado, estadodeafiliacion))

str(EMIS)
### Análisis Preliminar ====

## Base de datos de aportes

BD1<- Aportes %>% filter(aportes>=0) %>% 
  group_by(id_empresas,razonsocial, piramide1, piramide2, actividad, descripcionciiu) %>% 
  summarise(trabajadores=max(trabajadores, na.rm = T),
            Aportes=mean(aportes, na.rm = T),
            Remanente=mean(remaneteneto, na.rm = T)) %>% 
  mutate(trabajadores=ifelse(is.infinite(trabajadores), NA,trabajadores)) %>% 
  filter(actividad!="")
BD1$actividad<-as.factor(BD1$actividad)

# ggpairs(data = BD1[,-c(1,2,6)])
# boxplot(log(BD1$Aportes))

# Aporte:

# Trasformación no normalidad.
# boxcox(lm(Aportes~actividad+trabajadores,data=BD1),lambda=seq(0,1,by=.1))
# Lambda=0 --> transformacion logarítmica

## Prueba HSD de tukey
# Realizar pruebas de diferencias honestas pareadas para determinar posibles 
# agrupaciones a partir de la actividad de la empresa

# m1<-lm(data = BD1, log(Aportes)~actividad)
# t1<-HSD.test(m1,"actividad")
#  datatable(t1$groups, options = list(dom = 't')) %>%
#    formatCurrency(c('log(Aportes)'), digits = 2, currency = "")
# 
#  BD1$actividad<-factor(BD1$actividad, levels=rev(row.names(t1$groups)))
# 
#  ggplot(data = BD1, aes(x=actividad, y=log(Aportes)))+
#    geom_boxplot()+
#    scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
#    theme(axis.text.x = element_text(angle = 90, vjust = 0))+
#    coord_flip()



# Recodificacioón de variable actividad según los resulatdos de la prueba HSD

BD1$ActividadAportes<-as.factor(
  ifelse(BD1$actividad %in% 
           c("Tansporte y Almacenamiento", "Agricultura, Caza, Silvicultura y pesca"), 
         "Tansporte ,Almacenamiento, Agricultura, Caza, Silvicultura y pesca", as.character(BD1$actividad))
)

# m1<-lm(data = BD1, log(Aportes)~ActividadAportes)
# t1<-HSD.test(m1,"ActividadAportes")
# datatable(t1$groups, options = list(dom = 't')) %>% 
#   formatCurrency(c('log(Aportes)'), digits = 2, currency = "")

# BD1$ActividadAportes<-factor(BD1$ActividadAportes, levels=rev(row.names(t1$groups)))
# 
# ggplot(data = BD1, aes(x=ActividadAportes, y=log(Aportes)))+
#   geom_boxplot()+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0))+
#   coord_flip()

## Remanente Neto

# Variable no tranformable por existencia de valores negativos de remanente. 

# m1<-lm(data = BD1, Remanente~actividad)
# plot(m1)
# t1<-HSD.test(m1,"actividad")
# datatable(t1$groups, options = list(dom = 't')) %>% 
#   formatCurrency(c('Remanente'), digits = 2, currency = "")
# 
# BD1$actividad<-factor(BD1$actividad, levels=rev(row.names(t1$groups)))
# 
# ggplot(data = BD1, aes(x=actividad, y=Remanente))+
#   geom_boxplot()+
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 25))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0))+
#   coord_flip()

## Base de datos EMIS

Miss<-EMIS %>%  summarise_all(funs(sum(is.na(.))*100/n()))

EMIS<-EMIS %>% dplyr::select(1,2,3,4,5,6,7,8,9,16,27,42,46,70,71) # Variables con menos del 20% de ausentes

#Imputacion por la media
EMIS<-EMIS %>% group_by(industria, numerodeempleados) %>% 
  mutate_if(is.numeric,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))

### Unión ====

BD<- BD1 %>% inner_join(EMIS, by=c("id_empresas"="idsfiscalesnacionales"))

boxplot(BD$Aportes)
summary(BD$Aportes)
# boxcox(lm(Aportes~actividad+trabajadores,data=BD),lambda=seq(0,1,by=.1))

Data<-BD %>% ungroup() %>% 
  dplyr::select(id_empresas,Aportes, Remanente, actividad,descripcionciiu, trabajadores,
                   estadoprovinciaregion,formalegal,activostotales,activoscorrientes,pasivostotales,
                   ingresosnetosporventas,rotaciondeactivos,apalancamiento) %>% 
  mutate(estadoprovinciaregion=ifelse(estadoprovinciaregion=="Bogotá D.C","Bogotá D.C","Otros"),
         formalegal=ifelse(formalegal=="Persona Natural","Persona Natural","Persona Juridica"),
         LogAportes=log(Aportes))

Data$activostotales<-as.numeric(gsub(",","\\.",Data$activostotales))
Data$activoscorrientes<-as.numeric(gsub(",","\\.",Data$activoscorrientes))
Data$pasivostotales<-as.numeric(gsub(",","\\.",Data$pasivostotales))
Data$ingresosnetosporventas<-as.numeric(gsub(",","\\.",Data$ingresosnetosporventas))
Data$rotaciondeactivos<-as.numeric(gsub(",","\\.",Data$rotaciondeactivos))
Data$apalancamiento<-as.numeric(gsub(",","\\.",Data$apalancamiento))

# Data<-Data %>% group_by(actividad) %>% 
#   mutate_if(is.numeric,funs(ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
#   mutate_if(is.character,funs(ifelse(is.na(.), "Ausente", .)))

Miss<-Data %>% ungroup() %>% summarise_all(funs(sum(is.na(.))*100/n()))

rm(Aportes, BD, BD1, check, Miss)

# Fusion de niveles de variables categóricas.

# Actividad. 
m1<-lm(data = Data, Aportes~actividad)
t1<-HSD.test(m1,"actividad")
table(t1$groups[,2])

T_Actividad<-t1$groups %>% 
  mutate(actividad=row.names(t1$groups)) %>% 
  dplyr::select(actividad, groups) %>% 
  rename(GrupoActividad=groups)

# descripcionciiu. 
m1<-lm(data = Data, Aportes~descripcionciiu)
t1<-HSD.test(m1,"descripcionciiu")
table(t1$groups[,2])

T_descripcionciiu<-t1$groups %>% 
  mutate(descripcionciiu=row.names(t1$groups)) %>% 
  dplyr::select(descripcionciiu, groups) %>% 
  rename(Grupodescripcionciiu=groups)

Data<-Data %>% 
  ungroup %>% 
  left_join(T_Actividad) %>% 
  left_join(T_descripcionciiu) %>% 
  dplyr::select(-actividad, -descripcionciiu)

rm(m1, t1)

### Exportación -----

fwrite(Data, "Depuradas/Aportes.csv")
fwrite(T_Actividad, "Depuradas/Actividad.csv")
fwrite(T_descripcionciiu, "Depuradas/CIIU.csv")

names(Data)



table(Data$estadoprovinciaregion)

      